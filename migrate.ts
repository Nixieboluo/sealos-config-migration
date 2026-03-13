#!/usr/bin/env bun

import { mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { spawnSync } from 'node:child_process';
import Ajv2020 from 'ajv/dist/2020';
import { parseDocument } from 'yaml';

type ProviderMode = 'env' | 'configmap';
type ValueType = 'string' | 'number' | 'boolean' | 'array' | 'object' | 'enum';
type LogLevel = 'INFO' | 'WARN' | 'ERROR' | 'SKIP';

interface CliOptions {
	context?: string;
	apps?: string[];
	outDir?: string;
	providersConfig: string;
	assetsDir: string;
}

interface MappingItem {
	sourceKey: string;
	targetPath: string;
	type: ValueType;
	enumValues?: readonly string[];
	note?: string;
}

interface ProviderConfig {
	app: string;
	namespace: string;
	mode: ProviderMode;
	deployment?: string;
	container?: string;
	configmap?: string;
	dataKey?: string;
	templatePath: string;
	schemaPath: string;
	mappings: readonly MappingItem[];
}

interface AppSummary {
	app: string;
	namespace: string;
	status: 'ok' | 'skipped';
	reason?: string;
	missingCount: number;
	invalidCount: number;
	outputFile?: string;
}

const workspaceRoot = resolve(process.cwd());
const defaultProvidersConfig = join(
	workspaceRoot,
	'scripts',
	'export-provider-config.providers.json',
);
const defaultAssetsDir = join(workspaceRoot, 'scripts', 'export-provider-config-assets');
const ignoredEnvKeys = new Set([
	'NODE_ENV',
	'KUBERNETES_SERVICE_HOST',
	'KUBERNETES_SERVICE_PORT',
	'NEXT_PUBLIC_MOCK_USER',
	'NEXT_RUNTIME',
]);

function loadProviders(configPath: string): ProviderConfig[] {
	const parsed = JSON.parse(readFileSync(configPath, 'utf-8'));
	if (!Array.isArray(parsed)) {
		throw new Error(`Invalid providers config: expected JSON array in ${configPath}`);
	}
	return parsed as ProviderConfig[];
}

function log(level: LogLevel, message: string): void {
	console.log(`[${level}] ${message}`);
}

function parseCli(): CliOptions {
	const args = process.argv.slice(2);
	const options: CliOptions = {
		providersConfig: defaultProvidersConfig,
		assetsDir: defaultAssetsDir,
	};

	for (const arg of args) {
		if (!arg.startsWith('--')) {
			continue;
		}
		const [rawKey, rawVal] = arg.slice(2).split('=');
		const key = rawKey.trim();
		const value = (rawVal ?? '').trim();
		if (key === 'context' && value) {
			options.context = value;
		} else if (key === 'apps' && value) {
			options.apps = value
				.split(',')
				.map((item) => item.trim())
				.filter(Boolean);
		} else if (key === 'out-dir' && value) {
			options.outDir = resolve(workspaceRoot, value);
		} else if (key === 'providers-config' && value) {
			options.providersConfig = resolve(workspaceRoot, value);
		} else if (key === 'assets-dir' && value) {
			options.assetsDir = resolve(workspaceRoot, value);
		} else if (key === 'source' || key === 'raw-dir') {
			throw new Error(`Unsupported option: --${key} (live mode only)`);
		}
	}
	return options;
}

function runCmd(args: readonly string[]): { ok: boolean; stdout: string; stderr: string } {
	const result = spawnSync(args[0], args.slice(1), { encoding: 'utf-8' });
	return {
		ok: result.status === 0,
		stdout: result.stdout ?? '',
		stderr: result.stderr ?? '',
	};
}

function getCurrentK8sContext(): string {
	const result = runCmd(['kubectl', 'config', 'current-context']);
	if (!result.ok) {
		throw new Error(`Failed to get kubectl context: ${result.stderr || 'unknown error'}`);
	}
	return result.stdout.trim();
}

function getNestedValue(source: unknown, dottedPath: string): unknown {
	if (!source || typeof source !== 'object') {
		return undefined;
	}
	const keys = dottedPath.split('.');
	let cursor: any = source;
	for (const key of keys) {
		if (cursor == null || typeof cursor !== 'object' || !(key in cursor)) {
			return undefined;
		}
		cursor = cursor[key];
	}
	return cursor;
}

function applyCommentsByAst(doc: any, comments: Map<string, string[]>): void {
	const findPairByPath = (keys: string[]): any | undefined => {
		let current = doc.contents;
		let pair: any | undefined;
		for (const key of keys) {
			if (!current?.items || !Array.isArray(current.items)) {
				return undefined;
			}
			pair = current.items.find((item: any) => {
				const keyVal = item?.key?.value ?? item?.key;
				return String(keyVal) === key;
			});
			if (!pair) {
				return undefined;
			}
			current = pair.value;
		}
		return pair;
	};

	for (const [targetPath, lines] of comments.entries()) {
		const keys = targetPath.split('.');
		const pair = findPairByPath(keys);
		if (!pair) {
			continue;
		}
		const normalized = lines.map((line) => ` ${line}`).join('\n');
		if (pair.key) {
			pair.key.commentBefore = pair.key.commentBefore
				? `${pair.key.commentBefore}\n${normalized}`
				: normalized;
		}
	}
}

function fallbackValueForType(type: ValueType, enumValues?: readonly string[]): unknown {
	if (type === 'string') return '';
	if (type === 'number') return 0;
	if (type === 'boolean') return false;
	if (type === 'array') return [];
	if (type === 'object') return {};
	if (type === 'enum') return enumValues?.[0] ?? '';
	return '';
}

function resolveReplacementValue(
	defaultValue: unknown,
	type: ValueType,
	enumValues?: readonly string[],
): unknown {
	if (defaultValue !== undefined) {
		return defaultValue;
	}
	return fallbackValueForType(type, enumValues);
}

function hasSourceValue(raw: unknown): boolean {
	if (raw === undefined || raw === null) return false;
	if (typeof raw === 'string') return raw.trim() !== '';
	return true;
}

function formatSourceValue(raw: unknown): string {
	if (raw === undefined) return 'undefined';
	if (raw === null) return 'null';
	if (typeof raw === 'string') return raw;
	try {
		return JSON.stringify(raw);
	} catch {
		return String(raw);
	}
}

function isPlainObject(input: unknown): input is Record<string, unknown> {
	return typeof input === 'object' && input !== null && !Array.isArray(input);
}

function convertByType(raw: unknown, type: ValueType, enumValues?: readonly string[]): unknown {
	if (raw === undefined || raw === null) {
		return undefined;
	}

	if (typeof raw === 'string') {
		const trimmed = raw.trim();
		if (trimmed === '') {
			return undefined;
		}
		if (type === 'string') {
			return trimmed;
		}
		if (type === 'number') {
			const num = Number(trimmed);
			return Number.isFinite(num) ? num : undefined;
		}
		if (type === 'boolean') {
			const lower = trimmed.toLowerCase();
			if (['true', '1', 'yes', 'y'].includes(lower)) return true;
			if (['false', '0', 'no', 'n'].includes(lower)) return false;
			return undefined;
		}
		if (type === 'enum') {
			return enumValues?.includes(trimmed) ? trimmed : undefined;
		}
		if (type === 'array') {
			if (trimmed.startsWith('[')) {
				try {
					const parsed = JSON.parse(trimmed);
					if (Array.isArray(parsed)) {
						return parsed;
					}
				} catch {
					return undefined;
				}
			}
			const arr = trimmed
				.split(',')
				.map((item) => item.trim())
				.filter(Boolean);
			if (arr.length === 0) {
				return undefined;
			}
			const numArr = arr.map((item) => Number(item));
			return numArr.every((item) => Number.isFinite(item)) ? numArr : arr;
		}
		if (type === 'object') {
			try {
				const parsed = JSON.parse(trimmed);
				return isPlainObject(parsed) ? parsed : undefined;
			} catch {
				return undefined;
			}
		}
	}

	if (type === 'number') {
		if (typeof raw === 'number' && Number.isFinite(raw)) return raw;
		return undefined;
	}
	if (type === 'boolean') {
		if (typeof raw === 'boolean') return raw;
		return undefined;
	}
	if (type === 'string') {
		return typeof raw === 'string' ? raw : String(raw);
	}
	if (type === 'enum') {
		if (typeof raw !== 'string') return undefined;
		return enumValues?.includes(raw) ? raw : undefined;
	}
	if (type === 'array') {
		return Array.isArray(raw) ? raw : undefined;
	}
	if (type === 'object') {
		return isPlainObject(raw) ? raw : undefined;
	}

	return undefined;
}

function setNestedMutable(
	target: Record<string, unknown>,
	dottedPath: string,
	value: unknown,
): void {
	const keys = dottedPath.split('.');
	let cursor: Record<string, unknown> = target;
	for (let i = 0; i < keys.length - 1; i += 1) {
		const key = keys[i];
		const next = cursor[key];
		if (!isPlainObject(next)) {
			cursor[key] = {};
		}
		cursor = cursor[key] as Record<string, unknown>;
	}
	cursor[keys[keys.length - 1]] = value;
}

function parseEnvText(text: string): Record<string, string> {
	const envMap: Record<string, string> = {};
	for (const line of text.split('\n')) {
		const trimmed = line.trim();
		if (!trimmed || trimmed.startsWith('#')) continue;
		const splitAt = trimmed.indexOf('=');
		if (splitAt < 0) continue;
		const key = trimmed.slice(0, splitAt).trim();
		const value = trimmed.slice(splitAt + 1);
		envMap[key] = value;
	}
	return envMap;
}

function parseEnvFromDeploymentJson(
	jsonText: string,
	containerName: string,
): Record<string, string> | undefined {
	const obj = JSON.parse(jsonText);
	const containers = obj?.spec?.template?.spec?.containers;
	if (!Array.isArray(containers)) {
		return undefined;
	}
	const target = containers.find((item: any) => item?.name === containerName);
	if (!target || !Array.isArray(target.env)) {
		return undefined;
	}
	const result: Record<string, string> = {};
	for (const item of target.env) {
		const key = item?.name;
		if (!key || ignoredEnvKeys.has(key)) {
			continue;
		}
		if (typeof item?.value === 'string') {
			result[key] = item.value;
		}
	}
	return result;
}

function parseEnvFromConfigmapJson(jsonText: string): Record<string, string> | undefined {
	const obj = JSON.parse(jsonText);
	const envText = obj?.data?.['.env'];
	if (typeof envText !== 'string' || envText.trim() === '') {
		return undefined;
	}
	return parseEnvText(envText);
}

function mergeEnvSources(
	configmapEnv: Record<string, string> | undefined,
	deploymentEnv: Record<string, string> | undefined,
): Record<string, string> | undefined {
	const merged = {
		...(configmapEnv ?? {}),
		...(deploymentEnv ?? {}),
	};
	return Object.keys(merged).length === 0 ? undefined : merged;
}

function resolveAssetPath(relativePath: string, options: CliOptions): string {
	return join(options.assetsDir, relativePath);
}

function readSourceForProvider(provider: ProviderConfig, options: CliOptions): unknown {
	if (provider.mode === 'env') {
		const deployRes = runCmd([
			'kubectl',
			...(options.context ? ['--context', options.context] : []),
			'-n',
			provider.namespace,
			'get',
			'deployment',
			provider.deployment!,
			'-o',
			'json',
		]);
		const deploymentEnv = deployRes.ok
			? parseEnvFromDeploymentJson(deployRes.stdout, provider.container!)
			: undefined;

		let configmapEnv: Record<string, string> | undefined;
		if (provider.configmap) {
			const cmRes = runCmd([
				'kubectl',
				...(options.context ? ['--context', options.context] : []),
				'-n',
				provider.namespace,
				'get',
				'configmap',
				provider.configmap,
				'-o',
				'json',
			]);
			if (cmRes.ok) {
				configmapEnv = parseEnvFromConfigmapJson(cmRes.stdout);
			}
		}

		return mergeEnvSources(configmapEnv, deploymentEnv);
	}

	const cmRes = runCmd([
		'kubectl',
		...(options.context ? ['--context', options.context] : []),
		'-n',
		provider.namespace,
		'get',
		'configmap',
		provider.configmap!,
		'-o',
		'json',
	]);
	if (!cmRes.ok) {
		return undefined;
	}
	const cmObj = JSON.parse(cmRes.stdout);
	const cmYaml = cmObj?.data?.[provider.dataKey!];
	if (typeof cmYaml !== 'string' || cmYaml.trim() === '') {
		return undefined;
	}
	const doc = parseDocument(cmYaml);
	return doc.toJS();
}

function formatDefaultValue(value: unknown): string {
	if (value === undefined) return '<none>';
	if (typeof value === 'string') return value === '' ? "''" : value;
	if (value === null) return 'null';
	try {
		return JSON.stringify(value);
	} catch {
		return String(value);
	}
}

function buildOutputDir(options: CliOptions): string {
	if (options.outDir) {
		return options.outDir;
	}
	const context = (options.context && options.context.trim()) || getCurrentK8sContext();
	const safeContext = context.replace(/[^a-zA-Z0-9._-]/g, '_');
	const d = new Date();
	const time = `${d.getFullYear()}${String(d.getMonth() + 1).padStart(2, '0')}${String(d.getDate()).padStart(2, '0')}-${String(d.getHours()).padStart(2, '0')}${String(d.getMinutes()).padStart(2, '0')}${String(d.getSeconds()).padStart(2, '0')}`;
	return join(workspaceRoot, `config-export-${safeContext}-${time}`);
}

function processProvider(
	provider: ProviderConfig,
	options: CliOptions,
	outputDir: string,
): AppSummary {
	const sourceObj = readSourceForProvider(provider, options);
	if (!sourceObj || typeof sourceObj !== 'object') {
		const reason =
			provider.mode === 'env'
				? 'deployment env and configmap .env not found'
				: 'configmap data not found';
		const hint =
			provider.mode === 'env'
				? `kubectl -n ${provider.namespace} get deployment ${provider.deployment} -o json && kubectl -n ${provider.namespace} get configmap ${provider.configmap} -o json`
				: `kubectl -n ${provider.namespace} get configmap ${provider.configmap} -o json`;
		log('SKIP', `${provider.app} (${provider.namespace}) skipped: ${reason}; hint="${hint}"`);
		return {
			app: provider.app,
			namespace: provider.namespace,
			status: 'skipped',
			reason,
			missingCount: 0,
			invalidCount: 0,
		};
	}

	const templateText = readFileSync(resolveAssetPath(provider.templatePath, options), 'utf-8');
	const schemaObj = JSON.parse(
		readFileSync(resolveAssetPath(provider.schemaPath, options), 'utf-8'),
	);
	const ajv = new Ajv2020({ allErrors: true, strict: false });
	const validateSchema = ajv.compile(schemaObj);
	const outputDoc = parseDocument(templateText);
	const templateDoc = parseDocument(templateText);
	const templateObj = templateDoc.toJS();
	const comments = new Map<string, string[]>();
	let missingCount = 0;
	let invalidCount = 0;

	const grouped = new Map<string, MappingItem[]>();
	for (const item of provider.mappings) {
		if (!grouped.has(item.targetPath)) {
			grouped.set(item.targetPath, []);
		}
		grouped.get(item.targetPath)!.push(item);
	}

	for (const [targetPath, candidates] of grouped.entries()) {
		const defaultVal = getNestedValue(templateObj, targetPath);
		let resolved = false;
		let hasInvalid = false;
		const invalidRawValues: string[] = [];
		let firstNote = '';

		for (const mapping of candidates) {
			if (!firstNote && mapping.note) {
				firstNote = mapping.note;
			}
			const rawVal = getNestedValue(sourceObj, mapping.sourceKey);
			if (!hasSourceValue(rawVal)) {
				continue;
			}
			const converted = convertByType(rawVal, mapping.type, mapping.enumValues);
			if (converted === undefined) {
				hasInvalid = true;
				invalidRawValues.push(`${mapping.sourceKey}=${formatSourceValue(rawVal)}`);
				continue;
			}
			let valid = false;
			const testObj = outputDoc.toJS() as Record<string, unknown>;
			setNestedMutable(testObj, targetPath, converted);
			valid = Boolean(validateSchema(testObj));

			if (valid) {
				outputDoc.setIn(targetPath.split('.'), converted);
				resolved = true;
				break;
			}
			hasInvalid = true;
			invalidRawValues.push(`${mapping.sourceKey}=${formatSourceValue(rawVal)}`);
		}

		if (resolved) {
			continue;
		}

		const baseType = candidates[0].type;
		const replacementValue = resolveReplacementValue(
			defaultVal,
			baseType,
			candidates[0].enumValues,
		);
		outputDoc.setIn(targetPath.split('.'), replacementValue);

		if (hasInvalid) {
			invalidCount += 1;
			comments.set(
				targetPath,
				[
					'值不符合 Schema: 线上值校验失败，已回填默认值。',
					`线上值: ${invalidRawValues.join(' | ') || '<unknown>'}`,
					`默认值: ${formatDefaultValue(defaultVal)}`,
					firstNote ? `备注: ${firstNote}` : '',
				].filter(Boolean),
			);
		} else {
			missingCount += 1;
			comments.set(
				targetPath,
				[
					'来源不全: 线上缺失该字段，已回填默认值。',
					`默认值: ${formatDefaultValue(defaultVal)}`,
					firstNote ? `备注: ${firstNote}` : '',
				].filter(Boolean),
			);
		}
	}

	const outputFile = join(outputDir, `${provider.namespace}.config.yaml`);
	applyCommentsByAst(outputDoc, comments);
	writeFileSync(outputFile, outputDoc.toString(), 'utf-8');
	log(
		'INFO',
		`${provider.app} exported -> ${outputFile} (missing=${missingCount}, invalid=${invalidCount})`,
	);

	return {
		app: provider.app,
		namespace: provider.namespace,
		status: 'ok',
		missingCount,
		invalidCount,
		outputFile,
	};
}

function printSummary(results: readonly AppSummary[], outputDir: string): void {
	const ok = results.filter((item) => item.status === 'ok');
	const skipped = results.filter((item) => item.status === 'skipped');
	const missing = results.reduce((sum, item) => sum + item.missingCount, 0);
	const invalid = results.reduce((sum, item) => sum + item.invalidCount, 0);

	log('INFO', '---------------- Summary ----------------');
	log('INFO', `Output directory: ${outputDir}`);
	log('INFO', `Succeeded apps: ${ok.length}`);
	log('INFO', `Skipped apps: ${skipped.length}`);
	log('INFO', `Total missing fields: ${missing}`);
	log('INFO', `Total invalid fields: ${invalid}`);
	for (const item of skipped) {
		log('SKIP', `${item.app} (${item.namespace}): ${item.reason}`);
	}
}

function main(): void {
	const options = parseCli();
	const providers = loadProviders(options.providersConfig);
	const selected = new Set(options.apps ?? providers.map((item) => item.app));
	const runList = providers.filter((item) => selected.has(item.app));

	if (runList.length === 0) {
		throw new Error('No app selected. Use --apps=cronjob,dbprovider,...');
	}

	const outputDir = buildOutputDir(options);
	mkdirSync(outputDir, { recursive: true });
	log('INFO', `source=live, outputDir=${outputDir}`);

	const results: AppSummary[] = [];
	for (const provider of runList) {
		const result = processProvider(provider, options, outputDir);
		results.push(result);
	}

	printSummary(results, outputDir);
}

try {
	main();
} catch (error) {
	const message = error instanceof Error ? error.message : String(error);
	log('ERROR', message);
	process.exit(1);
}
