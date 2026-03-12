# Export Provider Config

## 前置条件

- 可访问目标 Kubernetes 集群
- 本机可执行 `kubectl`
- `kubectl config current-context` 可返回有效上下文

额外运行时依赖：

- Scala 版本脚本：`scala-cli`
- TypeScript 版本脚本：`bun`

## CLI 参数

- `--apps=<a,b,c>`：只导出指定 app，逗号分隔
- `--context=<kube-context>`：指定 kubectl context（可选）
- `--out-dir=<path>`：指定输出目录（可选）
- `--providers-config=<path>`：provider 配置文件路径（可选）
- `--assets-dir=<path>`：template/schema 资产目录（可选）

推荐参数：

```sh
  --providers-config=./providers.json \
  --assets-dir=./assets \
  --apps=cronjob,dbprovider,applaunchpad,costcenter,devbox,objectstorage
```

## 执行

```sh
# Bun
bun migrate.ts

# Scala CLI
scala-cli migrate.sc --

# Native
./migrate
```

## Native 构建

### Scala Native Image

示例命令（无 fallback）：

```bash
scala-cli package --power \
  --native-image \
  --graalvm-jvm-id graalvm-java21:21.0.2 \
  --graalvm-args=--no-fallback \
  -f \
  -o ./migrate-graal-native \
  ./migrate.sc
```

### Bun

```bash
bun build --compile --outfile migrate-ts-native migrate.ts
```
