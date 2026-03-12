//> using scala 3
//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep org.yaml:snakeyaml:2.6
//> using dep com.networknt:json-schema-validator:3.0.1
//> using dep tools.jackson.core:jackson-databind:3.1.0

import com.networknt.schema.{SchemaRegistry, SpecificationVersion}
import tools.jackson.databind.{JsonNode, ObjectMapper}
import org.yaml.snakeyaml.{DumperOptions, LoaderOptions, Yaml}
import org.yaml.snakeyaml.comments.{CommentLine, CommentType}
import org.yaml.snakeyaml.nodes.{MappingNode, Node, NodeTuple, ScalarNode}

import scala.jdk.CollectionConverters._
import scala.util.Try
import java.io.{StringReader, StringWriter}

type AnyMap = collection.mutable.LinkedHashMap[String, Any]

enum ProviderMode:
  case env, configmap

enum ValueType:
  case string, number, boolean, array, obj, enumType

case class CliOptions(
    context: Option[String] = None,
    apps: Option[Set[String]] = None,
    outDir: Option[os.Path] = None,
    providersConfig: os.Path =
      os.pwd / "scripts" / "export-provider-config.providers.json",
    assetsDir: os.Path = os.pwd / "scripts" / "export-provider-config-assets"
)

case class MappingItem(
    sourceKey: String,
    targetPath: String,
    valueType: ValueType,
    enumValues: Seq[String] = Seq.empty,
    note: Option[String] = None
)

case class ProviderConfig(
    app: String,
    namespace: String,
    mode: ProviderMode,
    deployment: Option[String],
    container: Option[String],
    configmap: Option[String],
    dataKey: Option[String],
    templatePath: String,
    schemaPath: String,
    mappings: Seq[MappingItem]
)

case class AppSummary(
    app: String,
    namespace: String,
    status: String,
    reason: Option[String] = None,
    missingCount: Int = 0,
    invalidCount: Int = 0,
    outputFile: Option[String] = None
)

val ignoredEnvKeys = Set(
  "NODE_ENV",
  "KUBERNETES_SERVICE_HOST",
  "KUBERNETES_SERVICE_PORT",
  "NEXT_PUBLIC_MOCK_USER",
  "NEXT_RUNTIME"
)

def log(level: String, message: String): Unit = println(s"[$level] $message")

def parseCli(args: Seq[String]): CliOptions =
  args.foldLeft(CliOptions()) { (acc, arg) =>
    if !arg.startsWith("--") then acc
    else
      val parts = arg.stripPrefix("--").split("=", 2)
      val key = parts.headOption.getOrElse("").trim
      val value = if parts.length > 1 then parts(1).trim else ""
      key match
        case "context" if value.nonEmpty => acc.copy(context = Some(value))
        case "apps" if value.nonEmpty    =>
          acc.copy(apps =
            Some(value.split(",").map(_.trim).filter(_.nonEmpty).toSet)
          )
        case "out-dir" if value.nonEmpty =>
          acc.copy(outDir = Some(os.Path(value, os.pwd)))
        case "providers-config" if value.nonEmpty =>
          acc.copy(providersConfig = os.Path(value, os.pwd))
        case "assets-dir" if value.nonEmpty =>
          acc.copy(assetsDir = os.Path(value, os.pwd))
        case "source" | "raw-dir" =>
          throw new RuntimeException(
            s"Unsupported option: --$key (live mode only)"
          )
        case _ => acc
  }

def runCmd(parts: Seq[String]): (Boolean, String, String) =
  val stdout = new StringBuilder
  val stderr = new StringBuilder
  val result = os
    .proc(parts)
    .call(
      cwd = os.pwd,
      check = false,
      stdout = os.ProcessOutput.Readlines(stdout.append(_).append('\n')),
      stderr = os.ProcessOutput.Readlines(stderr.append(_).append('\n'))
    )
  (result.exitCode == 0, stdout.result().trim, stderr.result().trim)

def getCurrentK8sContext: String =
  val (ok, out, err) = runCmd(Seq("kubectl", "config", "current-context"))
  if !ok then throw new RuntimeException(s"Failed to get kubectl context: $err")
  out

def parseYamlToAny(input: String): Any =
  val yaml = Yaml()
  yaml.load(input)

def toScalaAny(value: Any): Any = value match
  case m: java.util.Map[?, ?] =>
    val out = collection.mutable.LinkedHashMap.empty[String, Any]
    m.asScala.foreach { case (k, v) =>
      out.update(String.valueOf(k), toScalaAny(v))
    }
    out
  case l: java.util.List[?] =>
    l.asScala.toVector.map(toScalaAny)
  case other => other

def toJavaAny(value: Any): Any = value match
  case m: collection.Map[?, ?] =>
    val out = new java.util.LinkedHashMap[String, Any]()
    m.foreach { case (k, v) => out.put(String.valueOf(k), toJavaAny(v)) }
    out
  case seq: Seq[?] =>
    seq.map(toJavaAny).asJava
  case other => other

def getNestedValue(obj: Any, path: String): Option[Any] =
  val keys = path.split("\\.").toList
  keys.foldLeft(Option(obj)) { (cursor, key) =>
    cursor.flatMap {
      case m: collection.Map[?, ?] =>
        m.asInstanceOf[collection.Map[String, Any]].get(key)
      case _ => None
    }
  }

def setNestedValue(target: AnyMap, path: String, value: Any): Unit =
  val keys = path.split("\\.").toVector
  var cursor: AnyMap = target
  for key <- keys.dropRight(1) do
    val next = cursor.get(key) match
      case Some(map: collection.mutable.Map[?, ?]) => map.asInstanceOf[AnyMap]
      case Some(map: collection.Map[?, ?])         =>
        val converted = collection.mutable.LinkedHashMap.from(
          map.asInstanceOf[collection.Map[String, Any]]
        )
        cursor.update(key, converted)
        converted
      case _ =>
        val created = collection.mutable.LinkedHashMap.empty[String, Any]
        cursor.update(key, created)
        created
    cursor = next
  cursor.update(keys.last, value)

def readText(path: os.Path): String = os.read(path)

def fallbackValueOf(t: ValueType, enums: Seq[String]): Any =
  t match
    case ValueType.string  => ""
    case ValueType.number  => 0
    case ValueType.boolean => false
    case ValueType.array   => Seq.empty[Any]
    case ValueType.obj => collection.mutable.LinkedHashMap.empty[String, Any]
    case ValueType.enumType => enums.headOption.getOrElse("")

def replacementValue(
    defaultValue: Option[Any],
    t: ValueType,
    enums: Seq[String]
): Any =
  defaultValue match
    case Some(null) => fallbackValueOf(t, enums)
    case Some(v)    => v
    case None       => fallbackValueOf(t, enums)

def hasSourceValue(raw: Option[Any]): Boolean =
  raw match
    case None            => false
    case Some(null)      => false
    case Some(s: String) => s.trim.nonEmpty
    case Some(_)         => true

def formatSourceValue(raw: Any): String =
  raw match
    case null      => "null"
    case s: String => s
    case other     =>
      Try(new ObjectMapper().writeValueAsString(toJavaAny(other)))
        .getOrElse(String.valueOf(other))

def convertByType(
    raw: Option[Any],
    t: ValueType,
    enums: Seq[String]
): Option[Any] =
  raw.flatMap {
    case s: String =>
      val trimmed = s.trim
      if trimmed.isEmpty then None
      else
        t match
          case ValueType.string  => Some(trimmed)
          case ValueType.number  => Try(trimmed.toDouble).toOption
          case ValueType.boolean =>
            trimmed.toLowerCase match
              case "true" | "1" | "yes" | "y" => Some(true)
              case "false" | "0" | "no" | "n" => Some(false)
              case _                          => None
          case ValueType.enumType => Some(trimmed).filter(enums.contains)
          case ValueType.array    =>
            if trimmed.startsWith("[") then
              Try(
                toScalaAny(
                  new ObjectMapper()
                    .readValue(trimmed, classOf[java.util.List[?]])
                )
              ).toOption
            else
              val arr = trimmed.split(",").toSeq.map(_.trim).filter(_.nonEmpty)
              if arr.isEmpty then None
              else
                val asNum = arr.map(v => Try(v.toDouble).toOption)
                if asNum.forall(_.nonEmpty) then Some(asNum.flatten)
                else Some(arr)
          case ValueType.obj =>
            Try {
              toScalaAny(
                new ObjectMapper()
                  .readValue(trimmed, classOf[java.util.Map[?, ?]])
              )
            }.toOption
    case n: java.lang.Integer if t == ValueType.number  => Some(n.doubleValue())
    case n: java.lang.Long if t == ValueType.number     => Some(n.doubleValue())
    case n: java.lang.Double if t == ValueType.number   => Some(n.doubleValue())
    case b: java.lang.Boolean if t == ValueType.boolean =>
      Some(b.booleanValue())
    case seq: Seq[?] if t == ValueType.array             => Some(seq)
    case list: java.util.List[?] if t == ValueType.array =>
      Some(list.asScala.toVector.map(toScalaAny))
    case map: collection.Map[?, ?] if t == ValueType.obj => Some(map)
    case map: java.util.Map[?, ?] if t == ValueType.obj => Some(toScalaAny(map))
    case v if t == ValueType.string => Some(String.valueOf(v))
    case _                          => None
  }

def formatDefault(v: Option[Any]): String =
  v match
    case None                         => "<none>"
    case Some(null)                   => "null"
    case Some(s: String) if s.isEmpty => "''"
    case Some(other)                  =>
      Try(new ObjectMapper().writeValueAsString(toJavaAny(other)))
        .getOrElse(String.valueOf(other))

def deepCopyMap(input: AnyMap): AnyMap =
  val mapper = ObjectMapper()
  val copied = mapper.convertValue(
    toJavaAny(input),
    classOf[java.util.LinkedHashMap[String, Object]]
  )
  toScalaAny(copied).asInstanceOf[AnyMap]

def findTupleByPath(root: Node, path: Seq[String]): Option[NodeTuple] =
  path
    .foldLeft(Option(root -> Option.empty[NodeTuple])) { (state, key) =>
      state.flatMap { case (currentNode, _) =>
        currentNode match
          case mapNode: MappingNode =>
            val nextTuple = mapNode.getValue.asScala.find { entry =>
              entry.getKeyNode match
                case scalar: ScalarNode => scalar.getValue == key
                case _                  => false
            }
            nextTuple.map(tuple => tuple.getValueNode -> Some(tuple))
          case _ => None
      }
    }
    .flatMap(_._2)

def applyAstComments(
    yamlText: String,
    comments: collection.Map[String, Seq[String]]
): String =
  val loaderOptions = LoaderOptions()
  loaderOptions.setProcessComments(true)
  val dumperOptions = DumperOptions()
  dumperOptions.setProcessComments(true)
  dumperOptions.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
  dumperOptions.setPrettyFlow(true)
  val yaml = Yaml(loaderOptions, dumperOptions)
  val root = yaml.compose(StringReader(yamlText))
  if root == null then return yamlText

  comments.foreach { case (path, lines) =>
    findTupleByPath(root, path.split("\\.").toSeq).foreach { tuple =>
      val existing = Option(tuple.getKeyNode.getBlockComments)
        .map(_.asScala.toSeq)
        .getOrElse(Seq.empty)
      val extra =
        lines.map(msg => CommentLine(null, null, s" $msg", CommentType.BLOCK))
      tuple.getKeyNode.setBlockComments((existing ++ extra).asJava)
    }
  }

  val writer = StringWriter()
  yaml.serialize(root, writer)
  writer.toString

def requiredText(node: JsonNode, key: String): String =
  val value = node.path(key).asString("").trim
  if value.isEmpty then
    throw new RuntimeException(s"Invalid providers config: missing \"$key\"")
  value

def optionalText(node: JsonNode, key: String): Option[String] =
  val child = node.path(key)
  if child.isMissingNode || child.isNull then None
  else
    val value = child.asString("").trim
    if value.isEmpty then None else Some(value)

def parseProviderMode(raw: String): ProviderMode =
  raw match
    case "env"       => ProviderMode.env
    case "configmap" => ProviderMode.configmap
    case other => throw new RuntimeException(s"Invalid provider mode: $other")

def parseValueType(raw: String): ValueType =
  raw match
    case "string"  => ValueType.string
    case "number"  => ValueType.number
    case "boolean" => ValueType.boolean
    case "array"   => ValueType.array
    case "object"  => ValueType.obj
    case "enum"    => ValueType.enumType
    case other => throw new RuntimeException(s"Invalid mapping type: $other")

def loadProviders(configPath: os.Path): Seq[ProviderConfig] =
  if !os.exists(configPath) then
    throw new RuntimeException(
      s"Providers config not found: ${configPath.toString}"
    )

  val mapper = ObjectMapper()
  val root = mapper.readTree(os.read(configPath))
  if !root.isArray then
    throw new RuntimeException(
      s"Invalid providers config: root must be array, file=${configPath.toString}"
    )

  root.iterator().asScala.toSeq.map { node =>
    val mappingsNode = node.path("mappings")
    if !mappingsNode.isArray then
      throw new RuntimeException(
        s"Invalid providers config: mappings must be array, app=${requiredText(node, "app")}"
      )

    val mappings = mappingsNode.iterator().asScala.toSeq.map { m =>
      val enumValuesNode = m.path("enumValues")
      val enumValues =
        if enumValuesNode.isArray then
          enumValuesNode.iterator().asScala.toSeq.map(_.asText(""))
        else Seq.empty[String]
      MappingItem(
        sourceKey = requiredText(m, "sourceKey"),
        targetPath = requiredText(m, "targetPath"),
        valueType = parseValueType(requiredText(m, "type")),
        enumValues = enumValues,
        note = optionalText(m, "note")
      )
    }

    ProviderConfig(
      app = requiredText(node, "app"),
      namespace = requiredText(node, "namespace"),
      mode = parseProviderMode(requiredText(node, "mode")),
      deployment = optionalText(node, "deployment"),
      container = optionalText(node, "container"),
      configmap = optionalText(node, "configmap"),
      dataKey = optionalText(node, "dataKey"),
      templatePath = requiredText(node, "templatePath"),
      schemaPath = requiredText(node, "schemaPath"),
      mappings = mappings
    )
  }

def parseEnvText(text: String): Map[String, String] =
  text.linesIterator.flatMap { line =>
    val trimmed = line.trim
    if trimmed.isEmpty || trimmed.startsWith("#") then None
    else
      val idx = trimmed.indexOf('=')
      if idx < 0 then None
      else
        val key = trimmed.substring(0, idx).trim
        val value = trimmed.substring(idx + 1)
        Some(key -> value)
  }.toMap

def parseEnvFromDeployJson(
    jsonText: String,
    containerName: String
): Option[Map[String, String]] =
  val mapper = ObjectMapper()
  val root = mapper.readTree(jsonText)
  val containers =
    root.path("spec").path("template").path("spec").path("containers")
  if !containers.isArray then None
  else
    val envMap = containers
      .iterator()
      .asScala
      .find(node => node.path("name").asText() == containerName)
      .flatMap { c =>
        val env = c.path("env")
        if !env.isArray then None
        else
          Some(
            env
              .iterator()
              .asScala
              .flatMap { item =>
                val key = item.path("name").asText("")
                val value = item.path("value").asText("")
                if key.nonEmpty && value != null && !ignoredEnvKeys
                    .contains(key)
                then Some(key -> value)
                else None
              }
              .toMap
          )
      }
    envMap

def parseEnvFromConfigmapJson(jsonText: String): Option[Map[String, String]] =
  val mapper = ObjectMapper()
  val root = mapper.readTree(jsonText)
  val envText = root.path("data").path(".env").asString("")
  if envText.trim.isEmpty then None else Some(parseEnvText(envText))

def mergeEnvSources(
    configmapEnv: Option[Map[String, String]],
    deploymentEnv: Option[Map[String, String]]
): Option[Map[String, String]] =
  val merged =
    configmapEnv.getOrElse(Map.empty) ++ deploymentEnv.getOrElse(Map.empty)
  if merged.isEmpty then None else Some(merged)

def resolveAssetPath(relativePath: String, options: CliOptions): os.Path =
  os.Path(relativePath, options.assetsDir)

def readSource(provider: ProviderConfig, options: CliOptions): Option[Any] =
  provider.mode match
    case ProviderMode.env =>
      val depCmd = Seq("kubectl") ++ options.context
        .map(c => Seq("--context", c))
        .getOrElse(Seq.empty) ++
        Seq(
          "-n",
          provider.namespace,
          "get",
          "deployment",
          provider.deployment.getOrElse(""),
          "-o",
          "json"
        )
      val (depOk, depOut, _) = runCmd(depCmd)
      val deploymentEnv =
        if !depOk then None
        else parseEnvFromDeployJson(depOut, provider.container.getOrElse(""))
      val configmapEnv = provider.configmap.flatMap { cm =>
        val cmCmd = Seq("kubectl") ++ options.context
          .map(c => Seq("--context", c))
          .getOrElse(Seq.empty) ++
          Seq("-n", provider.namespace, "get", "configmap", cm, "-o", "json")
        val (cmOk, cmOut, _) = runCmd(cmCmd)
        if !cmOk then None else parseEnvFromConfigmapJson(cmOut)
      }
      mergeEnvSources(configmapEnv, deploymentEnv)
    case ProviderMode.configmap =>
      val cmd = Seq("kubectl") ++ options.context
        .map(c => Seq("--context", c))
        .getOrElse(Seq.empty) ++
        Seq(
          "-n",
          provider.namespace,
          "get",
          "configmap",
          provider.configmap.getOrElse(""),
          "-o",
          "json"
        )
      val (ok, out, _) = runCmd(cmd)
      if !ok then None
      else
        val mapper = ObjectMapper()
        val root = mapper.readTree(out)
        val yamlText =
          root.path("data").path(provider.dataKey.getOrElse("")).asString("")
        if yamlText.trim.isEmpty then None
        else Some(toScalaAny(parseYamlToAny(yamlText)))

def getSchemaValidator(schemaText: String): JsonNode => Boolean =
  val registry =
    SchemaRegistry.withDefaultDialect(SpecificationVersion.DRAFT_2020_12)
  val schema = registry.getSchema(schemaText)
  (node: JsonNode) =>
    val errors = schema.validate(node)
    errors == null || errors.isEmpty

def toJsonNode(value: Any): JsonNode =
  val mapper = ObjectMapper()
  mapper.valueToTree(toJavaAny(value))

def processProvider(
    provider: ProviderConfig,
    options: CliOptions,
    outDir: os.Path
): AppSummary =
  val sourceOpt = readSource(provider, options)
  if sourceOpt.isEmpty then
    val reason =
      if provider.mode == ProviderMode.env then
        "deployment env and configmap .env not found"
      else "configmap data not found"
    val hint =
      if provider.mode == ProviderMode.env then
        s"kubectl -n ${provider.namespace} get deployment ${provider.deployment.getOrElse("")} -o json && kubectl -n ${provider.namespace} get configmap ${provider.configmap.getOrElse("")} -o json"
      else
        s"kubectl -n ${provider.namespace} get configmap ${provider.configmap.getOrElse("")} -o json"
    log(
      "SKIP",
      s"${provider.app} (${provider.namespace}) skipped: $reason; hint=\"$hint\""
    )
    return AppSummary(provider.app, provider.namespace, "skipped", Some(reason))

  val templateText = readText(resolveAssetPath(provider.templatePath, options))
  val schemaText = readText(resolveAssetPath(provider.schemaPath, options))
  val templateObj =
    toScalaAny(parseYamlToAny(templateText)).asInstanceOf[AnyMap]
  val outputObj = collection.mutable.LinkedHashMap.from(templateObj)
  val sourceObj = sourceOpt.get
  val validateSchema = getSchemaValidator(schemaText)

  var missing = 0
  var invalid = 0
  val comments = collection.mutable.LinkedHashMap.empty[String, Seq[String]]
  val grouped = provider.mappings.groupBy(_.targetPath)

  grouped.foreach { case (targetPath, candidates) =>
    val defaultVal = getNestedValue(templateObj, targetPath)
    var resolved = false
    var hasInvalid = false
    val invalidRawValues = collection.mutable.ArrayBuffer.empty[String]
    var note: Option[String] = None

    candidates.foreach { mapping =>
      if !resolved then
        if note.isEmpty then note = mapping.note
        val raw = getNestedValue(sourceObj, mapping.sourceKey)
        if hasSourceValue(raw) then
          val converted =
            convertByType(raw, mapping.valueType, mapping.enumValues)
          converted match
            case Some(v) =>
              val testObj = deepCopyMap(outputObj)
              setNestedValue(testObj, targetPath, v)
              val ok = validateSchema(toJsonNode(testObj))
              if ok then
                setNestedValue(outputObj, targetPath, v)
                resolved = true
              else
                hasInvalid = true
                invalidRawValues.append(
                  s"${mapping.sourceKey}=${formatSourceValue(raw.orNull)}"
                )
            case None =>
              hasInvalid = true
              invalidRawValues.append(
                s"${mapping.sourceKey}=${formatSourceValue(raw.orNull)}"
              )
    }

    if !resolved then
      val baseType = candidates.head.valueType
      setNestedValue(
        outputObj,
        targetPath,
        replacementValue(defaultVal, baseType, candidates.head.enumValues)
      )
      if hasInvalid then
        invalid += 1
        comments.update(
          targetPath,
          Seq(
            "值不符合 Schema: 线上值校验失败，已回填默认值。",
            s"线上值: ${
                if invalidRawValues.nonEmpty
                then invalidRawValues.mkString(" | ")
                else "<unknown>"
              }",
            s"默认值: ${formatDefault(defaultVal)}"
          ) ++ note.map(n => s"备注: $n")
        )
      else
        missing += 1
        comments.update(
          targetPath,
          Seq(
            "来源不全: 线上缺失该字段，已回填默认值。",
            s"默认值: ${formatDefault(defaultVal)}"
          ) ++ note.map(n => s"备注: $n")
        )
  }

  val dumperOptions = DumperOptions()
  dumperOptions.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
  dumperOptions.setPrettyFlow(true)
  dumperOptions.setProcessComments(true)
  val yamlDumper = Yaml(dumperOptions)
  val dumped = yamlDumper.dump(toJavaAny(outputObj))
  val withComments = applyAstComments(dumped, comments.toMap)
  val outputFile = outDir / s"${provider.namespace}.config.yaml"
  os.write.over(outputFile, withComments, createFolders = true)
  log(
    "INFO",
    s"${provider.app} exported -> ${outputFile.toString} (missing=$missing, invalid=$invalid)"
  )
  AppSummary(
    provider.app,
    provider.namespace,
    "ok",
    missingCount = missing,
    invalidCount = invalid,
    outputFile = Some(outputFile.toString)
  )

def buildOutDir(options: CliOptions): os.Path =
  options.outDir.getOrElse {
    val context = options.context.getOrElse(getCurrentK8sContext)
    val safeCtx = context.replaceAll("[^a-zA-Z0-9._-]", "_")
    val now = java.time.LocalDateTime.now()
    val time =
      f"${now.getYear}%04d${now.getMonthValue}%02d${now.getDayOfMonth}%02d-${now.getHour}%02d${now.getMinute}%02d${now.getSecond}%02d"
    os.pwd / s"config-export-$safeCtx-$time"
  }

def printSummary(results: Seq[AppSummary], outDir: os.Path): Unit =
  val ok = results.count(_.status == "ok")
  val skipped = results.count(_.status == "skipped")
  val missing = results.map(_.missingCount).sum
  val invalid = results.map(_.invalidCount).sum
  log("INFO", "---------------- Summary ----------------")
  log("INFO", s"Output directory: ${outDir.toString}")
  log("INFO", s"Succeeded apps: $ok")
  log("INFO", s"Skipped apps: $skipped")
  log("INFO", s"Total missing fields: $missing")
  log("INFO", s"Total invalid fields: $invalid")
  results.filter(_.status == "skipped").foreach { item =>
    log(
      "SKIP",
      s"${item.app} (${item.namespace}): ${item.reason.getOrElse("unknown")}"
    )
  }

def runMain(args: Seq[String]): Unit =
  val options = parseCli(args)
  val providers = loadProviders(options.providersConfig)
  val selected = options.apps.getOrElse(providers.map(_.app).toSet)
  val runList = providers.filter(p => selected.contains(p.app))
  if runList.isEmpty then
    throw new RuntimeException(
      "No app selected. Use --apps=cronjob,dbprovider,..."
    )
  val outDir = buildOutDir(options)
  os.makeDir.all(outDir)
  log(
    "INFO",
    s"source=live, outputDir=${outDir.toString}, providersConfig=${options.providersConfig.toString}, assetsDir=${options.assetsDir.toString}"
  )
  val results = runList.map(processProvider(_, options, outDir))
  printSummary(results, outDir)

runMain(args.toSeq)
