import toolkitUtils.*

import io.circe.yaml.parser as yamlParser
import io.circe.generic.auto.*
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.Json
import io.circe.Decoder

case class ProjectConfig(project: ProjectDetail)
case class ProjectDetail(name: String, `type`: String, ver: String)
case class ProjectParam(parameters: Option[List[ParamDef]] = None)
case class ParamDef(name: String, value: Json, data_t: Option[String] = None)

case class IOLogic(ext_port: String, direction: String)
case class IOIF(ext_port: String, modport: String)
case class BindDef(port_name: String, net: String, 
    index: Option[String] = None, slice: Option[String] = None, invert: Option[Boolean] = None, 
    per_instance: Option[Boolean] = None, offset: Option[Json] = None,
    modport: Option[String] = None, signal: Option[String] = None)

case class LogicNet(net_name: String, bits: Option[Json], bus_wd: Option[Json] = None, 
     io: Option[IOLogic] = None)
case class InterfaceNet(net_name: String, interface: String, array_size: Option[Json] = None,
    parameters: Option[List[ParamDef]] = None, bindings: Option[List[BindDef]] = None,
    io: Option[IOIF] = None)

case class NetsList(logic: Option[List[LogicNet]] = None, 
    interface: Option[List[InterfaceNet]] = None)

// Explicitly defined TaskDef since circle seems to have problem with too deep nested case class
case class TaskDef(task_name: String, args: Option[List[ArgsDef]] = None)
object TaskDef {
  given Decoder[TaskDef] = Decoder.instance { cursor =>
    for {
      task_name <- cursor.downField("task_name").as[String]
      args      <- cursor.downField("args").as[Option[List[ArgsDef]]]
    } yield TaskDef(task_name, args)
  }
}
case class SignalDef(direction: String, net: String)
case class ArgsDef(arg_name: String, arg_t: String)
case class ModportDef(mod_name: String, signals: Option[List[SignalDef]] = None, 
    tasks: Option[List[TaskDef]] = None)

case class InterfaceModport(modport: Option[List[ModportDef]])

case class InstsDef(inst_name: String, components: String, array_size: Option[Json] = None,
    parameters: Option[List[ParamDef]] = None, bindings: Option[List[BindDef]] = None)
case class ProjectInsts(instances: Option[List[InstsDef]] = None)

@main def elabSvManifest (args: String*): Unit =
    val currentScriptName = "elaborate_sv.scala"   // Validate script calls
    val (targetDir) = validator.validateDir(args, currentScriptName)

    // Rules for sv top dir
    val topReg: List[String] = List("*_top.sv", "README.md", ".gitignore" )

    val topDir = validator.getTopDir(targetDir,topReg)
    val manifestYaml = topDir / "src" / "manifest.yaml"

    val templateDir = os.pwd / "templates"
    val genDes = topDir / "src" / "gen"

    val projectData = for
        _       <- validator. checkFilesExist(Seq(manifestYaml))
        content <- try Right(os.read(manifestYaml)) 
                    catch case ex: Exception => Left(s"Failed reading $manifestYaml: ${ex.getMessage}")
        json    <- yamlParser.parse(content)
                    .left.map(err => s"YAML syntax error in $manifestYaml: ${err.getMessage}")
        config  <- json.as[ProjectConfig]
                    .left.map(err => s"Failed decoding 'project' in $manifestYaml: ${err.getMessage}")
        param   <- json.as[ProjectParam]
                    .left.map(err => s"Failed decoding 'parameters' in $manifestYaml: ${err.getMessage}")
        inst    <- json.as[ProjectInsts]
                    .left.map(err => s"Failed decoding 'instances' in $manifestYaml: ${err.getMessage}")
        // Circle seems to have trouble with nested data, so manually shift hcursor
        nets    <- json.hcursor.downField("nets").as[NetsList]
                    .left.map(err => s"Failed decoding 'nets' in $manifestYaml: ${err.getMessage}")
        modports <- json.as[InterfaceModport]
                    .left.map(err => s"Failed decoding 'modports' in $manifestYaml: ${err.getMessage}")
    yield (config,param,inst,nets,modports)

    val (config,param,inst,nets,modports) = validator.unwrapOrExit(projectData)

    val paramList : List[Map[String, String]] = param.parameters.getOrElse(List.empty).map(buildParamMap)
        
    val logicNets : List[Map[String, Any]] = nets.logic.getOrElse(List.empty).map({
        logicNet =>
            Map(
                "net_name"  -> logicNet.net_name,
                "bits"      -> logicNet.bits.fold("1")({value => value.asString.getOrElse(value.noSpaces)}),
                "bus_wd"    -> logicNet.bus_wd.fold("1")({value => value.asString.getOrElse(value.noSpaces)}),
                "is_io"     -> logicNet.io.isDefined,
                "io_name"   -> logicNet.io.fold("")(_.ext_port),
                "io_dir"    -> logicNet.io.fold("")(_.direction)
            )
    })

    val interfaceNets: List[Map[String, Any]] = nets.interface.getOrElse(List.empty).map({
        interfaceNet =>
            Map(
                "net_name"  -> interfaceNet.net_name,
                "interface" -> interfaceNet.interface,
                "array_size"-> interfaceNet.array_size.fold("1")({value => value.asString.getOrElse(value.noSpaces)}),
                "param"     -> interfaceNet.parameters.getOrElse(List.empty).map(buildParamMap),
                "bindings"  -> interfaceNet.bindings.getOrElse(List.empty).map(buildBindingMap),
                "is_io"     -> interfaceNet.io.isDefined,
                "io_name"   -> interfaceNet.io.fold("")(_.ext_port),
                "io_modport"-> interfaceNet.io.fold("")(_.modport)
            )
        
    })

    val manifestContext: Map[String, Any] = Map(
        "manifestParam"     -> paramList,
        "manifestLogicNet"  -> logicNets,
        "manifestLogicIO"   -> logicNets.filter(net => net("is_io") == true),
        "manifestIFNet"     -> interfaceNets,
        "manifestIFIO"      -> interfaceNets.filter(net => net("is_io") == true)
    )

    val ioParamTemplate = templateDir / "io_param.svh.hbs"
    val ioSigTemplate = templateDir / "io_sig.svh.hbs"
    val netsTemplate = templateDir / "nets.svh.hbs"

    val ioParamDir = genDes / s"io_param_${config.project.name}.svh"
    val ioSigDir = genDes / s"io_sig_${config.project.name}.svh"
    val netsDir = genDes / s"nets_${config.project.name}.svh"

    generator.generateFile(ioParamDir, ioParamTemplate, manifestContext)
    generator.generateFile(ioSigDir, ioSigTemplate, manifestContext)
    generator.generateFile(netsDir, netsTemplate, manifestContext)

    // Checking for interface project to generate modport
    config.project.`type` match {
        case "Interface" =>
            val modportList: List [Map[String,Any]] = modports.modport.getOrElse(List.empty).map({
                modportDef =>   
                Map(
                    "mod_name"  -> modportDef.mod_name,
                    "signals"   -> modportDef.signals.getOrElse(List.empty).map({signalDef => 
                                    Map(
                                        "direction" -> signalDef.direction,
                                        "net"       -> signalDef.net
                                    )}),
                    "tasks"     -> modportDef.tasks.getOrElse(List.empty).map({task =>
                                    Map( 
                                        "task_name" -> task.task_name,
                                        "args"      -> task.args.getOrElse(List.empty)
                                            .map({
                                                arg => Map(
                                                    "arg_name"  -> arg.arg_name,
                                                    "arg_t"     -> arg.arg_t
                                                )
                                            })
                                )})
                )
            })
            val interfaceContext: Map[String, Any] = Map(
                "manifestModport" -> modportList
            )
            val modportTemplate = templateDir / "modport.svh.hbs"
            val modportDir = genDes / s"modport_${config.project.name}.svh"
            generator.generateFile(modportDir, modportTemplate, interfaceContext)
        case "Component" =>
            val instanceList: List [Map[String,Any]] = inst.instances.getOrElse(List.empty).map({
                instDef =>   
                Map(
                    "inst_name"     -> instDef.inst_name,
                    "components"    -> instDef.components,
                    "array_size"    -> instDef.array_size.fold("1")({value => value.asString.getOrElse(value.noSpaces)}),
                    "param"         -> instDef.parameters.getOrElse(List.empty).map(buildParamMap),
                    "bindings"      -> instDef.bindings.getOrElse(List.empty).map(buildBindingMap) 
                )
            })
            val componentContext: Map[String, Any] = Map(
                "manifestInsts" -> instanceList
            )
            val instsTemplate = templateDir / "insts.svh.hbs"
            val instsDir = genDes / s"insts_${config.project.name}.svh"
            generator.generateFile(instsDir, instsTemplate, componentContext)
        case other =>
}

private def buildParamMap(paramDef: ParamDef): Map[String, String] = Map(
  "name"  -> paramDef.name,
  "value" -> paramDef.value.asString.getOrElse(paramDef.value.noSpaces),
  "data_t"    -> paramDef.data_t.getOrElse("")
)

private def buildBindingMap(bindDef: BindDef): Map[String, Any] = Map(
  "port_name"    -> bindDef.port_name,
  "net"          -> bindDef.net,
  "invert"       -> bindDef.invert.getOrElse(false),
  "index"        -> bindDef.index.getOrElse(""),
  "slice"        -> bindDef.slice.getOrElse(""),
  "per_instance" -> bindDef.per_instance.getOrElse(false),
  "offset"       -> bindDef.offset.fold("0")(value => value.asString.getOrElse(value.noSpaces)),
  "modport"      -> bindDef.modport.getOrElse(""),
  "signal"       -> bindDef.signal.getOrElse("")
)