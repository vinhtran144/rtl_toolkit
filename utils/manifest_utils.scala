package toolkitUtils

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

object manifestHandler:
    def getContext(yamlPath: os.Path, fields: List[String] = List("project")): Map[String, Any] =
        val projectData = for
            _       <- validator. checkFilesExist(Seq(yamlPath))
            content <- try Right(os.read(yamlPath)) 
                        catch case ex: Exception => Left(s"Failed reading $yamlPath: ${ex.getMessage}")
            json    <- yamlParser.parse(content)
                        .left.map(err => s"YAML syntax error in $yamlPath: ${err.getMessage}")
            config  <- json.as[ProjectConfig]
                        .left.map(err => s"Failed decoding 'project' in $yamlPath: ${err.getMessage}")
            param   <- json.as[ProjectParam]
                        .left.map(err => s"Failed decoding 'parameters' in $yamlPath: ${err.getMessage}")
            inst    <- json.as[ProjectInsts]
                        .left.map(err => s"Failed decoding 'instances' in $yamlPath: ${err.getMessage}")
            // Circle seems to have trouble with nested data, so manually shift hcursor
            nets    <- json.hcursor.downField("nets").as[NetsList]
                        .left.map(err => s"Failed decoding 'nets' in $yamlPath: ${err.getMessage}")
            modports <- json.as[InterfaceModport]
                        .left.map(err => s"Failed decoding 'modports' in $yamlPath: ${err.getMessage}")
        yield (config,param,inst,nets,modports)

        val (config,param,inst,nets,modports) = validator.unwrapOrExit(projectData)

        val projectConfig: Map[String, String] = Map(
                "project_name"  -> config.project.name,
                "project_type"  -> config.project.`type`,
                "project_ver"   -> config.project.ver
            )

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

        val manifestContext: Map[String, Any] = Map(
            "project"       -> projectConfig,
            "parameters"    -> paramList,
            "logic"         -> logicNets,
            "logicIO"       -> logicNets.filter(net => net("is_io") == true),
            "interface"     -> interfaceNets,
            "interfaceIO"   -> interfaceNets.filter(net => net("is_io") == true),
            "modport"       -> modportList,
            "instances"     -> instanceList
        )

        // Filter to return only the requested fields
        manifestContext.view.filterKeys(fields.contains).toMap

    
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
    
