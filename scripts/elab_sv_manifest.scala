import toolkitUtils.*

import io.circe.yaml.parser as yamlParser
import io.circe.generic.auto.*
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.Json
import cats.data.Op

case class ProjectConfig(project: ProjectDetail)
case class ProjectDetail(name: String, `type`: String, ver: String)
case class ProjectParam(parameters: Option[List[ParamDef]] = None)
case class ParamDef(name: String, value: Json, data_t: Option[String] = None)

case class IOLogic(ext_port: String, direction: String)
case class IOIF(ext_port: String, modport: String)
case class BindDef(port_name: String, net: String, 
    index: Option[String] = None, slice: Option[String] = None, invert: Option[Boolean] = None, 
    modport: Option[String] = None, signal: Option[String] = None)

case class LogicNet(net_name: String, bits: Option[Json], bus_wd: Option[Json] = None, 
     io: Option[IOLogic] = None)
case class InterfaceNet(net_name: String, interface: String, array_size: Option[Json] = None,
    parameters: Option[List[ParamDef]] = None, bindings: Option[List[BindDef]] = None,
    io: Option[IOIF] = None)

case class NetsList(logic: Option[List[LogicNet]] = None, 
    interface: Option[List[InterfaceNet]] = None)

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
    yield (config,param,inst,nets)

    val (config,param,inst,nets) = validator.unwrapOrExit(projectData)

    val paramList : List[Map[String, String]] = param.parameters // Unpack Option
        .getOrElse(List.empty)      // For None case, i.e parameters left plank
        .map({paramDef =>     // Iterates through each ParamDef object

            // Assemble context
            Map(
            "name"      -> paramDef.name,
            "value"     -> paramDef.value.asString.getOrElse(paramDef.value.noSpaces),
            "data_t"    -> paramDef.data_t.getOrElse("")
            )
        })
        
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
            // Map parameters into list of map
            val ifParamList : List[Map[String, String]] = interfaceNet.parameters.getOrElse(List.empty)
                .map({
                    paramDef => Map(
                        "name"      -> paramDef.name,
                        "value"     -> paramDef.value.asString.getOrElse(paramDef.value.noSpaces)
                    )
                })
            val ifBindingList: List[Map[String, Any]] = interfaceNet.bindings.getOrElse(List.empty)
                .map({
                    bindDef => Map(
                        "port_name" -> bindDef.port_name,
                        "net"       -> bindDef.net,
                        "invert"    -> bindDef.invert.getOrElse(false),
                        "index"     -> bindDef.index.getOrElse(""),
                        "slice"     -> bindDef.slice.getOrElse(""),
                        "modport"   -> bindDef.modport.getOrElse(""),
                        "signal"    -> bindDef.signal.getOrElse("")
                    )
                }) 
            Map(
                "net_name"  -> interfaceNet.net_name,
                "interface" -> interfaceNet.interface,
                "array_size"-> interfaceNet.array_size.fold("1")({value => value.asString.getOrElse(value.noSpaces)}),
                "param"     -> ifParamList,  //List
                "bindings"  -> ifBindingList,
                "is_io"     -> interfaceNet.io.isDefined,
                "io_name"   -> interfaceNet.io.fold("")(_.ext_port),
                "io_modport"-> interfaceNet.io.fold("")(_.modport)
            )
        
    })

    val instanceList: List[Map[String, Any]] = inst.instances.getOrElse(List.empty).map({
        instDef =>
            // Map parameters into list of map
            val instParamList : List[Map[String, String]] = instDef.parameters.getOrElse(List.empty)
                .map({
                    paramDef => Map(
                        "name"      -> paramDef.name,
                        "value"     -> paramDef.value.asString.getOrElse(paramDef.value.noSpaces)
                    )
                })
            val instBindingList: List[Map[String, Any]] = instDef.bindings.getOrElse(List.empty)
                .map({
                    bindDef => Map(
                        "port_name" -> bindDef.port_name,
                        "net"       -> bindDef.net,
                        "invert"    -> bindDef.invert.getOrElse(false),
                        "index"     -> bindDef.index.getOrElse(""),
                        "slice"     -> bindDef.slice.getOrElse(""),
                        "modport"   -> bindDef.modport.getOrElse(""),
                        "signal"    -> bindDef.signal.getOrElse("")
                    )
                }) 
            Map(
                "inst_name" -> instDef.inst_name,
                "components"-> instDef.components,
                "array_size"-> instDef.array_size.fold("1")({value => value.asString.getOrElse(value.noSpaces)}),
                "param"     -> instParamList,  //List
                "bindings"  -> instBindingList,
            )
        
    })

    val manifestContext: Map[String, Any] = Map(
        "manifestParam"     -> paramList,
        "manifestLogicNet"  -> logicNets,
        "manifestLogicIO"   -> logicNets.filter(net => net("is_io") == true),
        "manifestIFNet"     -> interfaceNets,
        "manifestIFIO"      -> interfaceNets.filter(net => net("is_io") == true),
        "manifestInsts"     -> instanceList
    )

    val ioParamTemplate = os.pwd / "templates" / "io_param.svh.hbs"
    val ioSigTemplate = os.pwd / "templates" / "io_sig.svh.hbs"
    val netsTemplate = os.pwd / "templates" / "nets.svh.hbs"

    val ioParamDir = topDir / "src" / "gen" / s"io_param_${config.project.name}.svh"
    val ioSigDir = topDir / "src" / "gen" / s"io_sig_${config.project.name}.svh"
    val netsDir = topDir / "src" / "gen" / s"nets_${config.project.name}.svh"

    generator.generateFile(ioParamDir, ioParamTemplate, manifestContext)
    generator.generateFile(ioSigDir, ioSigTemplate, manifestContext)
    generator.generateFile(netsDir, netsTemplate, manifestContext)