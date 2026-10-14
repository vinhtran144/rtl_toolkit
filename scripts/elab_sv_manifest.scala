import toolkitUtils.*

import io.circe.yaml.parser as yamlParser
import io.circe.generic.auto.*
import io.circe.Json

case class ProjectConfig(project: ProjectDetail)
case class ProjectDetail(name: String, `type`: String, ver: String)
case class ProjectParam(parameters: Option[List[ParamDef]] = None)
case class ParamDef(name: String, value: Json, data_t: Option[String] = None)

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
    yield (config,param)

    val (config,param) = validator.unwrapOrExit(projectData)

    val paramList : List[Map[String, String]] = param.parameters // Unpack Option
        .getOrElse(List.empty)      // For None case, i.e parameters left plank
        .map({paramDef =>     // Iterates through each ParamDef object

            val paramVal = paramDef.value.asString.getOrElse(paramDef.value.noSpaces)
            // Assemble context
            Map(
            "name"      -> paramDef.name,
            "value"     -> paramVal,
            "data_t"    -> paramDef.data_t.getOrElse("")
            )
        })
        .toList

    val ioParamTemplate = os.pwd / "templates" / "io_param.svh.hbs"
    val paramContext: Map[String, Any] = Map(
        "manifestParam" -> paramList
    )
    val ioParamDir = topDir / "src" / "gen" / s"io_param_${config.project.name}.svh"
    generator.generateFile(ioParamDir, ioParamTemplate, paramContext)

