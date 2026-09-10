//> using scala 3.8.4
//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep io.circe::circe-core:0.14.16
//> using dep io.circe::circe-generic:0.14.16
//> using dep io.circe::circe-yaml:1.15.0
//> using file ../utils

import toolkitUtils.*
import io.circe.yaml.parser as yamlParser
import io.circe.generic.auto.*
import os.call

case class ProjectDirs(dirs: List[String])

@main def genSVInterface(args: String*): Unit =
    val templateDir = os.pwd / "templates"
    val interfaceTemp = templateDir / "sv_interface.yaml"

    val currentScriptName = "gen_sv_interface.scala"
    val (targetDir, projectName) = validator.validateDirAndName(args, currentScriptName)
    
    val rawYaml =os.read(interfaceTemp)
    // Parse YAML into ProjectDir case class
    val dirConfig = for 
        json <- yamlParser.parse(rawYaml)
        config <- json.as[ProjectDirs]
    yield config

    dirConfig match
        case Left(err) =>
            println(s"Error parsing YAML: ${err.getMessage}")
            sys.exit(1)

        case Right(cfg) =>
            // Create folders from yaml template
            for dir <- cfg.dirs if dir.trim.nonEmpty do
                val dirPath = targetDir / os.RelPath(dir)
                os.makeDir.all(dirPath)
                println(s"Created folder: $dir")
    