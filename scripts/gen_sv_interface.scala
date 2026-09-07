//> using scala 3.8.4
//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep io.circe::circe-core:0.14.16
//> using dep io.circe::circe-generic:0.14.16
//> using dep io.circe::circe-yaml:1.15.0

import io.circe.yaml.parser as yamlParser
import io.circe.generic.auto.*

case class ProjectDirs(dirs: List[String])

@main def genSVInterface(args: String*): Unit =
    // Checking for required 2 input arguments
    if args.length < 2 then
        println("Error: Not enough input arguments, require 1. Target dir, 2. Project name")
        sys.exit(1)
    
    val targetDir = os.Path(args(0), os.pwd)
    val projectName = args(1)

    // check if the target Dir exists
    if !os.exists(targetDir) then
        println(s"Error, $targetDir isn't a valid directory")
        sys.exit(1)

    val templateDir = os.pwd / "templates"
    val interfaceTemp = templateDir / "sv_interface.yaml"
    if !os.exists(interfaceTemp) then
        println(s"Error: YAML config missing on: $interfaceTemp")
        sys.exit(1)

    val rawYaml =os.read(interfaceTemp)
    // Parse YAML into ProjectDir case class
    val dirConfig = for 
        json <- yamlParser.parse(rawYaml)
        config <- json.as[ProjectDirs]
    yield config
    // Returns Either[LeftType, RightType]

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
    