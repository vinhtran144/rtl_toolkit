//> using scala 3.8.4
//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep io.circe::circe-core:0.14.9
//> using dep io.circe::circe-parser:0.14.9
//> using dep io.circe::circe-yaml:0.15.1

import io.circe._
import io.circe.yaml.parser
import io.circe.parser.{parse => parseJson}

@main def vsInit(args: String*): Unit = {
    val toolkitDir = findToolkitDir(os.pwd) 
    //pass execution directory to find the toolkit folder
    //Note: only work only if this method is called within toolkit folder

    val scriptDir = toolkitDir / "script"

    //Checking if the "script" exist and a folder
    if (!(os.exists(scriptDir) && os.isDir(scriptDir))) {
        println("ERROR: No script folder found")
        sys.exit(1)
    }

    // Filter scala files and then remove .scala extension
    // val scriptFiles = os.list(scriptDir)
    // .filter(_.ext == "scala")
    // .map(_.last.stripSuffix(".scala"))
    // .toList

    // println(s"Discovered Scripts: ${scriptFiles.mkString(", ")}")

    // Read task template
    val taskTempDir = toolkitDir / "templates" / "toolbox_init_tasks.yaml"
    val rawYamlStr   = os.read(taskTempDir)

    val templateJson: Json = parser.parse(rawYamlStr) match {
      case Right(json) => json
      case Left(err)   => 
        println(s"[!] Failed to parse YAML: ${err.getMessage}")
        sys.exit(1)
    }

    // Inject toolkit local directory into task template
    val envObj = Json.obj("TOOLKIT_PATH" -> Json.fromString(toolkitDir.toString))


    val taskJson = templateJson.hcursor
        .downField("tasks")
        .downArray                      // Assume the's only 1 task requiring toolkit path
        .downField("options")
        .downField("env")
        .withFocus(_.deepMerge(envObj))
        .top
        .getOrElse(templateJson)

    println(taskJson)

    //VSCode Tasks folder
    val tasksJsonFile = toolkitDir / ".vscode" / "tasks.json"

    // Write tasks.json into toolkit .vscode folder
    val formattedJsonStr = Printer.spaces2.copy(dropNullValues = true).print(taskJson)
    os.write.over(tasksJsonFile, formattedJsonStr, createFolders = true)
}

def findToolkitDir(currentDir: os.Path): os.Path = {
    // walk up the current directory to find a *_toolkit folder
    if (currentDir.last.endsWith("_toolkit"))
        currentDir // returns current directory
    else if (currentDir == os.root){
        // Exit since it reaches root
        println("ERROR: Could not find *_toolkit parent")
        sys.exit(1)
    } else {
        // Repeat method with the current directory moved up
        findToolkitDir(currentDir / os.up)
    }
}