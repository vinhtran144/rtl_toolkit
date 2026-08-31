//> using scala 3.8.4

//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep io.circe::circe-core:0.14.16
//> using dep io.circe::circe-parser:0.14.16

import io.circe.*
import io.circe.parser.parse as parseJson

@main def toolkitUpdater(args: String*): Unit = {
    val currentDir = os.pwd
    // Get the directory of tasks.json
    val tasksJsonFile = currentDir / ".vscode" / "tasks.json"

    // check for tasks.json
    if !os.exists(tasksJsonFile) then
        println(s"Error: tasks.json is not found on: $tasksJsonFile")
        sys.exit(1)

    //Attempt to parse JSON into scala
    val baseJson = parseJson(os.read(tasksJsonFile)).fold(
        err => 
            println(s"Error: Failed to parse $tasksJsonFile: ${err.getMessage}")
            sys.exit(1),
        identity
    )

    println(baseJson.spaces2)
}