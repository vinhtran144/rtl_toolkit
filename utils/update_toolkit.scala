//> using scala 3.8.4
//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep io.circe::circe-core:0.14.16
//> using dep io.circe::circe-parser:0.14.16
//> using file ../utils

import toolkitUtils.*
import io.circe.*
import io.circe.parser.parse as parseJson

@main def toolkitUpdater(args: String*): Unit = {
    val currentDir = os.pwd
    val scriptDir = currentDir / "scripts"
    val tasksJsonFile = currentDir / ".vscode" / "tasks.json"

    val validateToolkit: Either[String, Json] = for
        // Check that both tasks.json and scripts/ folder exist
        _ <- validator.checkFilesExist(Seq(tasksJsonFile, scriptDir))

        // Attempt to read and parse tasks.json safely
        rawJson = os.read(tasksJsonFile)
        parsedJson <- parseJson(rawJson).left.map { err =>
            s"Failed to parse JSON at $tasksJsonFile: ${err.getMessage}"
        }
    yield parsedJson

    // validateToolkit is Either, with Left being error message String, and Right if everything is success
    val baseJson = validateToolkit match
    case Left(errorMsg) =>
      println(s"Error: $errorMsg")
      sys.exit(1)

    case Right(json) =>
      json

    // Extract scripts name list, then create an array for select option
    val scriptFiles = os.list(scriptDir)
        .filter(_.ext == "scala")
        .map(_.last.stripSuffix(".scala"))
        .toList.sorted

    // generation scripts have prefix gen_, everything else execution scripts
    val (genScripts, execScripts) = scriptFiles
        .partition(_.startsWith("gen_"))

    val genArray = Json.fromValues(genScripts.map(str => Json.fromString(str.stripPrefix("gen_"))))
    val execArray = Json.fromValues(execScripts.map(str => Json.fromString(str)))
    // create Json from the output of a map, where the input is each string in List and mapped into Json string

    // execArray is for execScriptName
    // genArray is for genScriptName

    val updatedJson = baseJson.hcursor
        .downField("inputs")
        .withFocus(_.mapArray{ inputsVector =>  // circle lib, unpack into Vector[Json]
            inputsVector.map{ inputJson =>      // scala, each input is a Json
                inputJson.hcursor.downField("id").as[String].toOption match
                    case Some("execScriptName") =>      
                        inputJson.hcursor.downField("options").set(execArray).top.getOrElse(inputJson)
                    case Some("genScriptName") =>
                        inputJson.hcursor.downField("options").set(genArray).top.getOrElse(inputJson)
                    case _ => 
                        inputJson // Leave other inputs (e.g. targetName prompt) untouched
    }

        })
        .top
        .getOrElse(baseJson)
    
    // println(updatedJson.spaces2)

    // Overwrite the tasks.json
    val formattedJsonStr = Printer.spaces2.copy(dropNullValues = true).print(updatedJson)
    os.write.over(tasksJsonFile, formattedJsonStr, createFolders = true)

    println(s"Successfully updated toolkit")
    println(s"Execution scripts: ${execScripts.mkString(", ")}")
    println(s"Generation scripts: ${genScripts.map(str => str.stripPrefix("gen_")).mkString(", ")}")
}