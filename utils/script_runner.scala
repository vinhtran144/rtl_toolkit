import scala.sys.process.*

@main def scriptRunner(isGen: Boolean, scriptName: String, currentDir: String, targetDir: String, extraArg: String = ""): Unit =
    val words = scriptName.split("_").filter(_.nonEmpty).map(_.capitalize)

    // Build scripts to runScript and mainClass
    val (targetScript, mainClass) = if isGen then
        val className = words.mkString
        (s"$currentDir/scripts/gen_$scriptName.scala",s"gen$className")
    else
        // make sure the classname is camel case
        val className = words.headOption.map(_.toLowerCase).getOrElse("") + words.drop(1).mkString
        (s"$currentDir/scripts/$scriptName.scala", className)

    // Change "scala" -> "scala-cli" (or "scala-cli.bat" / "scala.bat" on Windows)
    val isWindows = sys.props("os.name").toLowerCase.contains("win")
    val scalaExec = if isWindows then "scala-cli.bat" else "scala-cli"

    val baseCmd = Seq(
        scalaExec, "run", ".", targetScript,
        "--main-class", mainClass,
        "--", targetDir
    )

    val fullCmd = if isGen then
        baseCmd :+ extraArg
    else
        baseCmd
    
    val exitCode = Process(fullCmd).!
    if exitCode != 0 then
        sys.error(s"Script runner failed with exit code $exitCode")
