import toolkitUtils.*

@main def elaborateSv (args: String*): Unit =
    val currentScriptName = "elaborate_sv.scala"   // For usage message
    val (targetDir) = validator.validateDir(args, currentScriptName)