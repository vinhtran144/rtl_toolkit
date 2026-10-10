import toolkitUtils.*

@main def elaborateSv (args: String*): Unit =
    val currentScriptName = "elaborate_sv.scala"   // For usage message

    // Validate target not in toolkit
    val (targetDir) = validator.validateDir(args, currentScriptName)

    val topDir = validator.getSvTopDir(targetDir)
    println(topDir)