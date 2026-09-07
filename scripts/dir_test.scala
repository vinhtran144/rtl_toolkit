//> using scala 3.8.4
//> using dep com.lihaoyi::os-lib:0.11.8


@main def dirTest(args: String*): Unit = {
    val targetPathStr = if (args.nonEmpty) args(0) else "."
    println("Hello World")
    val currentDir = os.pwd
    println(s"Current directory $currentDir")
    println(s"Executed from $targetPathStr")
}