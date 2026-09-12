package toolkitUtils

object validator:
    def checkArgCount(args: Seq[String], minCount: Int, usageMsg: String): Either[String, Unit] =
    if args.length >= minCount then
      Right(()) 
    else
      Left(s"Insufficient arguments.\nUsage: $usageMsg")

    def checkTargetDir(rawPath: String): Either[String, os.Path] =
        try Right(os.Path(rawPath, os.pwd))
        catch case ex: Exception => Left(s"Invalid target directory '$rawPath': ${ex.getMessage}")

    // Note, using Seq, so a list of os.Path can be added
    def checkFilesExist(paths: Seq[os.Path]): Either[String, Unit] =
        val missing = paths.filterNot(os.exists)
        if missing.isEmpty then
            Right(())
        else
            Left("Required file(s) missing:\n" + missing.map(p => s" - $p").mkString("\n"))

    // Unwrap Either and return based on the input generics
    def unwrapOrExit[T](result: Either[String, T]): T = result match
        case Right(data) => data
        case Left(error) =>
            println(s"Error: $error")
            sys.exit(1)

    //
    // For Checking a Target module and Name from input args
    // returns ONLY 1. Target path, 2. project name
    def validateDirAndName(
      args: Seq[String],
      scriptName: String
     ): (os.Path, String) =
        val usageMsg   = s"scala run $scriptName -- <target_dir> <project_name>"
        val callCheck = for
            _ <- checkArgCount(args, 2, usageMsg)
            targetDir <- checkTargetDir(args(0))

            projectName = args(1)
        yield (targetDir, projectName)
        unwrapOrExit(callCheck)

     // returns ONLY Target path
    def validateDir(
      args: Seq[String],
      scriptName: String
     ): os.Path =
        val usageMsg = s"scala run $scriptName -- <target_dir> "
        val callCheck = for
            _ <- checkArgCount(args, 1, usageMsg)
            targetDir <- checkTargetDir(args(0))
        yield (targetDir)
        unwrapOrExit(callCheck)