package toolkitUtils

// Validate Utils, returns Either

object validator:
    def checkArgCount(args: Seq[String], minCount: Int, usageMsg: String): Either[String, Unit] =
    if args.length >= minCount then
      Right(()) 
    else
      Left(s"Insufficient arguments.\nUsage: $usageMsg")

    def checkTargetDir(rawPath: String): Either[String, os.Path] =
        try Right(os.Path(rawPath, os.pwd))
        catch case ex: Exception => Left(s"Invalid target directory '$rawPath': ${ex.getMessage}")

    def checkFilesExist(paths: Seq[os.Path]): Either[String, Unit] =
        val missing = paths.filterNot(os.exists)
        if missing.isEmpty then
            Right(())
        else
            Left("Required file(s) missing:\n" + missing.map(p => s" - $p").mkString("\n"))