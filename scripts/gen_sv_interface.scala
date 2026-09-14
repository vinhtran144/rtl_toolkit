import toolkitUtils.*

@main def genSvInterface(args: String*): Unit =
    val templateDir = os.pwd / "templates"
    val interfaceConfig = templateDir / "sv_interface.yaml"
    validator.unwrapOrExit(validator.checkFilesExist(Seq(templateDir, interfaceConfig)))

    val currentScriptName = "gen_sv_interface.scala"
    val (targetDir, projectName) = validator.validateDirAndName(args, currentScriptName)
    
    val (dirsConfig, filesConfig, generateContext) = generator.extractYamlConfig(interfaceConfig,projectName)

    generator.generateDirs(dirsConfig, targetDir)
    generator.generateFiles(filesConfig, targetDir, projectName, generateContext)
    