import toolkitUtils.*

@main def genSvInterface(args: String*): Unit =
    val schemaDir = os.pwd / "schemas"
    val interfaceSchema = schemaDir / "sv_interface.yaml"

    validator.unwrapOrExit(validator.checkFilesExist(Seq(schemaDir, interfaceSchema)))
    
    val currentScriptName = "gen_sv_interface.scala"
    val (targetDir, projectName) = validator.validateDirAndName(args, currentScriptName)
    
    val (dirsConfig, filesConfig, generateContext) = generator.extractYamlConfig(interfaceSchema,projectName)

    generator.generateDirs(dirsConfig, targetDir, projectName)
    generator.generateFiles(filesConfig, targetDir, projectName, generateContext)
    