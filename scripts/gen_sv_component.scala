import toolkitUtils.*

@main def genSvComponent(args: String*): Unit =
    val schemaDir = os.pwd / "schemas"
    val componentSchema = schemaDir / "sv_component.yaml"

    validator.unwrapOrExit(validator.checkFilesExist(Seq(schemaDir, componentSchema)))
    
    val currentScriptName = "gen_sv_component.scala"
    val (targetDir, projectName) = validator.validateDirAndName(args, currentScriptName)
    
    val (dirsConfig, filesConfig, generateContext) = generator.extractYamlConfig(componentSchema,projectName)

    // Inject project type into context so hbs can use
    val genContextFull = generateContext ++ Map (
        "project_type" -> "component"
    )
    
    // Check if the new project already exist
    val newProjectDir = targetDir / projectName
    if !os.exists(newProjectDir) then
        generator.generateDirs(dirsConfig, targetDir, projectName)
        generator.generateProject(filesConfig, targetDir, projectName, genContextFull)
    else
        println(s"Error: Another project already exist with that name (${projectName})")
    