import toolkitUtils.*

@main def elabSvManifest (args: String*): Unit =
    val currentScriptName = "elaborate_sv.scala"   // Validate script calls
    val (targetDir) = validator.validateDir(args, currentScriptName)

    // Rules for sv top dir
    val topReg: List[String] = List("*_top.sv", "README.md", ".gitignore" )
    val topDir = validator.getTopDir(targetDir,topReg)

    val manifestYaml = topDir / "src" / "manifest.yaml"
    val genDes = topDir / "src" / "gen"

    val templateDir = os.pwd / "templates"      // Templates within toolkit

    // Extract context from manifest
    val elabFields = List("project","parameters","modport","logic","logicIO","interface","interfaceIO","instances")
    val manifestContext = manifestHandler.getContext(manifestYaml,elabFields)

    val projectName = manifestContext("project").asInstanceOf[Map[String, Any]]("project_name").asInstanceOf[String]
    val projectType = manifestContext("project").asInstanceOf[Map[String, Any]]("project_type").asInstanceOf[String]

    //Generate common files
    generator.generateFile(genDes / s"io_param_${projectName}.svh", templateDir / "io_param.svh.hbs", manifestContext)
    generator.generateFile(genDes / s"io_sig_${projectName}.svh", templateDir / "io_sig.svh.hbs", manifestContext)
    generator.generateFile(genDes / s"nets_${projectName}.svh", templateDir / "nets.svh.hbs", manifestContext)

    // Generate files for interface and component
    projectType match {
        case "Interface" =>
            generator.generateFile(genDes / s"modport_${projectName}.svh", templateDir / "modport.svh.hbs", manifestContext)
        case "Component" =>
            generator.generateFile(genDes / s"insts_${projectName}.svh", templateDir / "insts.svh.hbs", manifestContext)
        case other =>
}
