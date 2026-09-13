//> using scala 3.8.4
//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep io.circe::circe-core:0.14.16
//> using dep io.circe::circe-generic:0.14.16
//> using dep io.circe::circe-yaml:1.15.0
//> using dep com.github.jknack:handlebars:4.5.4
//> using dep org.slf4j:slf4j-nop:2.0.19
//> using file ../utils
 
import toolkitUtils.*

@main def genSVInterface(args: String*): Unit =
    val templateDir = os.pwd / "templates"
    val interfaceConfig = templateDir / "sv_interface.yaml"
    validator.unwrapOrExit(validator.checkFilesExist(Seq(templateDir, interfaceConfig)))

    val currentScriptName = "gen_sv_interface.scala"
    val (targetDir, projectName) = validator.validateDirAndName(args, currentScriptName)
    
    val (dirsConfig, filesConfig, generateContext) = generator.extractYamlConfig(interfaceConfig,projectName)

    generator.generateDirs(dirsConfig, targetDir)
    generator.generateFiles(filesConfig, targetDir, projectName, generateContext)
    