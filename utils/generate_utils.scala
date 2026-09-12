package toolkitUtils

// Data models for YAML structure
case class ProjectDirs(dirs: List[String] = Nil)
case class ProjectFiles(files: List[ProjectFileSpec] = Nil)
case class ProjectFileSpec(template: String, output_dir: String)

import io.circe.yaml.parser as yamlParser
import io.circe.generic.auto.*

object generator:
    def extractYamlConfig(configYaml: os.Path): (ProjectDirs, ProjectFiles) =
        val projectConfig = for
            _       <- validator. checkFilesExist(Seq(configYaml))
            content <- try Right(os.read(configYaml)) 
                        catch case ex: Exception => Left(s"Failed reading $configYaml: ${ex.getMessage}")
            json    <- yamlParser.parse(content)
                        .left.map(err => s"YAML syntax error in $configYaml: ${err.getMessage}")
            dirs    <- json.as[ProjectDirs]
                        .left.map(err => s"Failed decoding 'dirs' in $configYaml: ${err.getMessage}")
            files   <- json.as[ProjectFiles]
                        .left.map(err => s"Failed decoding 'files' in $configYaml: ${err.getMessage}")
        yield (dirs, files)

        validator.unwrapOrExit(projectConfig)
    
    def generateDirs(dirsConfig: ProjectDirs, targetWorkspace: os.Path): Unit =
        for dir <- dirsConfig.dirs if dir.trim.nonEmpty do
            val dirPath = targetWorkspace / os.RelPath(dir)
            if os.exists(dirPath) then
                println(s"Directory $dirPath already exists")
            else
                os.makeDir.all(dirPath)
                println(s"Created directory: $dirPath")