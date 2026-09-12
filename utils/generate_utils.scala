package toolkitUtils

import cats.instances.double

// Data models for YAML structure
case class ProjectDirs(dirs: List[String] = Nil)
case class ProjectFiles(files: List[ProjectFileSpec] = Nil)
case class ProjectFileSpec(template: String, output_dir: String, file_type: Option[String] = None)
// Option[String] to handle if there's no file_type included

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

    def generateFiles(filesConfig: ProjectFiles, targetWorkspace: os.Path,
        projectName: String= "new_project", 
        extraContext: Map[String, String] = Map.empty): Unit =
        
        val context = Map(
            "name" -> projectName.trim,              // replace {{name}} with projectName
            "NAME" -> projectName.trim.toUpperCase   // ie. change axi_bus to AXI_BUS, for macros
        ) ++ extraContext                            // Add any other Map to hbs template 
        
        val templateDir = os.pwd / "templates"
        validator.unwrapOrExit(validator.checkFilesExist(Seq(templateDir)))
        println(filesConfig)
        for fileSpec <- filesConfig.files do
            val fileName = constructFileName(
                inputName    = projectName.trim,
                fileType     = fileSpec.file_type,
                templateName = fileSpec.template
            )
            println(s"Generating $fileName")
            
    // Name constructed structure: <project_name>_<file_type>_<template>
    // Example project_name = mem_bus, file_type = monitor, template = task.svh.hbs
    // returns: mem_bus_monitor_task.svh
    private def constructFileName(
      inputName: String,
      fileType: Option[String],
      templateName: String
    ): String =
        // Get name of template, trime .hbs extension
        val templateSeg = if templateName.endsWith(".hbs") then
            templateName.substring(0, templateName.length - 4)
        else
            templateName

        // Add file type segment if included
        val typeSeg = fileType.map(_.trim).filter(_.nonEmpty) match
            case Some(t) => s"_${t}"
            case None    => ""

        // Prefix inputName
        s"${inputName}${typeSeg}_${templateSeg}"

