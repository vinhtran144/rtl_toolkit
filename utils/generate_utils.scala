package toolkitUtils

// Data models for YAML structure
case class ProjectDirs(dirs: List[String] = Nil)
case class ProjectFiles(files: List[ProjectFileSpec] = Nil)
case class ProjectFileSpec(template: String, output_dir: String, file_type: Option[String] = None)
// Option[String] to handle if there's no file_type included

import io.circe.yaml.parser as yamlParser
import io.circe.generic.auto.*

object generator:
    def extractYamlConfig(configYaml: os.Path, projectName: String= "new_project"): (ProjectDirs, ProjectFiles, Map[String, Any]) =
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
        yield 
            val filesPaths = files.files.map { fileSpec =>
                 // Construct filename 
                val fileName = constructFileName(  
                    inputName    = projectName.trim,
                    fileType     = fileSpec.file_type,
                    templateName = fileSpec.template
                )
                val fileDir = fileSpec.output_dir       
                s"$fileDir/$fileName"               // Append dir to the names
            }   
            val generateContext = extractContext(filesPaths)
            (dirs, files, generateContext)

        validator.unwrapOrExit(projectConfig)

    private def extractContext(filesPaths: List[String]): Map[String, Any] =
        val includeDirs = filesPaths
            .filter(_.endsWith(".svh"))             // Get all header files
            .map { p =>
                val lastSlash = p.lastIndexOf('/') // Get directory to the file
                if lastSlash > 0 then
                    p.substring(0, lastSlash) 
                else ""
            }
            .filter(_.nonEmpty)
            .distinct                               // remove duplicates
        
        // Get all files ends with .sv to be source files
        val sourceFiles = filesPaths
            .filter(p => p.endsWith(".sv") || p.endsWith(".v")) // Get sources files
            .distinct

        // Check if there's package, supposed to be 1 singular, fixed <projectName>_pkg
        val hasPkg : Boolean = sourceFiles.filter(_.contains("_pkg")).nonEmpty

        // Extract single io_param and io_sig headers 
        val paramHeader: String = filesPaths
            .find(_.contains("io_param.svh"))
            .getOrElse("")
        val sigHeader: String = filesPaths
            .find(_.contains("io_sig.svh"))
            .getOrElse("")
        
        // Get list of logic headers
        val logicHeaders = filesPaths.filter(_.contains("logic.svh"))
        // Get list of task headers
        val taskHeaders = filesPaths.filter(_.contains("task.svh"))

        // Construct context for extractContext
        Map(
            "include_dirs" -> includeDirs,
            "source_files" -> sourceFiles,
            "hasPkg"       -> hasPkg,
            "module_param" -> paramHeader,
            "module_sign"  -> sigHeader,
            "logic_header" -> logicHeaders,
            "task_header"  -> taskHeaders
        )

    
    def generateDirs(dirsConfig: ProjectDirs, targetDir: os.Path): Unit =
        for dir <- dirsConfig.dirs if dir.trim.nonEmpty do
            val dirPath = targetDir / os.RelPath(dir)
            if os.exists(dirPath) then
                println(s"Directory $dirPath already exists")
            else
                os.makeDir.all(dirPath)
                println(s"Created directory: $dirPath")

    def generateFiles(filesConfig: ProjectFiles, targetDir: os.Path,
        projectName: String= "new_project", 
        extraContext: Map[String, Any] = Map.empty): Unit =
        
        val context = Map(
            "name" -> projectName.trim,              // replace {{name}} with projectName
            "NAME" -> projectName.trim.toUpperCase   // ie. change axi_bus to AXI_BUS, for macros
        ) ++ extraContext                            // Add any other Map to hbs template 
               
        for fileSpec <- filesConfig.files do
            val fileName = constructFileName(
                inputName    = projectName.trim,
                fileType     = fileSpec.file_type,
                templateName = fileSpec.template
            )
            val filePath = targetDir / os.RelPath(fileSpec.output_dir) / fileName
            val templatePath = os.pwd / "templates" / fileSpec.template
        
            // target file check
            if os.exists(filePath) then
                println(s"File already exist, skipping $filePath")
            else if !os.exists(templatePath) then
                println(s"Warning: Template not found $templatePath, skip generation")
            else 
                val templateRaw = os.read(templatePath)
                val renderedContent = HandlebarsRenderer.render(templateRaw, context)
                os.write.over(filePath, renderedContent)
            
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


