//> using scala 3.8.4
//> using dep com.lihaoyi::os-lib:0.11.8

@main def vsInit(args: String*): Unit = {
    val toolkitDir = findToolkitDir(os.pwd) 
    //pass execution directory to find the toolkit folder
    //Note: only work only if this method is called within toolkit folder

    val scriptDir = toolkitDir / "script"

    //Checking if the "script" exist and a folder
    if (!(os.exists(scriptDir) && os.isDir(scriptDir))) {
        println("ERROR: No script folder found")
        sys.exit(1)
    }

    // Filter scala files and then remove .scala extension
    val scriptFiles = os.list(scriptDir)
    .filter(_.ext == "scala")
    .map(_.last.stripSuffix(".scala"))
    .toList

    println(scriptFiles)
}

def findToolkitDir(currentDir: os.Path): os.Path = {
    // walk up the current directory to find a *_toolkit folder
    if (currentDir.last.endsWith("_toolkit"))
        currentDir // returns current directory
    else if (currentDir == os.root){
        // Exit since it reaches root
        println("ERROR: Could not find *_toolkit parent")
        sys.exit(1)
    } else {
        // Repeat method with the current directory moved up
        findToolkitDir(currentDir / os.up)
    }
}