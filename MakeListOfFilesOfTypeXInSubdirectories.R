## auxiliary functions 

ExtractRelativeFilePaths <- function( subdir_name, ext ){
  ## auxiliary function that harvest the files of extension "ext 
  ## of the subdirectory passed as argument
  current_working_directory <- getwd()
  
  if ( is.character(ext) & (length(ext)>0) ) {
    targetFiles <- tryCatch( {
      nwd <- setwd(normalizePath( subdir_name,
                                  winslash="/",
                                  mustWork=NA))
      filePattern = paste(c(".*[.]", ext ), collapse="")
      
      targetFiles <- list.files( getwd(),
                              filePattern,
                              recursive=TRUE)
      
      listOfFilesWithRelativePath <- unlist(lapply( targetFiles, 
                                        function(x) file.path( subdir_name, 
                                                               x , 
                                                               fsep=.Platform$file.sep ) ) )    
      
      }, warn = function(wrn){
        setwd(current_working_directory)
        cat("Better check results: ", wrn)
        return( listOfFilesWithRelativePath )
      },  error = function(err){
        setwd(current_working_directory)
        cat("No files with target extension found.")
        return(list(" ") )
      }
    )
  }
  
  if( getwd() != current_working_directory ) {
    setwd(current_working_directory)
    listOfFilesWithRelativePath
  }
  else list("")
}


MakeListOfFilesOfTypeXInSubdirectories <- function( ext="sum" ) {
  ## Call this function to prepare a list of 
  ## file paths and names relative to the current working directory
  ## of all files of extension "ext"
  
  
  ## Make a list of all subdirectories:
  listOffilesAndDirectories <- file.info(dir()) ## look up ?file.info
  logicalFilterForDirectories <- listOffilesAndDirectories$isdir # results in logical vector
  listOfSubDirectories <- dir()[logicalFilterForDirectories]
  
  rawListOfFiles <- sapply( X=listOfSubDirectories, 
                            FUN=ExtractRelativeFilePaths, 
                            ext, 
                            USE.NAMES=FALSE )
  
  flatten <- function(x) Reduce("c", x)
  
  flatten(rawListOfFiles)
} 

