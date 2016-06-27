## PIPESIM SUM File specific functionality

source("ParseResultsFromSumFileLine.R")

tools.groupNames <- list( "FriggsStFergus"=1, "FriggsMCP01"=2, "MCP01StFergus"=3)
tools.flowRatesEqGasMMSM3D <-  list( "FriggsStFergus"=c(15.679,
                                                         20.001,
                                                         21.308,
                                                         22.614,
                                                         27.438,
                                                         28.846,
                                                         31.76,
                                                         33.569),
                                     "FriggsMCP01"=c(  33.4,
                                                         38.9,
                                                         40.9,
                                                         43.8),
                                     "MCP01StFergus"=c(32.4,
                                                         33.4,
                                                         36.1,
                                                         38.4,
                                                         38.9,
                                                         40.9,
                                                         43.8)  )
tools.experimentalInletPressureBara <- list( "FriggsStFergus" = c(114.0,
                                                                  108.0,
                                                                  105.0,
                                                                  131.0,
                                                                  145.0,
                                                                  132.0,
                                                                  143.0,
                                                                  149.1 ),
                                             "FriggsMCP01"=c(   149.1,
                                                                  148.4,
                                                                  148.4,
                                                                  147.9 ),
                                             
                                             "MCP01StFergus"=c( 109.2,
                                                                  109.3,
                                                                  117.5,
                                                                  122.9,
                                                                  125.1,
                                                                  131.4,
                                                                  140.3 ) )

tools.experimentalExitPressureBara <- list( "FriggsStFergus" = c( 88.9, 
                                                                  66.0,
                                                                  50.0,
                                                                  88.0, 
                                                                  84.5, 
                                                                  50.0, 
                                                                  50.0, 
                                                                  48.7),
                                            "FriggsMCP01"=c(   109.4,
                                                                  88.9, 
                                                                  30.0, 
                                                                  16.9 ),
                                            "MCP01StFergus"=c(  51.1, 
                                                                  48.7, 
                                                                  47.7, 
                                                                  48.5, 
                                                                  48.0, 
                                                                  48.5, 
                                                                  49.1 ) )
   
tools.experimentalInletTemperatureDegC <- list( "FriggsStFergus" = c(47.0,
                                                                     47.0,
                                                                     47.0,
                                                                     47.0,
                                                                     47.0,
                                                                     47.0,
                                                                     47.0,
                                                                     28.0 ),
                                                "FriggsMCP01"=c(   28.0,
                                                                     29.3,
                                                                     32.6,
                                                                     27.9 ),
                                                
                                                "MCP01StFergus"=c( 5.6,
                                                                     2.0,
                                                                     5.1,
                                                                     5.1,
                                                                     23.3,
                                                                     33.3,
                                                                     45.6 ) )


tools.mappingPIPESIMMethodNames <- list( "BB1"=           "Neotec Beggs & Brill Original", 
                                         "BB2"=           "Neotec Beggs & Brill (Rough pipe)",
                                         "BBO"=           "Beggs & Brill Revised (bja)", 
                                         "BBOTD"=         "Beggs & Brill Taitel Dukler map (bja)",  
                                         "BBR"=           "Beggs & Brill Revised (bja) ",
                                         "BBRev1"=        "Neotec Beggs & Brill Revised",
                                         "BBrev2"=        "Neotec Beggs & Brill revised",
                                         "BBRTD"=         "Beggs & Brill rev Taitel Dukler map (bja)",
                                         "BJA"=           "Baker Jardine Revised (bja)", 
                                         "BP1"=           "Segregated flow GRE Mech Model BP",
                                         "BP2"=           "Segregated flow GRE Mech Model BP",
                                         "DKAGAD"=        "Dukler, AGA & Flanagan (bja)",
                                         "DKAGAF"=        "Dukler, AGA & Flanagan [Eaton holdup] (bja) ",
                                         "EatonOli1"=     "Neotec Eaton & Oliemans", 
                                         "HughDuk1"=      "Neotec Hughmark Dukler", 
                                         "LEDA2P1.2e-2"=  "LedaFlow PM 2P (v.1.2)", 
                                         "LEDA3P1.2e-2"=  "LedaFlow PM 3P (v.1.2)",
                                         "LOCKMAR"=       "Lockhart & Martineli (bja)", 
                                         "LOCKMARTD"=     "Lockhart & Martineli, Taitel Dukler Map (bja)", 
                                         "MB"=            "Mukherjee & Brill (bja) ", 
                                         "NOSLIP"=        "No slip assumption (bja)", 
                                         "olga_2p_627"=   "OLGAS 2000 V.6.2.7 2-Phase",
                                         "olga_2p_50"=    "OLGAS 2000 V.5.0.2 2-Phase", 
                                         "olga_2p_53"=    "OLGAS 2000 V.5.3.3 2-Phase", 
                                         "olga_2p_532"=   "OLGAS 2000 V.5.3.2 2-Phase",
                                         "olga_3p_50"=    "OLGAS 2000 V.5.0.2 3-Phase",
                                         "olga_3p_53"=    "OLGAS 2000 V.5.3.3 3-Phase", 
                                         "olga_3p_532"=   "OLGAS 2000 V.5.3.2 3-Phase",
                                         "olga_3p_627"=   "OLGAS 2000 V.6.2.7 3-Phase",
                                         "olga_2p_72"=    "OLGAS 2000 v.7.2 2-P",
                                         "olga_3p_72"=    "OLGAS 2000 v.7.2 3-P", 
                                         "OLIEMANS"=      "Oliemans (bja)",
                                         "Oliemanist1"=   "Neotec Oliemans Mechanistic ",
                                         "TBB"=           "Beggs and Brill (Tulsa)",
                                         "TDUK"=          "Dukler (Tulsa) ",
                                         "TMB"=           "Mukherjee & Brill (Tulsa)",
                                         "TU2P"=          "TUFFP Unified 2P (v.2007.1) ",
                                         "TUFFPU3P_e-2"=  "TUFFP Unified 3-Phase (v.2011.1) (override emulsion visc)",
                                         "TUFFPU3P_e-0"=  "TUFFP Unified 3-Phase (v.2011.1) (emul default)",
                                         "TUFFPU2P"=      "TUFFP Unified 2P (v.2011.1)",
                                         "XIAO"=          "Xiao (bja)",
                                         "Xiao1"=         "Neotec Xiao",
                                         "Xiao2"=         "Neotec Xiao modfilm" )
                                                                                                   

tools.mappingPIPESIMMethodCategories <- list(  "BB1"=           "a", 
                                               "BB2"=           "a",
                                               "BBO"=           "a", 
                                               "BBOTD"=         "a",  
                                               "BBR"=           "a",
                                               "BBRev1"=        "a",
                                               "BBRev2"=        "a",
                                               "BBRTD"=         "a",
                                               "BJA"=           "e", 
                                               "BP1"=           "e",
                                               "BP2"=           "e",
                                               "DKAGAD"=        "c",
                                               "DKAGAF"=        "c",
                                               "EatonOli1"=     "d", 
                                               "HughDuk1"=      "d", 
                                               "LEDA2P1"=       "h", 
                                               "LEDA3P1"=       "h",
                                               "LOCKMAR"=       "c", 
                                               "LOCKMARTD"=     "c", 
                                               "MB"=            "b", 
                                               "NOSLIP"=        "a", 
                                               "olga_2p_627"=   "h",
                                               "olga_2p_50"=    "h", 
                                               "olga_2p_53"=    "h", 
                                               "olga_2p_532"=   "h",
                                               "olga_3p_50"=    "h",
                                               "olga_3p_53"=    "h", 
                                               "olga_3p_532"=   "h",
                                               "olga_3p_627"=   "h",
                                               "olga_2p_72"=    "h",
                                               "olga_3p_72"=    "h", 
                                               "OLIEMANS"=      "f",
                                               "Oliemanist1"=   "f",
                                               "TBB"=           "a",
                                               "TDUK"=          "c",
                                               "TMB"=           "b",
                                               "TU2P"=          "f",
                                               "TUFFPU3P_e-2"=  "h",
                                               "TUFFPU3P_e-0"=  "h",
                                               "TUFFPU2P"=      "h",
                                               "XIAO"=          "g",
                                               "Xiao1"=         "g",
                                               "Xiao2"=         "g" )



# tools.defaultListBasicOutput <- list( paste( c( "Group", "NA", "Run", "NA", "ElevProf", "", "Pset", "NA", "FModel", "NA",
#                                           "Pin", "NA", "Uo", "NA", "FModel", "NA", "Pin", "NA", "Pout", "NA", "Tin", "NA",
#                                           "Tout", "NA", "PlossElev", "NA", "PlossFric", "NA", "LiqHoldup", "NA"), 
#                                        collapse="," ) )

## cleaning the extra labels
tools.defaultListBasicOutput <- list( paste( c( "NA", "NA", "", "NA", "NA",
                                                "NA", "NA", "NA","NA", "NA", "NA",
                                                "NA", "NA", "NA", "NA"), 
                                             collapse="," ) )

## helper function for flattening nested lists 
tools.flatten <- function(x) Reduce("c", x)


#######################################################################
ExtractBasicInforFromFileName <- function( file_name ) {
  ## parse the file name to obtain the group type
  ## the typical file name looks like this:
  ##   "FriggsStFergus-01-DetElev-AllFlowModels-Multiflash-Uo-0.39-Pup.sum"
  
  ## 'strsplit' returns a list of character vectors with all sub strings split
  vInfo <- strsplit(file_name, ".sum")[[1]][1] ## return the first element if more... 
  vInfo <- strsplit( x=vInfo, 
                     split=.Platform$file.sep, ## remove at the subdirectory 
                     fixed=TRUE)[[1]][2] ## slice and return only the second element

  vInfo <- strsplit(vInfo,"-") # save everything to reuse
  groupName <- vInfo[[1]][1]
  groupNumber <- as.integer( Position( function(x) x==groupName, names(tools.groupNames) ) )
  
  runNumber <- as.integer(vInfo[[1]][2])
  
  elevationProfileType <- strsplit( vInfo[[1]][3], "Elev" )[[1]]
  
  if( is.na( Position( function(x) x=="Pup", vInfo[[1]]) ) ) {
    pressSetType = "PDown"
  }
  else {
    pressSetType = "Pup"
  }
  
  Uo <- as.double(vInfo[[1]][7])
  
  eqGasMMSM3D <- tools.flowRatesEqGasMMSM3D[[groupNumber]][runNumber] 
  ExpExitP <-    tools.experimentalExitPressureBara[[groupNumber]][runNumber]
  ExpInletT <-   tools.experimentalInletTemperatureDegC[[groupNumber]][runNumber]
  ExpInletP <-   tools.experimentalInletPressureBara[[groupNumber]][runNumber]
  
  ## make a comma separated character string for csv file
#   paste(c( "Group", groupName, "Run", runNumber, "ElevProf", elevationProfileType, 
#            "Pset", pressSetType, "Uo", Uo, "eqGasMMSM3D", eqGasMMSM3D, "ExpExitP", 
#             ExpExitP,"ExpInletT", ExpInletT,
#             "ExpInletP", ExpInletP),
#         collapse=",")
  
  paste(c( groupName, runNumber, elevationProfileType, 
           pressSetType, Uo, eqGasMMSM3D, ExpExitP, ExpInletT, ExpInletP),
        collapse=",")
}

#######################################################################
ReadSpecData <- function( model_name, inletData_line, exitData_line ) {
## This function extracts specific data from a text line
## it assumes that the text labels do exist 
  
  inData <- ParseResultsFromSumFileLine(inletData_line)
  outData <- ParseResultsFromSumFileLine(exitData_line)
  
  inletTemp <- as.double(inData[ Position(function(x) x=="InletT",names(inData)) ])
  inletPress <- as.double(inData[ Position(function(x) x=="InletP",names(inData)) ])
  
  exitTemp <- as.double(outData[ Position(function(x) x=="ExitT",names(outData)) ])
  exitPress <- as.double(outData[ Position(function(x) x=="ExitP",names(outData)) ])
  PlossElev <- as.double(outData[ Position(function(x) x=="PlossElev",names(outData)) ])
  PlossFric <- as.double(outData[ Position(function(x) x=="PlossFric",names(outData)) ])
  LiqHoldup <- as.double(outData[ Position(function(x) x=="LiqHoldup",names(outData)) ])
  
  ## prepare data to return
#   paste( c("FModel", model_name, "InletP", inletPress, "InletT", inletTemp, "ExitP", exitPress,
#            "ExitT", exitTemp, "PlossElev", PlossElev, "PlossFric", PlossFric, "LiqHoldup", LiqHoldup), 
#          collapse=",")

  paste( c( model_name, inletPress,  inletTemp,  exitPress,
            exitTemp, PlossElev, PlossFric, LiqHoldup),
         collapse=",")
}


BatchProcessSUMFile <- function( file_path ) {
  ## parse one file at a time generating
  ## a list of csv ready entries for each model results
  
  atom_basic_info <- ExtractBasicInforFromFileName(file_path)
  
  list_of_model_result_specifics <- ReadFileForSpecifics( file_path )
  
  ## now they have to be assembled into a single list with
  ## all the results found in one file
  vector_basic_info <- rep(atom_basic_info, times=length(list_of_model_result_specifics))
  paste( vector_basic_info, list_of_model_result_specifics, sep=",")
}


#######################################################################
ReadFileForSpecifics <- function( file_path) {
  ## process file for specific information
  ## return a comma separated character vector
  
  library(stringr) ##necessary library for cleaner syntax in using regular expressions covers 95% of use cases
  
  linesRead <- readLines( normalizePath(path=file_path,
                                        winslash="/",
                                        mustWork=FALSE) )
  ## obtaining the model types:
  regexFilterForModelType <- "TYPE=([a-zA-Z0-9_-]+)"
  
  linesWithModel <- grep( "TYPE=", linesRead, value=TRUE)
  
  listOfModelsFound <- str_extract( linesWithModel, regexFilterForModelType) 
  
  rawModelList<-unlist(lapply(listOfModelsFound, 
                              (function(x) strsplit(x, "TYPE=",fixed=TRUE)[1] ) ))
  ## this gives a vector like [1] ""             "BP1"          ""             "TUFFPU3P_e-2"
  ## so remove all that is non empty:
  models <- Filter( function(x) x!="", rawModelList)
  
  ## obtaining the inlet data:
  inletData <- grep("^[ ]+([a-zA-Z][a-zA-Z0-9]+){1}[ ]+([-+]?[0-9]*[.][0-9][ ]+)+",
                    linesRead,
                    value=TRUE)
  
  ## obtaining the exit data:
  exitData <- grep("(case terminated)|(^[ ]+([-+]?[0-9]*[.][0-9][ ]+)+)",
                   linesRead,
                   value=TRUE)

  ## this filter has FALSE where the model produced a "case terminated"...
  filterForInletDataAndModels <- !sapply( X=exitData, 
                                          FUN=function(line) {grepl(pattern="case terminated",
                                                                    x=line)} )
  inletDataSuccessful <- inletData[filterForInletDataAndModels]
  exitDataSuccessful <- exitData[filterForInletDataAndModels]
  modelsSuccessful <- models[filterForInletDataAndModels]
  
  inletDataFailedCases <- inletData[!filterForInletDataAndModels]
  modelsFailedCases <- models[!filterForInletDataAndModels]
  
  ## the failed case line is useless for parsing so create an artificial one:
  failedCaseLine <- paste(c("                 ", "","","","",
                            "","","","","","","",""," "," "," "),collapse=" ")
    
  exitDataFailedCases <- rep(x=failedCaseLine,times=length(inletDataFailedCases))
  
  totalInlet <- c(inletDataSuccessful,inletDataFailedCases)
  totalExit <- c(exitDataSuccessful,exitDataFailedCases)
  totalModels <- c( modelsSuccessful, modelsFailedCases) 
  
  ## put together all data and watch for failed cases:
  collectedData <- tryCatch( {
    suppressWarnings(mapply(ReadSpecData, totalModels, totalInlet, totalExit ) )
    }, warning = function(wrn){
      if (length(exitData)<length(inletData) ) {
        regexFilterForFailedCases = "(case terminated)|(^[ ]+([-+]?[0-9]*[.][0-9][ ]+)+)"
        ## this vector will have "case terminated" as an indicator of failure
        newExitData <-grep( regexFilterForFailedCases,
                            linesRead,
                            value=TRUE)
        ## this filter has FALSE where the model produced a "case terminated"...
        filterForInletDataAndModels <- !sapply( X=newExitData, 
                                               FUN=function(line) {grepl(pattern="case terminated",
                                                                                    x=line)} )
        inletData <- inletData[filterForInletDataAndModels]
        exitData <- newExitData[filterForInletDataAndModels]
        models <- models[filterForInletDataAndModels]
        result_without_failed_cases<- mapply(ReadSpecData, models, inletData, exitData )
        return(result_without_failed_cases)
      }
      return(list(""))
    }
  )
}
