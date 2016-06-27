

ParseResultsFromSumFileLine <- function( lineToParse ){
  ## lineToParse: this is a character array with a predefined format
  ## Returns:     the values extracted as an array of pairs
  ##              empty array if pattern no matched.
  
  ## Precondition:  The lines has been identified as a good candidate to have
  ##                the sought information in the format predefined
  ##                The units are not checked. User should do so manually.

  ## Alternative way of extracting all members:
  #splitArray <- strsplit(lineToParse,"[ ]+",fixed=FALSE)

  ## Prefered way:
  library(stringr) ##necessary library for cleaner syntax in using regular expressions covers 95% of use cases
  regexFilterForNumbers <- "([-+]?[0-9]*[.][0-9]*)"
  regexFilterForNames <- "([a-zA-Z]{1}[a-zA-Z0-9/]+)"
  ## apply str_extract_all and return the first vector of the list of character vectors
  listOfNumbersFound <- str_extract_all( lineToParse, regexFilterForNumbers)[[1]] 
  listOfCharactersFound <-  str_extract_all( lineToParse, regexFilterForNames )[[1]]
  ## concatenate both lists:
  listofAllMatches <- c(listOfNumbersFound , listOfCharactersFound ) ## returns only the first line 
  ## Now apply business rules to extract information
  ## The .sum files from PIPESIM classic have these headers:
  ##         Water Liquid     Free   Pres.  Temp.   Pressure Losses   Mixt.  Liquid  Liquid  Slug   Flow
  ##           Cut   Flow        Gas                     (Bar)           Vel.  Holdup  Holdup Number Pattern
  ##           (%)   (m3/d) (mmsm3d)  (Bara)   (C)  Elev.  Frn. Total  (m/s)   frn.    (m3)  (PI-SS)
  ## In the first empty spots the group label may appear as a set of characters: one word made up of
  ## letters and numbers.
  posOfResultVars <- list("watercut"=1,"liqFlow"=2,"freegas"=3,"press"=4,"temp"=5,
                            "elevdeltap"=6,"fricdeltap"=7,"totaldeltap"=8,"mixvel"=9,
                            "liqholdupfrac"=10,"liqholdupvol"=11,"slugNumber"=12)
  ## create the list to be returned: l{(k,v)}
  dataParsed <-list()
  ## check if there is a label to know what to do with the flow pattern characters:
  regexToCheckIfThereIsALabelAtTheStart <- "^[ ]+[a-zA-Z]{1}[a-zA-Z0-9/]+"
  isThereAGroupLabel <- (str_count( lineToParse, regexToCheckIfThereIsALabelAtTheStart )>0)
  if(isThereAGroupLabel) {
    ## Get all but the first label as the flow pattern
    if (length(listOfCharactersFound)>1){
      flowPattern <- paste(listOfCharactersFound[(2:length(listOfCharactersFound))],collapse="-")
    }
    else{
      flowPattern <- "unidentified" 
    }
  }
  else {
    ## Form the flow pattern with all the characters available
    flowPattern <- paste(listOfCharactersFound,collapse="-")
  }
  ## store the flow pattern 
  dataParsed <- c(dataParsed, list("Exit flow Pattern" =flowPattern))
  
  ## parse and store the pressure and temperature:
  if (isThereAGroupLabel) {
    dataParsed <- c(dataParsed, list("InletP" = as.double(listOfNumbersFound[posOfResultVars$'press']),
                                     "InletT" = as.double(listOfNumbersFound[posOfResultVars$'temp'])))
  }
  else {
    dataParsed <- tryCatch({
      c(dataParsed, list("ExitP" = as.double(listOfNumbersFound[posOfResultVars$'press']),
                         "ExitT" = as.double(listOfNumbersFound[posOfResultVars$'temp']),
                         "PlossElev" = as.double(listOfNumbersFound[posOfResultVars$'elevdeltap']),
                         "PlossFric" = as.double(listOfNumbersFound[posOfResultVars$'fricdeltap']),
                         "PlossTotal" = as.double(listOfNumbersFound[posOfResultVars$'totaldeltap']),
                         "LiqHoldup"=as.double(listOfNumbersFound[posOfResultVars$'liqholdupvol'])))
    }, error = function(err){
      if (length(listOfNumbersFound)>listOfNumbersFound$'totaldeltap'){
        dataParsed <- c(dataParsed, list("ExitP" = as.double(listOfNumbersFound[posOfResultVars$'press']),
                                         "ExitT" = as.double(listOfNumbersFound[posOfResultVars$'temp']),
                                         "PlossElev" = as.double(listOfNumbersFound[posOfResultVars$'elevdeltap']),
                                         "PlossFric" = as.double(listOfNumbersFound[posOfResultVars$'fricdeltap']),
                                         "PlossTotal" = as.double(listOfNumbersFound[posOfResultVars$'totaldeltap'])))
      }
      
      return( dataParsed )
    } ) # end of trycatch function
  }
  
}