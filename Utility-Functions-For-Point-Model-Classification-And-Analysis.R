# Utility-Functions-For-Point-Model-Classification-And-Analysis

#######################################################################
ExtractSumFileNames <- function( subdir_name ){
  ## auxiliary function that harvest the sum files 
  ## of the subdirectory passed as argument
  current_working_directory <- getwd()
  
  sumFiles <- tryCatch( {
    nwd <- setwd(normalizePath( subdir_name,
                                winslash="/",
                                mustWork=NA))
    sumFiles <- list.files( getwd(),
                            pattern=".*[.]sum",
                            recursive=TRUE)
  }, warning = function(wrn){
    setwd(current_working_directory)
    cat("No files returned due to: ", wrn)
    return(list(""))
  },  error = function(err){
    setwd(current_working_directory)
    cat("No files found due to: ", err)
    return(list(""))
  }
  )
  if( getwd() != current_working_directory ) {
    setwd(current_working_directory)
  }
  sumFiles
}

#######################################################################
WalkListOfFileNames <- function( list_of_file_names_rel_path ) {
  ## create a list of lines ready for csv file creation
  
  #   listOfBasicInfo <-  tools.flatten( lapply( list_of_file_names_rel_path,
  #                                              ExtractBasicInforFromFileName ) )
  
  listOfEntriesFound <- tools.flatten( lapply( list_of_file_names_rel_path,
                                               BatchProcessSUMFile  ) )
  
  #   mapply( function( x,y ) paste( c(x, y ), collapse=","), 
  #           listOfBasicInfo, 
  #           listOfSpecifics )
}

CreatePipesimResultsDataFrame <- function( ext="sum") {
  
  ## create a list of all sum files under local dir:
  listOfAllFilesInSubdirectoriesOfCurrentDirectory <- 
    MakeListOfFilesOfTypeXInSubdirectories( ext )
  
  if( length(listOfAllFilesInSubdirectoriesOfCurrentDirectory) > 0 ) {
    raw_csv_lines <- WalkListOfFileNames( listOfAllFilesInSubdirectoriesOfCurrentDirectory ) 
    csv_lines <- tools.flatten(raw_csv_lines)
    headers <- paste(c("Group","Run","ElevProf",
                       "Pset","Uo", 
                       "eqGasMMSM3D","ExpExitP", 
                       "ExpInletT", "ExpInletP",
                       "FModel","InletP",
                       "InletT", "ExitP","ExitT",
                       "PlossElev", "PlossFric",
                       "LiqHoldup"), collapse=",")
    csv_lines_with_headers<- c(headers,csv_lines)
  } else {
    cat("No files of extension: ", ext, " were found in any sudirectories." )
  }
  
  
  csv_file_connection<-file("pipesimresults.csv")
  writeLines(csv_lines_with_headers, csv_file_connection)
  close(csv_file_connection)
  # these match the previously defined 'headers'
  pipesim_cases_data_frame<-read.csv("pipesimresults.csv",
                                     header=TRUE,
                                     colClasses=c("factor","integer","factor",
                                                  "factor","numeric",
                                                  "numeric","numeric",
                                                  "numeric","numeric",
                                                  "factor","numeric",
                                                  "numeric","numeric","numeric",
                                                  "numeric","numeric",
                                                  "numeric"))
}


GenerateG9ClassificationBargg2Plot <- function (g9df, title)
{
  
  ## consult documentation at http://docs.ggplot2.org/0.9.2.1/theme.html
  black.bold.32.text <- element_text(face = "bold", color = "black", size = 32)
  black.bold.24.text <- element_text(face = "bold", color = "black", size = 14) #14 for HTML creation
  black.normal.15.text <- element_text(color = "black", size = 12) #12 for HTML
  
  # Automate the creation of the G9 bar plots for point model comparison
  gg2_plot_obj <- ( ggplot(g9df,aes(x=FModels,y=value, fill=variable)) 
                    + labs(title=title)
                    + ylab("Grade") + xlab("Point models")
                    + scale_y_continuous( limit=c(0,100),expand=c(0,0),breaks = c(0,10,20,30,40,50,60,70,80,90,100) ) 
                    + geom_bar(stat="identity")
                    + theme( text=element_text(size=12), panel.grid.minor = element_line(colour="white"),#18 for HTML
                             title=black.bold.24.text , 
                             axis.text = black.normal.15.text,
                             axis.text.x= element_text(angle = 90, vjust = 0.5, hjust=1), #http://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
                             legend.position = c(0.9, .75),
                             panel.grid.major.y= element_line(colour="black"),
                             panel.grid.minor.y= element_line(colour="gray"),
                             complete=FALSE ) )
  
  gg2_plot_obj <- gg2_plot_obj + scale_fill_brewer(palette = "Set1") 
  # gg2_plot_obj <- gg2_plot_obj + scale_fill_grey() 
  
}

GenerateTop10ClassificationBargg2Plot <- function( df)
{
  ## consult documentation at http://docs.ggplot2.org/0.9.2.1/theme.html
  black.bold.32.text <- element_text(face = "bold", color = "black", size = 32)
  black.bold.24.text <- element_text(face = "bold", color = "black", size = 14) #14 for HTML creation
  black.normal.15.text <- element_text(color = "black", size = 10) #12 for HTML
  
  # df has to have columns: FModels, G9, filter
  title <- "Top 10 point models by filter"
  gg2_plot_obj <- ( ggplot(df,aes(x=FModels,y=G9,fill=FModels)) 
                    + labs(title=title)
                    + ylab("Grade")
                    + scale_y_continuous( limits=c(0,100),expand=c(0.05,0),breaks = c(70,80,90,100) )
                    + coord_cartesian(ylim=c(70, 100)) #http://stackoverflow.com/q/25685185/1585486
                    + geom_bar(stat="identity")
                    + facet_wrap(~ filter)
                    + theme( text=element_text(size=12), 
                             panel.grid.minor = element_line(colour="white"),#18 for HTML
                             title=black.bold.24.text , 
                             axis.text = black.normal.15.text,
                             axis.text.x = element_text(size=8,angle = 90, vjust = 0.5, hjust=1), #http://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
                             axis.title.x = element_blank(),
                             legend.position = "none",
                             panel.grid.major.y = element_line(colour="black"),
                             panel.grid.minor.y = element_line(colour="gray"),
                             strip.text.x = element_text(face = "bold", color = "black", size = 12),
                             complete = FALSE )                    
                    )
}  


SaveGGplotObjToPNG <- function( gg2plotObj )
{
  # Create a file name with the name of the object to save
  file_name <- paste(deparse(substitute(gg2plotObj)),"png",sep=".")
  ggsave(file_name) # in working directory
}


CreateBarPlotfromG9DataFrame <- function(g9dfWithG9Column, title)
{
  # Prepare a data frame that has G9 and FModels Columns and create a gg2plot barplot with it
  
  g9dfWithG9Column$G9 <- NULL; g9dfWithG9Column1 <- melt(g9dfWithG9Column, id.var="FModels")
  
  g9dfWithG9Column_plot <- GenerateG9ClassificationBargg2Plot(g9dfWithG9Column1,title)
  
}

ReturnTop10NoFilter <- function(g9df)
{
  require(plyr)
  g9df_o<-arrange(g9df,desc(G9), FModels)
  top10G9 <- head(g9df_o$G9,n=10)
  top10PM<-head(g9df_o$FModels,n=10)
  data.frame(G9=top10G9,FModels=top10PM,cnt=rep(1,10))
}

ReturnTop10 <- function(g9df,filterName)
{
  require(plyr)
  g9df_o<-arrange(g9df,desc(G9), FModels)
  top10G9 <- head(g9df_o$G9,n=10)
  top10PM<-head(g9df_o$FModels,n=10)
  top10Filter <- rep(filterName,times=10)
  data.frame(G9=top10G9,FModels=top10PM,cnt=rep(1,10),filter=top10Filter)
}

