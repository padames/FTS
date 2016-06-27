# FTS
Frigg to St. Fergus runs

In order to execute the analysis code open RStudio or from an R comamnd line 
set the working directory to the place where the file Main.R is with "setwd(~/path-to-Main)",
where "path-to-Main" is where the project was copied to. All auxiliary files and subforlders
with ".sum" files should be in that path.

The subfolders GroupOne FriggStFergus, GroupTwo Friff to MCP01, and GroupThree MCP01 to St Fergus, contain
all the PIPESIM output files. Main.R and auxiliary scripts assume the .sum files are there.

All the results are generated automatically by the script provided the local installation has all the 
packages used in the scripts. The main ones are:

 - ggplot2
 - reshape2 
 - NbClust