# Frigg Transportation System (FTS)

**Data analysys of the Frigg to St. Fergus simulations**

In order to execute the analysis code first copy it to
a path  __`path/to/Main`__, then set the R working directory to that
location with the **R** command `setwd("~/path/to/Main"")` in Linux or
`setwd("C:\path\to\main"")` in Windows.
This can be done from he Console panel in __RStudio__ or directly via 
an **R** command console window. 
 
The assumption of the driver script, *Main.R*, and its helpers, is that all necessary 
data files can be found under that path for the **R** session. 
In particular, there are three subfolders that contain the **PIPESIM** simulation 
results in files of extension _.sum_ corresponding to each of the experimental runs
and simulation settings. Details of how the simulations were set up are explained 
in detail in [BHR-2016-285](<https://www.onepetro.org/conference-paper/BHR-2016-285>).

The subfolders with the simulation results are:

  - GroupOne FriggStFergus
  - GroupTwo Frigg to MCP01
  - GroupThree MCP01 to St Fergus,
  
All the numerical and graphic results of the data analysis are generated automatically 
by executing the driver script *Main.R*.
To do this run the **R** command: `source("Main.R")`.
The only dependency is on the existence of the following
freely available **R** packages:

 - ggplot2
 - reshape2 
 - NbClust

If necessary, they can be installed via the **R** command: `install.packages("ggplot2","reshape2","NbClust")`

The data and scripts was all that was needed to generate the exploratory and final 
results, plots and tables of results in the conference paper [BHR-2016-285](<https://www.onepetro.org/conference-paper/BHR-2016-285>):
_A comparison of 35 gas-liquid point models in a low-liquid loading subsea gas line: A technology classification effort_.
Presented at the June 2016 10th North American Conference on Multiphase Technology in Banff, AB, Canada.
