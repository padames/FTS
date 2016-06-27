## main executable script, last ran with this R version info. in the Windows OS:
# platform       x86_64-w64-mingw32          
# arch           x86_64                      
# os             mingw32                     
# system         x86_64, mingw32             
# status                                     
# major          3                           
# minor          2.3                         
# year           2015                        
# month          12                          
# day            10                          
# svn rev        69752                       
# language       R                           
# version.string R version 3.2.3 (2015-12-10)
# nickname       Wooden Christmas-Tree 

# And this RStudio version 0.99.892 - © 2009-2016 RStudio, Inc


#####################################################################
# file and path dependencies stated below, get those files first
#####################################################################
source('Tools-For-Parsing-FriggsStFergus-Pipesim-Sum-Files.R')
source('MakeListOfFilesOfTypeXInSubdirectories.R')
source("ComputeErrorsAndVariancesFromPIPESIMResultsDataFfame.R")
source("Utility-Functions-For-Point-Model-Classification-And-Analysis.R")

#######################################################################
######################  M A I N   #####################################
#######################################################################

library(ggplot2) # for making nice plots using a grammar of graphics
library(reshape2) # to shape data frames from long to short form and vice versa 
library(NbClust) # to determine recommended number of clusters
 
ps.df <- CreatePipesimResultsDataFrame()
df <- ComputeErrorsAndVariancesFromPIPESIMResultsDataFfame( ps.df )

df2P <- RemoveThreePhaseModels( df )


# Create the a priori categories:
# first create a lookup mapping from flow point model to empirical category
lookup<-unlist(tools.mappingPIPESIMMethodCategories)
# apply it to the column (variable) of point models from the results matrix (data frame)
# this is an example of character matching used for subsetting (a very powerful R idiom)
Type <- lookup[df2P$FModel]
# now put it into the data frame as factors:
df2P$Type <- as.factor(unname(Type))


#######################################################################
#################### Clustering analysis ####################
#######################################################################

# clustering analysis is performed to identify cohesive subgroups in a dataset 

index_df <- ComputeIndexesDataFrame(df2P)

##### Hierarchical clustering methods #####
# eliminate the non-data column of model names
index_df$FModels<-NULL
# compute distance matrix:
d <- dist(index_df)
# compute the hierarchical clusters
fit.averageLink <- hclust(d,method = "average")
fit.singleLink <- hclust(d,method = "single")
fit.completeLink <- hclust(d,method = "complete")
fit.centroid <- hclust(d,method="centroid")
fit.wardD <- hclust(d, method="ward.D")
fit.wardD2 <- hclust(d, method="ward.D2")
fit.mcquitty <- hclust(d, method = "mcquitty")
fit.median <- hclust(d, method= 'median')

plot(fit.averageLink, hang=-1, cex=.8, main = "Average Link")
plot(fit.singleLink, hang=-1, cex=.8, main = "Single Link")
plot(fit.completeLink, hang=-1, cex=.8, main = "Complete Link")
plot(fit.centroid, hang=-1, cex=.8, main = "Centroid") 
plot(fit.wardD, hang=-1, cex=.8, main = "Ward D")
plot(fit.wardD2, hang=-1, cex=.8, main = "Ward D2")
plot(fit.mcquitty, hang=-1, cex=.8, main = "WPGMA")
plot(fit.median, hang=-1, cex=.8, main = "Median")

# filter best result from grade analysis:
dfPDownDet<-df2P[df2P$Pset=="PDown" & df2P$ElevProf=="Det",]
index_PDownDet <- ComputeIndexesDataFrame(dfPDownDet)
index_PDownDet$FModels <-NULL

d2 <- dist(index_PDownDet)

# things you can see with pure "Euclidean" distances
# the OLGAS implementations arevary slightly among themselves with the 72 farther from the 50 series than the 627
olgas_distances_matrix<-round(as.matrix(d2)[21:25,21:25],3)
# > olgas_distances_matrix
#             olga_2p_50 olga_2p_53 olga_2p_532 olga_2p_627 olga_2p_72
# olga_2p_50       0.000      0.000       0.000       0.057      0.046
# olga_2p_53       0.000      0.000       0.000       0.057      0.046
# olga_2p_532      0.000      0.000       0.000       0.057      0.046
# olga_2p_627      0.057      0.057       0.057       0.000      0.012
# olga_2p_72       0.046      0.046       0.046       0.012      0.000

# the BB family is all separated when applied to this data set
BB_distance_matrix <- round(as.matrix(d2)[0:8,0:8],3)
# > BB_distance_matrix 
#          BB1   BB2   BBO BBOTD   BBR BBRev1 BBRev2 BBRTD
# BB1    0.000 0.957 0.165 0.089 0.430  0.812  1.015 0.491
# BB2    0.957 0.000 0.879 0.939 0.803  1.274  0.856 0.844
# BBO    0.165 0.879 0.000 0.086 0.305  0.945  0.934 0.364
# BBOTD  0.089 0.939 0.086 0.000 0.368  0.889  0.982 0.425
# BBR    0.430 0.803 0.305 0.368 0.000  1.081  0.661 0.122
# BBRev1 0.812 1.274 0.945 0.889 1.081  0.000  1.321 1.143
# BBRev2 1.015 0.856 0.934 0.982 0.661  1.321  0.000 0.646
# BBRTD  0.491 0.844 0.364 0.425 0.122  1.143  0.646 0.000

# round(as.matrix(d2)[14:25,14:25],3)
#             EatonOli1 HughDuk1 LEDA2P1 LOCKMAR LOCKMARTD    MB NOSLIP olga_2p_50 olga_2p_53 olga_2p_532
# EatonOli1       0.000    0.985   0.139   0.204     0.204 0.379  0.204      0.111      0.111       0.111
# HughDuk1        0.985    0.000   0.937   0.870     0.870 0.999  0.870      0.956      0.956       0.956
# LEDA2P1         0.139    0.937   0.000   0.098     0.098 0.433  0.098      
# LOCKMAR         0.204    0.870   0.098   0.000     0.000 0.487  0.000      0.136      0.136       0.136
# LOCKMARTD       0.204    0.870   0.098   0.000     0.000 0.487  0.000      0.136      0.136       0.136
# MB              0.379    0.999   0.433   0.487     0.487 0.000  0.487      0.395      0.395       0.395
# NOSLIP          0.204    0.870   0.098   0.000     0.000 0.487  0.000      0.136      0.136       0.136
# olga_2p_50      0.111    0.956   0.051   0.136     0.136 0.395  0.136      0.000      0.000       0.000
# olga_2p_53      0.111    0.956   0.051   0.136     0.136 0.395  0.136      0.000      0.000       0.000
# olga_2p_532     0.111    0.956   0.051   0.136     0.136 0.395  0.136      0.000      0.000       0.000
# olga_2p_627     0.101    0.976   0.107   0.183     0.183 0.358  0.183      0.057      0.057       0.057
# olga_2p_72      0.101    0.972   0.096   0.173     0.173 0.364  0.173      0.046      0.046       0.046
# olga_2p_627 olga_2p_72
# EatonOli1         0.101      0.101
# HughDuk1          0.976      0.972
# LEDA2P1           0.107      0.096
# LOCKMAR           0.183      0.173
# LOCKMARTD         0.183      0.173
# MB                0.358      0.364
# NOSLIP            0.183      0.173
# olga_2p_50        0.057      0.046
# olga_2p_53        0.057      0.046
# olga_2p_532       0.057      0.046
# olga_2p_627       0.000      0.012
# olga_2p_72        0.012      0.000


# compute the hierarchical clusters
fit.averageLink2 <- hclust(d2,method = "average")
fit.singleLink2 <- hclust(d2,method = "single")
fit.completeLink2 <- hclust(d2,method = "complete")
fit.centroid2 <- hclust(d2,method="centroid")
fit.wardD2 <- hclust(d2, method="ward.D")
fit.wardD22 <- hclust(d2, method="ward.D2")
fit.mcquitty2 <- hclust(d2, method = "mcquitty")
fit.median2 <- hclust(d2, method= 'median')

# plot dendograms, read from bottom up
# first two closest clusters are joined
# height dimension indicates the criteron value for distance between 
# two clusters dependin on the "method" selected
# Note: good for similarity analysis, not so good for actual
# (hopefully meaningful) group creation
plot(fit.averageLink2, hang=-1, cex=.8, main = "Pressure set downstream, detailed elevation, Average Link")
plot(fit.singleLink2, hang=-1, cex=.8, main = "Pressure set downstream, detailed elevation, Single Link")
plot(fit.completeLink2, hang=-1, cex=.8, main = "Pressure set downstream, detailed elevation, Complete Link")
plot(fit.centroid2, hang=-1, cex=.8, main = "Pressure set downstream, detailed elevation, Centroid") 
plot(fit.wardD2, hang=-1, cex=.8, main = "Pressure set downstream, detailed elevation, Ward D")
plot(fit.wardD22, hang=-1, cex=.8, main = "Pressure set downstream, detailed elevation, Ward D2")
plot(fit.mcquitty2, hang=-1, cex=.8, main = "Pressure set downstream, detailed elevation, WPGMA")
plot(fit.median2, hang=-1, cex=.8, main = "Pressure set downstream, detailed elevation, Median")

png("dendo_PDownDet_centroid.png")
plot(fit.centroid2, hang=-1, cex=.8,
     main = "Pressure set downstream, detailed elevation, Centroid",
     xlab="", ylab="Height")
# the following cuts the three at k=3 clusters and draws 3 rectangles around those clusters
rect.hclust(fit.centroid2, k=3) 
dev.off()

pdf("dendo_PDownDet_avglink.pdf")
plot(fit.averageLink2, hang=-1, cex=.8,
     main = "Pressure set downstream, detailed elevation, Average Link",
     xlab="", ylab="Height")
rect.hclust(fit.averageLink2, k=3)
dev.off()


#######################################################
## Find out the composition of the clusters
#######################################################
# get a character vector with the names of the methods in the clusters
FM <- names(cutree(fit.averageLink2, k=3))
# make a data frame
dfFM <- data.frame(FModels=FM, cluster=unname(cutree(fit.averageLink2,k=3)),stringsAsFactors=FALSE)
# make a vector of types for each method
Type <- lookup[dfFM$FModels]
# add it to the data frame
dfFM$Type <- unname(Type)
# Contingency table to see category of models by cluster
table(dfFM$cluster,dfFM$Type)
# produces this output
#   a b c d e f g h
# 1 8 0 0 1 0 0 0 0
# 2 1 0 3 0 0 0 0 0
# 3 1 2 2 1 3 3 3 7

########################################################
## Get the grade of the clusters
########################################################
# get the relative performace by subsetting the models in each cluster
RP_cluster1 <- rowSums(index_PDownDet[dfFM$FModels[dfFM$cluster==1],])
RP_cluster2 <- rowSums(index_PDownDet[dfFM$FModels[dfFM$cluster==2],])
RP_cluster3 <- rowSums(index_PDownDet[dfFM$FModels[dfFM$cluster==3],])

# Function 'ComputeIndexesDataFrame' uses weighted indices which give Max G9=7.3
G9_cluster1 <- mean(1-RP_cluster1/7.3)*100
G9_cluster2 <- mean(1-RP_cluster2/7.3)*100
G9_cluster3 <- mean(1-RP_cluster3/7.3)*100
# Create atomic vector with grades
c(G9_cluster1,G9_cluster2,G9_cluster3)


# To determine the number of clusters to use the NbCluster package comes in handy:
set.seed(932637)
devAskNewPage(ask =FALSE) # activates prompt to see next plot
nc_centroid <- NbClust(index_PDownDet, distance = 'euclidean', min.nc = 2,max.nc = 5, method = "centroid")
# produces the plots and text output:
# ** : The Hubert index is a graphical method of determining the number of clusters.
# In the plot of Hubert index, we seek a significant knee that corresponds to a 
# significant increase of the value of the measure i.e the significant peak in Hubert
# index second differences plot. 
# 
# Hit <Return> to see next plot: 
#   *** : The D index is a graphical method of determining the number of clusters. 
# In the plot of D index, we seek a significant knee (the significant peak in Dindex
#                                                     second differences plot) that corresponds to a significant increase of the value of
# the measure. 
# 
# ******************************************************************* 
#   * Among all indices:                                                
#   * 5 proposed 2 as the best number of clusters 
# * 7 proposed 3 as the best number of clusters 
# * 4 proposed 4 as the best number of clusters 
# * 7 proposed 5 as the best number of clusters 
# 
# ***** Conclusion *****                            
#   
#   * According to the majority rule, the best number of clusters is  3 
# 
# 
# *******************************************************************
devAskNewPage(ask =FALSE) # get rid of prompt asking to display next plot

table(nc_centroid$Best.nc[1,])
# produces
# 0 1 2 3 4 5 
# 2 1 5 7 4 7 

# which can be read as 1 criteria favors 1 cluster, 5 criteria favors 2 clusters,
# 7 criteria favors 3 clusters (top row), so on

set.seed(932637)
nc_kmeans <- NbClust(index_PDownDet, distance = 'euclidean', min.nc = 2,max.nc = 5, method = "kmeans")
table(nc_kmeans$Best.nc[1,])

set.seed(932637)
fit.km <- kmeans(index_PDownDet, 3, nstart=25)
fit.km$size
colSums(fit.km$centers)
# e        re        ae       are        ve       vre       vae      vare       nsc 
# 1.5610906 1.6221988 1.7596759 1.5847271 0.7928268 0.9910265 0.9335123 0.9417739 0.2502525 
rowSums(fit.km$centers)
# 1        2        3 
# 5.064787 1.256163 4.116135 
RP<-rowSums(fit.km$centers)
1-RP/9
# 1         2         3 
# 0.4372459 0.8604264 0.5426517 
(1-RP/9)*100
# 1        2        3 
# 43.72459 86.04264 54.26517 


#################################################################
# USe Partiion around medoids PAM
# less sensitive to outliers and a nice cluster plotting routine
#################################################################
library(cluster)
set.seed(932637)
fit.pam <- pam(index_PDownDet,k = 3, metric = "euclidean",stand = TRUE)
fit.pam$medoids
jpeg("pam_bivariate_clusters.jpg")
clusplot(fit.pam,main="Bivariate cluster analysys from PAM")
dev.off()

RP_pam<-rowSums(fit.pam$medoids)
G9_medoids <- (1-RP_pam/9)*100

# BBO         olga_2p_532 DKAGAD 
# 51.34781    84.08969    40.42908 



#######################################################################
#################### Grade Performance analysis ####################
#######################################################################

# compute grades for all G-L point models:
g9_all<- ComputeGrade9(df)
g9_2P<-ComputeGrade9(df2P)

## separate the data by pressure set point
dfPSetDown<-df2P[df2P$Pset=="PDown",]
dfPSetUp<-df2P[df2P$Pset=="Pup",]

## separate the data by elevation profile type:
dfCoarse<-df2P[df2P$ElevProf=="Coarse",]
dfDetail<-df2P[df2P$ElevProf=="Det",]

## Separate by Downstream and Elevation profile type:
dfPDownCoarse<-df2P[df2P$Pset=="PDown" & df2P$ElevProf=="Coarse",]
dfPDownDetail<-df2P[df2P$Pset=="PDown" & df2P$ElevProf=="Det",]
dfUpCoarse<-df2P[df2P$Pset=="Pup"& df2P$ElevProf=="Coarse",]
dfUpDetail<-df2P[df2P$Pset=="Pup"& df2P$ElevProf=="Det",]


## To do holdup comparisons the standar bar plots are not good because there is insufficient 
## data points. Instead the values reported by selected models was plotted and compared 
## with the measured value.
## Filter Group 1 Runs 6 and 7 for holdup comparisons: R6 ->360 to 475 m3; R7 -> 630 m3
dfG1R6R7<-df2P[df2P$Group=="FriggsStFergus" & df2P$Run%in% c(6,7),]

G1R6ExpLiqHoldup <- rep(as.double(475),times=length( (dfG1R6R7[dfG1R6R7$Run==6,])[[1]] ) )
G1R7ExpLiqHoldup <- rep(as.double(630),times=length( (dfG1R6R7[dfG1R6R7$Run==7,])[[1]] ) )

dfG1R6R7$ExpLiqHoldup<-c(G1R6ExpLiqHoldup,G1R7ExpLiqHoldup)

dfG1R6R7 <- ComputeLiqHoldupErrorsAndVariancesFromPIPESIMResultsDataFfame( dfG1R6R7 ) 

## filter out selected mechanistic models and some correlations for ilustration: 
dfG1R6R7_SelectedFModels <- dfG1R6R7[as.character(dfG1R6R7$FModel) %in%c("XIAO",
                                                                         "Xiao1",
                                                                         "Xiao2",
                                                                         "olga_2p_72",
                                                                         "olga_2p_627",
                                                                         "LEDA2P1",
                                                                         "BP1",
                                                                         "BP2",
                                                                         "BJA",
                                                                         "Oliemanist1",
                                                                         "NOSLIP",
                                                                         "TUFFPU2P",
                                                                         "TMB",
                                                                         "BB1",
                                                                         "BBR",
                                                                         "MB",
                                                                         "EatonOli1"), ] 


df6<-dfG1R6R7_SelectedFModels[dfG1R6R7_SelectedFModels$Run==6 & dfG1R6R7_SelectedFModels$Pset=="PDown" & dfG1R6R7_SelectedFModels$ElevProf=="Det",]
df7<-dfG1R6R7_SelectedFModels[dfG1R6R7_SelectedFModels$Run==7 & dfG1R6R7_SelectedFModels$Pset=="PDown" & dfG1R6R7_SelectedFModels$ElevProf=="Det",]

df6_o <- transform(df6,FModel=reorder(FModel, LiqHoldup))
df7_o <- transform(df7,FModel=reorder(FModel, LiqHoldup))


LiqHold6_plot <- ( ggplot(data=df6_o, aes(x=FModel, y=LiqHoldup))
                   + labs(title="Calculated holdup (m3) vs. measured at [360,475] m3 (FriggStFergus06)")
                   + geom_bar(position="stack", stat="identity",alpha=0.75)
                   + scale_y_continuous( limit=c(0,4500),expand=c(0,0),breaks = c(0,500,1000,2000,3000,4000) ) 
                   + ylab("") + xlab("")
                   + geom_hline(aes(yintercept=360), size=1,colour="black", linetype="solid")
                   + geom_hline(aes(yintercept=475), size=1,colour="black", linetype="solid")        
                   + coord_flip()
                   + theme(title=element_text(face = "bold", color = "black", size = 14),
                           axis.text.y=element_text(color="black",size=rel(2)),
                           axis.text.x=element_text(color="black",size=rel(2),angle=90),
                           panel.grid.major.x= element_line(colour="gray",linetype = 2),
                           panel.grid.minor.x= element_line(colour="gray",linetype = "dashed") ) )

SaveGGplotObjToPNG(LiqHold6_plot)

LiqHold7_plot <- ( ggplot(data=df7_o, aes(x=FModel, y=LiqHoldup) )
                   + labs(title="Calculated holdup (m3) vs. measured at 630m3 (FriggStFergus07)")
                   + geom_bar(position="stack", stat="identity", alpha=0.75)
                   + scale_y_continuous( limit=c(0,4500),expand=c(0,0),breaks = c(0,500,1000,2000,3000,4000) ) 
                   + ylab("Liquid inventory, m3") + xlab("")
                   + geom_hline(aes(yintercept=630), size=1,colour="black", linetype="solid")
                   + coord_flip()
                   + theme(title=element_text(face = "bold", color = "black", size = 14),
                           axis.text.y=element_text(color="black",size=rel(2)),
                           axis.text.x=element_text(color="black",size=rel(2),angle=90),
                           panel.grid.major.x= element_line(colour="gray",linetype = 2),
                           panel.grid.minor.x= element_line(colour="gray",linetype = "dashed") ) )

SaveGGplotObjToPNG(LiqHold7_plot)

print(LiqHold6_plot)
print(LiqHold7_plot)


g9_LiqInv <- ComputeGrade9LiqInv( dfG1R6R7 )

totalCasesForHoldup<-length(dfG1R6R7$LiqHoldup)

casesSolved <-  length(dfG1R6R7[!is.na(dfG1R6R7$LiqHoldup),]$LiqHoldup)
print(paste0("Cases solved for holdup: ", casesSolved," out of ",totalCasesForHoldup))


## Prepare the G9s:
g9_PDown <- ComputeGrade9(dfPSetDown)
g9_PUp <- ComputeGrade9(dfPSetUp)

g9_Coarse <- ComputeGrade9(dfCoarse)
g9_Detail <- ComputeGrade9(dfDetail)

g9_DC <- ComputeGrade9(dfPDownCoarse)
g9_DD <- ComputeGrade9(dfPDownDetail)
g9_UC <- ComputeGrade9(dfUpCoarse)
g9_UD <- ComputeGrade9(dfUpDetail)


## Plot G9 for all observations:

## we don't need G9 anymore because it is computed from the individual g_i:
g9_plot <- CreateBarPlotfromG9DataFrame(g9_2P, "G9 all models")

print(g9_plot)
SaveGGplotObjToPNG(g9_plot)

# Create PNGs for use in publications
g9_PDown_plot <- CreateBarPlotfromG9DataFrame(g9_PDown, "G9 pressure set downstream")
SaveGGplotObjToPNG(g9_PDown_plot)

g9_PUp_plot <- CreateBarPlotfromG9DataFrame(g9_PUp, "G9 pressure set upstream")
SaveGGplotObjToPNG(g9_PUp_plot)

## plot G9 for PDown and PUp:
print(g9_PUp_plot)
print(g9_PDown_plot)




## plot G9 results for coarse and detailed elevation profile
g9_Coarse_plot <- CreateBarPlotfromG9DataFrame( g9_Coarse, "G9 coarse elevation profile")
SaveGGplotObjToPNG(g9_Coarse_plot)

g9_Detail_plot <- CreateBarPlotfromG9DataFrame( g9_Detail, "G9 detailed elevation profile")
SaveGGplotObjToPNG(g9_Detail_plot)


print(g9_Coarse_plot)
print(g9_Detail_plot)


## plot G9 results for combinations of variables profile(Detailed/Coarse) and PSetLocation(PDown/PUp)  
g9_DC_Plot <- CreateBarPlotfromG9DataFrame(g9_DC,"G9 downstream P set point and coarse elevation")
SaveGGplotObjToPNG(g9_DC_Plot)

g9_DD_Plot <- CreateBarPlotfromG9DataFrame(g9_DD,"G9 downstream P set point and detailed elevation")
SaveGGplotObjToPNG(g9_DD_Plot)

g9_UC_Plot <- CreateBarPlotfromG9DataFrame(g9_UC,"G9 upstream P set point and coarse elevation")
SaveGGplotObjToPNG(g9_UC_Plot)

g9_UD_Plot <- CreateBarPlotfromG9DataFrame(g9_UD,"G9 upstream P set point and detailed elevation")
SaveGGplotObjToPNG(g9_UD_Plot)

print(g9_DC_Plot)
print(g9_DD_Plot)
print(g9_UC_Plot)
print(g9_UD_Plot)

##----------------------------------------------
## Comparing all results to get top 10 PM by Grade:
g9_Top10_2P_All <- ReturnTop10(g9_2P, "all")
g9_Top10_Coarse <- ReturnTop10(g9_Coarse, "coarse")
g9_Top10_Detail <- ReturnTop10(g9_Detail,"detail")
g9_Top10_PSetUp <- ReturnTop10(g9_PUp,"upstream")
g9_Top10_PSDown <- ReturnTop10(g9_PDown, "downstream")
g9_Top10_UpCors <- ReturnTop10(g9_UC,"up-coarse")
g9_Top10_UpDetl <- ReturnTop10(g9_UD,"up-detail")
g9_Top10_DwnCrs <- ReturnTop10(g9_DC,"down-coarse")
g9_Top10_DwnDet <- ReturnTop10(g9_DD,"down-detail")

mdf<-rbind.data.frame(g9_Top10_2P_All,
                      g9_Top10_Coarse,
                      g9_Top10_Detail,
                      g9_Top10_PSetUp,
                      g9_Top10_PSDown,
                      g9_Top10_UpCors,
                      g9_Top10_UpDetl,
                      g9_Top10_DwnCrs,
                      g9_Top10_DwnDet)
# > mdf
#          G9     FModels cnt      filter
# 1  88.38232 Oliemanist1   1         all
# 2  87.94749    TUFFPU2P   1         all
# 3  86.52804         TMB   1         all
# 4  86.08756          MB   1         all
# 5  84.66321   EatonOli1   1         all
top10Plot<-GenerateTop10ClassificationBargg2Plot(mdf)
print(top10Plot)

SaveGGplotObjToPNG(top10Plot)

mdf_m <- mdf
 # head(mdf_m)
#         G9     FModels cnt
# 1 90.57677 Oliemanist1   1
# 2 90.22408    TUFFPU2P   1
# 3 89.07274         TMB   1
# 4 88.71546          MB   1
# 5 87.56016   EatonOli1   1
# 6 86.69338     LEDA2P1   1

mdf_m$G9<-NULL
mdf_m$filter<-NULL

mc<-melt(mdf_m,id.vars = "FModels")
dc<-dcast(mc,formula = FModels ~ .,sum)
names(dc)<-c("FModels","Freq")

# dc
# FModels Freq
# 1  Oliemanist1    9
# 2     TUFFPU2P    9
# 3          TMB    9
# 4           MB    9
# 5    EatonOli1    4
# 6      LEDA2P1    4
# 7         XIAO    9
# 8        Xiao1    8
# 9        Xiao2    8
# 10         BJA    9
# 11         BP1    5
# 12         BP2    5
# 13 olga_2p_627    1
# 14  olga_2p_72    1


mdf_m2 <- mdf
mdf_m2$cnt<-NULL
mc<-melt(mdf_m2,id.vars = "FModels")
# > head(mc)
# FModels variable    value
# 1 Oliemanist1       G9 90.57677
# 2    TUFFPU2P       G9 90.22408
# 3         TMB       G9 89.07274
# 4          MB       G9 88.71546
# 5   EatonOli1       G9 87.56016
# 6     LEDA2P1       G9 86.69338
dc2<-dcast(mc,formula = FModels ~ .,mean)
names(dc2)<-c("FModels","Avg G9")
# > dc2
# FModels   Avg G9
# 1  Oliemanist1 90.90524
# 2     TUFFPU2P 91.06437
# 3          TMB 89.01124
# 4           MB 89.68280
# 5    EatonOli1 89.09737
# 6      LEDA2P1 89.03794
# 7         XIAO 87.18363
# 8        Xiao1 87.23361
# 9        Xiao2 86.99661
# 10         BJA 87.03658
# 11         BP1 88.11589
# 12         BP2 88.11589
# 13 olga_2p_627 89.14203
# 14  olga_2p_72 88.82127


#Plot all Top10s:
df_Top10_G9s <- data.frame(FullMatrix=g9_Top10_2P_All$G9,
                           Coarse=g9_Top10_Coarse$G9,
                           Detail=g9_Top10_Detail$G9,
                           PUp=g9_Top10_PSetUp$G9,
                           PDown=g9_Top10_PSDown$G9,
                           PDownCoarse=g9_Top10_DwnCrs$G9,
                           PDownDetail=g9_Top10_DwnDet$G9,
                           PUpCoarse=g9_Top10_UpCors$G9,
                           PUpDetail=g9_Top10_UpDetl$G9)

# boxplot(df_Top10_G9s, main="Top 10 point models for various filters",
#         xlab="Filter", ylab="Grade 9")
# boxplot(df_Top10_G9s, las = 2)
# 

mdf<-melt(df_Top10_G9s)
names(mdf)<-c("Filter","G9")


black.bold.16.text <- element_text(face = "bold", color = "black", size = 16)
black.normal.15.text <- element_text(color = "black", size = 15)
G9Top10ByFilter <- (qplot(Filter, G9,data=mdf)
                    + labs(title="Quantiles for grades of top 10 point models by filter")
                    + ylab("G9") + xlab("Filter")
                    + geom_boxplot(outlier.colour = "red", outlier.size = 4, outlier.shape = 13)
                    + geom_jitter()
                    + coord_flip()
                    + theme( text=element_text(size=16), panel.grid.minor = element_line(colour="white"),
                             title=black.bold.16.text,
                             axis.text = black.normal.15.text,
                             axis.text.x= element_text(angle = 90, vjust = 0.5, hjust=1), #http://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
                             panel.grid.major.y= element_line(colour="gray"),
                             panel.grid.minor.y= element_line(colour="gray"),
                             complete=FALSE )
                    + scale_x_discrete(limits=rev(levels(mdf$Filter)))
                    + scale_y_continuous( limit=c(80,95),expand=c(0,0),breaks = c(80,85,90,95) )
                    + scale_fill_grey())

SaveGGplotObjToPNG(G9Top10ByFilter)
print(G9Top10ByFilter)

# library(plyr)
# dc_o <- arrange(dc,desc(Freq),FModels)
#           FModels Freq
# 1          BJA    9
# 2           MB    9
# 3  Oliemanist1    9
# 4          TMB    9
# 5     TUFFPU2P    9
# 6         XIAO    9
# 7        Xiao1    8
# 8        Xiao2    8
# 9          BP1    5
# 10         BP2    5
# 11   EatonOli1    4
# 12     LEDA2P1    4
# 13 olga_2p_627    1
# 14  olga_2p_72    1

## plot liquid inventory g9s:
g9_LiqInv_plot <- CreateBarPlotfromG9DataFrame(g9_LiqInv, "G9 liquid inventory")

SaveGGplotObjToPNG(g9_LiqInv_plot)

print(g9_LiqInv_plot)

#--------------------------------------------------------------
# Plotting errors in different ways:
# Note: some plots take a few seconds to display
#--------------------------------------------------------------


# g<- ggplot(df2P, aes(ExpDP,e.i))
# g<-g+geom_point()+facet_wrap(FModel ~ Pset,nrow=9)+geom_smooth(method="loess")+theme_bw()
# print(g)

# require(graphics)
# pairs(df2P, 
#       panel = panel.smooth, 
#       main = "pipesim case results")

png("ErrorsCorrelated.png",
    width = 650,
    height = 650,
    units = "px",
    pointsize = 8,
    bg = "white",
    res = 160,
    family = "",
    restoreConsole = TRUE,
    type = c("cairo-png"),
    antialias="default")
dfErrors<- data.frame(error=df2P$e.i, absolute.error=df2P$abse.i,relative.error=df2P$rele.i,absrel.error=df2P$absrele.i)
pairs(dfErrors,
      panel = panel.smooth,
      main = "Friggs-St Fergus errors")
dev.off()
