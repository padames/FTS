## compute the errors and variances necessary for the grade

RemoveThreePhaseModels <- function( df ) {
  ## filter all three phase flow models and return filtered data frame
  df2P<- df[df$FModel!="olga_3p_72" & df$FModel != "olga_3p_627"
            & df$FModel!="olga_3p_532" & df$FModel!="olga_3p_53"
            & df$FModel!="olga_3p_50" & df$FModel!="TUFFPU3P_e-2"
            & df$FModel!="TUFFPU3P_e0" & df$FModel!="LEDA3P1",]
  df2P$FModel <- as.factor(as.character(df2P$FModel))
  df2P
}


ComputeErrorsAndVariancesFromPIPESIMResultsDataFfame <- function( data_frame ) {
  ## return the data frame with the calculated variables as new columns at the end
  
  ## create the delta pressure experimental(observed) and calculated by model:
  data_frame$ExpDP=data_frame$ExpInletP-data_frame$ExpExitP
  data_frame$CalcDP=data_frame$InletP-data_frame$ExitP
  
  ## calculate the errors per observation i:
  data_frame$e.i=data_frame$CalcDP-data_frame$ExpDP
  data_frame$abse.i=abs(data_frame$CalcDP-data_frame$ExpDP)
  data_frame$rele.i=data_frame$e.i/data_frame$ExpDP
  data_frame$absrele.i=abs(data_frame$e.i/data_frame$ExpDP)
  
  data_frame
}

ComputeLiqHoldupErrorsAndVariancesFromPIPESIMResultsDataFfame <- function( df ) {
  
  ## create the delta liquid inventory in m3 between observed and calculated by model:
  df$LiqInv.ei<- ( df$ExpLiqHoldup - df$LiqHoldup )
  df$LiqInv.absei <- abs( df$LiqInv.ei )
  df$LiqInv.relei <- df$LiqHoldup / df$ExpLiqHoldup
  df$LiqInv.absrelei <- abs( df$LiqInv.relei )
  
  df
}


ComputeGrade9LiqInv <- function( data_frame ) {
  
  errorMeanByModel<- sapply( X= levels(data_frame$FModel),    
                             FUN= function(x) { mean(data_frame[data_frame$FModel==x,]$LiqInv.ei, 
                                                     na.rm=TRUE) })
  
  relativeErrorMeanByModel<- sapply( X= levels(data_frame$FModel),    
                                     FUN= function(x) { mean(data_frame[data_frame$FModel==x,]$LiqInv.relei, 
                                                             na.rm=TRUE) })
  
  absErrorMeanByModel <- sapply( X= levels(data_frame$FModel),    
                                 FUN= function(x) { mean(data_frame[data_frame$FModel==x,]$LiqInv.absei, 
                                                         na.rm=TRUE) })
  
  absRelativeErrorMeanByModel <- sapply( X= levels(data_frame$FModel),    
                                         FUN= function(x) { mean(data_frame[data_frame$FModel==x,]$LiqInv.absrelei, 
                                                                 na.rm=TRUE) })
  
  errorVarianceByModel<- sapply( X= levels(data_frame$FModel),    
                                 FUN= function(x) { var(data_frame[data_frame$FModel==x,]$LiqInv.ei, 
                                                        na.rm=TRUE) })
  
  relativeErrorVarianceByModel<- sapply( X= levels(data_frame$FModel),    
                                         FUN= function(x) { var(data_frame[data_frame$FModel==x,]$LiqInv.relei, 
                                                                na.rm=TRUE) })
  
  absErrorVarianceByModel <- sapply( X= levels(data_frame$FModel),    
                                     FUN= function(x) { var(data_frame[data_frame$FModel==x,]$LiqInv.absei, 
                                                            na.rm=TRUE) })
  
  absRelativeErrorVarianceByModel <- sapply( X= levels(data_frame$FModel),    
                                             FUN= function(x) { var(data_frame[data_frame$FModel==x,]$LiqInv.absrelei, 
                                                                    na.rm=TRUE) })
  
  errorMinByModel<- min(abs(errorMeanByModel),na.rm=TRUE)
  
  relativeErrorMinByModel<- min(abs(relativeErrorMeanByModel),na.rm=TRUE)
  
  absErrorMinByModel <- min(abs(absRelativeErrorMeanByModel), na.rm=TRUE)
  
  absRelativeErrorMinByModel <- min(abs(absRelativeErrorMeanByModel), na.rm=TRUE)
  
  errorMaxByModel<- max(abs(errorMeanByModel),na.rm=TRUE)
  
  relativeErrorMaxByModel<- max(abs(relativeErrorMeanByModel),na.rm=TRUE)
  
  absErrorMaxByModel <- max(abs(absErrorMeanByModel),na.rm=TRUE)
  
  absRelativeErrorMaxByModel <- max(abs(absRelativeErrorMeanByModel),na.rm=TRUE)
  
  filterForFailedCases <-sapply( X=levels(data_frame$FModel),
                                 FUN = function(x) { is.na(data_frame$LiqHoldup[data_frame$FModel==x])},
                                 simplify=FALSE) # to get a list with the model names each of as many sum files as read 
  
  numberOfSuccessfulCases <- sapply( X=levels(data_frame$FModel),
                                     FUN = function(x) { (length(data_frame$LiqHoldup[data_frame$FModel==x]) - 
                                                            length(data_frame$LiqHoldup[data_frame$FModel==x][filterForFailedCases[[x]] ] ) )})
  
  index_1 <- ( abs( errorMeanByModel ) - errorMinByModel ) / ( errorMaxByModel - errorMinByModel ) 
  index_2 <- ( abs( relativeErrorMeanByModel) - relativeErrorMinByModel ) / ( relativeErrorMaxByModel - relativeErrorMinByModel)
  index_3 <- ( abs( absErrorMeanByModel) - absErrorMinByModel) / ( absErrorMaxByModel - absErrorMinByModel )  
  index_4 <- ( abs( absRelativeErrorMeanByModel) - absRelativeErrorMinByModel ) / ( absRelativeErrorMaxByModel - absRelativeErrorMinByModel )
  index_5 <- ( errorVarianceByModel - min(errorVarianceByModel,na.rm=TRUE)) / ( max(errorVarianceByModel,na.rm=TRUE)-min(errorVarianceByModel,na.rm=TRUE))
  index_6 <- ( relativeErrorVarianceByModel - min(relativeErrorVarianceByModel,na.rm=TRUE)) / ( max(relativeErrorVarianceByModel,na.rm=TRUE)-min(relativeErrorVarianceByModel,na.rm=TRUE) )
  index_7 <- ( absErrorVarianceByModel - min( absErrorVarianceByModel,na.rm=TRUE)) / ( max(absErrorVarianceByModel,na.rm=TRUE)-min(absErrorVarianceByModel,na.rm=TRUE) )
  index_8 <- ( absRelativeErrorVarianceByModel - min( absRelativeErrorVarianceByModel,na.rm=TRUE)) / ( max(absRelativeErrorVarianceByModel,na.rm=TRUE)-min(absRelativeErrorVarianceByModel,na.rm=TRUE) )
  index_9 <- ( max( numberOfSuccessfulCases,na.rm=TRUE ) - numberOfSuccessfulCases ) / ( max(numberOfSuccessfulCases,na.rm=TRUE)-min(numberOfSuccessfulCases,na.rm=TRUE))
  
  rel_perf_index <- index_1 + index_2 + index_3 + index_4 + index_5 + index_6 + index_7 + index_8 + index_9
  
  ## Apply formula to compute grade contributions:
  ## G_(N,i) = 100 (I_i)(1/I-1/N)
  common_factor <- 100 * (1/rel_perf_index - 1 / 9.)
  grade_1 <- common_factor * index_1
  grade_2 <- common_factor * index_2
  grade_3 <- common_factor * index_3
  grade_4 <- common_factor * index_4
  grade_5 <- common_factor * index_5
  grade_6 <- common_factor * index_6
  grade_7 <- common_factor * index_7
  grade_8 <- common_factor * index_8
  grade_9 <- common_factor * index_9
  
  
  
  G9 <- ( 1 - rel_perf_index / 9.) * 100.
  
  #   data.frame("G9"=sapply(X=names(G9), FUN=function(x) (x=G9[[x]])), 
  df<- data.frame("G9"=G9,
#                   "rel_perf_index"=rel_perf_index,
                  "e"=grade_1,
                  "re"=grade_2, 
                  "ae"=grade_3,
                  "are"=grade_4,
                  "ve"=grade_5,
                  "vre"=grade_6,
                  "vae"=grade_7,
                  "vare"=grade_8,
                  "nsc"=grade_9)
  
  filterNAs <-!is.na(df$G9)
  dff<-df[filterNAs,] 
  FModels<-row.names(dff)
  dff<-cbind(dff,FModels)
  transform(dff,FModels=reorder(FModels, -G9))


#   ## we don't need G9 anymore because it is computed from the individual g_i:
#   dff$G9 <- NULL
# 
#   library(reshape2);
#   dff1 <- melt(dff, id.var="FModels")
#   dff1
}


ComputeGrade9 <- function( data_frame ) {
  ## return the vector of grade_9 per model in the data frame
  
  errorMeanByModel<- sapply( X= levels(data_frame$FModel),    
                             FUN= function(x) { mean(data_frame[data_frame$FModel==x,]$e.i, 
                                                     na.rm=TRUE) })
  
  relativeErrorMeanByModel<- sapply( X= levels(data_frame$FModel),    
                                     FUN= function(x) { mean(data_frame[data_frame$FModel==x,]$rele.i, 
                                                        na.rm=TRUE) })
  
  absErrorMeanByModel <- sapply( X= levels(data_frame$FModel),    
                                 FUN= function(x) { mean(data_frame[data_frame$FModel==x,]$abse.i, 
                                                         na.rm=TRUE) })
  
  absRelativeErrorMeanByModel <- sapply( X= levels(data_frame$FModel),    
                                 FUN= function(x) { mean(data_frame[data_frame$FModel==x,]$absrele.i, 
                                                         na.rm=TRUE) })

  errorVarianceByModel<- sapply( X= levels(data_frame$FModel),    
                             FUN= function(x) { var(data_frame[data_frame$FModel==x,]$e.i, 
                                                     na.rm=TRUE) })
  
  relativeErrorVarianceByModel<- sapply( X= levels(data_frame$FModel),    
                                     FUN= function(x) { var(data_frame[data_frame$FModel==x,]$rele.i, 
                                                             na.rm=TRUE) })
  
  absErrorVarianceByModel <- sapply( X= levels(data_frame$FModel),    
                                 FUN= function(x) { var(data_frame[data_frame$FModel==x,]$abse.i, 
                                                         na.rm=TRUE) })
  
  absRelativeErrorVarianceByModel <- sapply( X= levels(data_frame$FModel),    
                                         FUN= function(x) { var(data_frame[data_frame$FModel==x,]$absrele.i, 
                                                                 na.rm=TRUE) })
  
  errorMinByModel<- min(abs(errorMeanByModel),na.rm=TRUE)
  
  relativeErrorMinByModel<- min(abs(relativeErrorMeanByModel),na.rm=TRUE)
  
  absErrorMinByModel <- min(abs(absRelativeErrorMeanByModel), na.rm=TRUE)
  
  absRelativeErrorMinByModel <- min(abs(absRelativeErrorMeanByModel), na.rm=TRUE)
  
  errorMaxByModel<- max(abs(errorMeanByModel),na.rm=TRUE)
  
  relativeErrorMaxByModel<- max(abs(relativeErrorMeanByModel),na.rm=TRUE)
  
  absErrorMaxByModel <- max(abs(absErrorMeanByModel),na.rm=TRUE)
  
  absRelativeErrorMaxByModel <- max(abs(absRelativeErrorMeanByModel),na.rm=TRUE)
  
  filterForFailedCases <-sapply( X=levels(data_frame$FModel),
                                 FUN = function(x) { is.na(data_frame$LiqHoldup[data_frame$FModel==x])},
                                 simplify=FALSE) # to get a list with the model names each of as many sum files as read 
  
  numberOfSuccessfulCases <- sapply( X=levels(data_frame$FModel),
                                 FUN = function(x) { (length(data_frame$LiqHoldup[data_frame$FModel==x]) - 
                                                        length(data_frame$LiqHoldup[data_frame$FModel==x][filterForFailedCases[[x]] ] ) )})
  
  index_1 <- 0.9*( abs( errorMeanByModel ) - errorMinByModel ) / ( errorMaxByModel - errorMinByModel ) 
  index_2 <- 0.9*( abs( relativeErrorMeanByModel) - relativeErrorMinByModel ) / ( relativeErrorMaxByModel - relativeErrorMinByModel)
  index_3 <- 0.9*( abs( absErrorMeanByModel) - absErrorMinByModel) / ( absErrorMaxByModel - absErrorMinByModel )  
  index_4 <- 0.9*( abs( absRelativeErrorMeanByModel) - absRelativeErrorMinByModel ) / ( absRelativeErrorMaxByModel - absRelativeErrorMinByModel )
  index_5 <- 0.9*( errorVarianceByModel - min(errorVarianceByModel,na.rm=TRUE)) / ( max(errorVarianceByModel,na.rm=TRUE)-min(errorVarianceByModel,na.rm=TRUE))
  index_6 <- 0.9*( relativeErrorVarianceByModel - min(relativeErrorVarianceByModel,na.rm=TRUE)) / ( max(relativeErrorVarianceByModel,na.rm=TRUE)-min(relativeErrorVarianceByModel,na.rm=TRUE) )
  index_7 <- 0.9*( absErrorVarianceByModel - min( absErrorVarianceByModel,na.rm=TRUE)) / ( max(absErrorVarianceByModel,na.rm=TRUE)-min(absErrorVarianceByModel,na.rm=TRUE) )
  index_8 <- 0.9*( absRelativeErrorVarianceByModel - min( absRelativeErrorVarianceByModel,na.rm=TRUE)) / ( max(absRelativeErrorVarianceByModel,na.rm=TRUE)-min(absRelativeErrorVarianceByModel,na.rm=TRUE) )
  index_9 <- 0.1*( max( numberOfSuccessfulCases,na.rm=TRUE ) - numberOfSuccessfulCases ) / ( max(numberOfSuccessfulCases,na.rm=TRUE)-min(numberOfSuccessfulCases,na.rm=TRUE))
  
  rel_perf_index <- index_1 + index_2 + index_3 + index_4 + index_5 + index_6 + index_7 + index_8 + index_9
  
  # max relative performance with weighing: (0.9)(8)+0.1(1)=7.3
  # G9 <- (1-rel_perf_index/9) * 100
  G9 <- (1-rel_perf_index/7.3) * 100
  
  # Apply formula to compute grade contributions:
  # G_(N,i) = 100 (I_i)(1/I-1/N)

  g1_e <- G9 * index_1/rel_perf_index
  g2_re <- G9 * index_2/rel_perf_index
  g3_ae <- G9 * index_3/rel_perf_index
  g4_are <- G9 * index_4/rel_perf_index
  g5_sde <- G9 * index_5/rel_perf_index
  g6_sdre <- G9 * index_6/rel_perf_index
  g7_sdae <- G9 * index_7/rel_perf_index
  g8_sdare <- G9 * index_8/rel_perf_index
  g9_nsc <- G9 * index_9/rel_perf_index
  
  df<- data.frame("G9"=G9,
                  "g1_e"=g1_e,
                  "g2_re"=g2_re, 
                  "g3_ae"=g3_ae,
                  "g4_are"=g4_are,
                  "g5_sde"=g5_sde,
                  "g6_sdre"=g6_sdre,
                  "g7_sdae"=g7_sdae,
                  "g8_sdare"=g8_sdare,
                  "g9_nsc"=g9_nsc)
  
  filterNAs <-!is.na(df$G9)
  dff<-df[filterNAs,] 
  FModels<-row.names(dff)
  dff<-cbind(dff,FModels)
  library(plyr)
  # dff<-arrange(dff,desc(G9),FModels) # from http://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns
  transform(dff,FModels=reorder(FModels, -G9))
  
}

ComputeIndexesDataFrame <- function( data_frame ) {
  ## return the vector of grade_9 per model in the data frame
  
  errorMeanByModel<- sapply( X= levels(data_frame$FModel),    
                             FUN= function(x) { mean(data_frame[data_frame$FModel==x,]$e.i, 
                                                     na.rm=TRUE) })
  
  relativeErrorMeanByModel<- sapply( X= levels(data_frame$FModel),    
                                     FUN= function(x) { mean(data_frame[data_frame$FModel==x,]$rele.i, 
                                                             na.rm=TRUE) })
  
  absErrorMeanByModel <- sapply( X= levels(data_frame$FModel),    
                                 FUN= function(x) { mean(data_frame[data_frame$FModel==x,]$abse.i, 
                                                         na.rm=TRUE) })
  
  absRelativeErrorMeanByModel <- sapply( X= levels(data_frame$FModel),    
                                         FUN= function(x) { mean(data_frame[data_frame$FModel==x,]$absrele.i, 
                                                                 na.rm=TRUE) })
  
  errorVarianceByModel<- sapply( X= levels(data_frame$FModel),    
                                 FUN= function(x) { var(data_frame[data_frame$FModel==x,]$e.i, 
                                                        na.rm=TRUE) })
  
  relativeErrorVarianceByModel<- sapply( X= levels(data_frame$FModel),    
                                         FUN= function(x) { var(data_frame[data_frame$FModel==x,]$rele.i, 
                                                                na.rm=TRUE) })
  
  absErrorVarianceByModel <- sapply( X= levels(data_frame$FModel),    
                                     FUN= function(x) { var(data_frame[data_frame$FModel==x,]$abse.i, 
                                                            na.rm=TRUE) })
  
  absRelativeErrorVarianceByModel <- sapply( X= levels(data_frame$FModel),    
                                             FUN= function(x) { var(data_frame[data_frame$FModel==x,]$absrele.i, 
                                                                    na.rm=TRUE) })
  
  errorMinByModel<- min(abs(errorMeanByModel),na.rm=TRUE)
  
  relativeErrorMinByModel<- min(abs(relativeErrorMeanByModel),na.rm=TRUE)
  
  absErrorMinByModel <- min(abs(absRelativeErrorMeanByModel), na.rm=TRUE)
  
  absRelativeErrorMinByModel <- min(abs(absRelativeErrorMeanByModel), na.rm=TRUE)
  
  errorMaxByModel<- max(abs(errorMeanByModel),na.rm=TRUE)
  
  relativeErrorMaxByModel<- max(abs(relativeErrorMeanByModel),na.rm=TRUE)
  
  absErrorMaxByModel <- max(abs(absErrorMeanByModel),na.rm=TRUE)
  
  absRelativeErrorMaxByModel <- max(abs(absRelativeErrorMeanByModel),na.rm=TRUE)
  
  filterForFailedCases <-sapply( X=levels(data_frame$FModel),
                                 FUN = function(x) { is.na(data_frame$LiqHoldup[data_frame$FModel==x])},
                                 simplify=FALSE) # to get a list with the model names each of as many sum files as read 
  
  numberOfSuccessfulCases <- sapply( X=levels(data_frame$FModel),
                                     FUN = function(x) { (length(data_frame$LiqHoldup[data_frame$FModel==x]) - 
                                                            length(data_frame$LiqHoldup[data_frame$FModel==x][filterForFailedCases[[x]] ] ) )})
  
  index_1 <- 0.9*( abs( errorMeanByModel ) - errorMinByModel ) / ( errorMaxByModel - errorMinByModel ) 
  index_2 <- 0.9*( abs( relativeErrorMeanByModel) - relativeErrorMinByModel ) / ( relativeErrorMaxByModel - relativeErrorMinByModel)
  index_3 <- 0.9*( abs( absErrorMeanByModel) - absErrorMinByModel) / ( absErrorMaxByModel - absErrorMinByModel )  
  index_4 <- 0.9*( abs( absRelativeErrorMeanByModel) - absRelativeErrorMinByModel ) / ( absRelativeErrorMaxByModel - absRelativeErrorMinByModel )
  index_5 <- 0.9*( errorVarianceByModel - min(errorVarianceByModel,na.rm=TRUE)) / ( max(errorVarianceByModel,na.rm=TRUE)-min(errorVarianceByModel,na.rm=TRUE))
  index_6 <- 0.9*( relativeErrorVarianceByModel - min(relativeErrorVarianceByModel,na.rm=TRUE)) / ( max(relativeErrorVarianceByModel,na.rm=TRUE)-min(relativeErrorVarianceByModel,na.rm=TRUE) )
  index_7 <- 0.9*( absErrorVarianceByModel - min( absErrorVarianceByModel,na.rm=TRUE)) / ( max(absErrorVarianceByModel,na.rm=TRUE)-min(absErrorVarianceByModel,na.rm=TRUE) )
  index_8 <- 0.9*( absRelativeErrorVarianceByModel - min( absRelativeErrorVarianceByModel,na.rm=TRUE)) / ( max(absRelativeErrorVarianceByModel,na.rm=TRUE)-min(absRelativeErrorVarianceByModel,na.rm=TRUE) )
  index_9 <- 0.1*( max( numberOfSuccessfulCases,na.rm=TRUE ) - numberOfSuccessfulCases ) / ( max(numberOfSuccessfulCases,na.rm=TRUE)-min(numberOfSuccessfulCases,na.rm=TRUE))
  
  df<- data.frame("e"=index_1,
                  "re"=index_2, 
                  "ae"=index_3,
                  "are"=index_4,
                  "ve"=index_5,
                  "vre"=index_6,
                  "vae"=index_7,
                  "vare"=index_8,
                  "nsc"=index_9)
  FModels<-row.names(df)
  df<-cbind(df,FModels)
}