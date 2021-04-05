require(raster)
require(sp)
require(rgdal)
require(rfUtilities)
require(randomForest)
require(dismo)
library(plyr)
library(dplyr)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("cowplot")
library(cowplot)
# install.packages("maptools")
library(maptools)
# install.packages("rgeos")
library(rgeos)

#####SSP1262140#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP1262140")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP1262140invasive_with_envi <- invasive_location
SSP1262140native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP1262140invasive_with_envi <- cbind(SSP1262140invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP1262140native_with_envi <- cbind(SSP1262140native_with_envi,native)
}

SSP1262140invasive_with_envi$pa <- 1
SSP1262140native_with_envi$pa <- 1

write.csv(SSP1262140invasive_with_envi, file="SSP1262140invasive_with_envi.csv")
write.csv(SSP1262140native_with_envi, file="SSP1262140native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP1262140invasive_with_envi <- na.omit(SSP1262140invasive_with_envi)
SSP1262140native_with_envi <- na.omit(SSP1262140native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP1262140invasive_with_envi),nrow(SSP1262140native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP1262140background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP1262140background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP1262140background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP1262140background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP1262140background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP1262140background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP1262140background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP1262140background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP1262140invasive_background <- SSP1262140background_invasive_locations
SSP1262140native_background <- SSP1262140background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP1262140invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP1262140background_invasive_locations))
  colnames(SSP1262140invasive_background_tmp) <- envi_filename
  SSP1262140invasive_background <- cbind(SSP1262140invasive_background,SSP1262140invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP1262140native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP1262140background_native_locations))
  colnames(SSP1262140native_background_tmp) <- envi_filename
  SSP1262140native_background <- cbind(SSP1262140native_background,SSP1262140native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP1262140invasive_background$pa <- 0
SSP1262140native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP1262140invasive_background <- na.omit(SSP1262140invasive_background)
SSP1262140native_background <- na.omit(SSP1262140native_background)




invSSP1262140_randomF_rf_importance_total <- data.frame()
invSSP1262140_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP1262140invasive_with_envi[,colnames(SSP1262140invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP1262140invasive_background[,colnames(SSP1262140invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.

  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP1262140rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP1262140_randomF_rf_importance <- importance(invSSP1262140rf_regress)
  invSSP1262140_randomF_rf_importance <- data.frame(names=row.names(invSSP1262140_randomF_rf_importance),invSSP1262140_randomF_rf_importance)
  invSSP1262140_randomF_rf_importance_total <- rbind(invSSP1262140_randomF_rf_importance_total,invSSP1262140_randomF_rf_importance)
  
  invSSP1262140_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP1262140rf_regress))
  invSSP1262140_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP1262140_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP1262140_randomF_rf_evaluation$AUC <- invSSP1262140_erf@auc
  invSSP1262140_randomF_rf_evaluation$cor <- invSSP1262140_erf@cor
  invSSP1262140_randomF_rf_evaluation$kappa <- max(invSSP1262140_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP1262140_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP1262140_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP1262140_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP1262140_randomF_evaluation_total <- rbind(invSSP1262140_randomF_evaluation_total,invSSP1262140_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP1262140_randomF_evaluation_total)
tmpSD <- apply(invSSP1262140_randomF_evaluation_total,2,sd)
invSSP1262140_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP1262140_randomF_evaluation_total_summarized,paste("InvasiveSSP1262140randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)





natSSP1262140_randomF_rf_importance_total <- data.frame()
natSSP1262140_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP1262140native_with_envi[,colnames(SSP1262140native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP1262140native_background[,colnames(SSP1262140native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP1262140rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP1262140_randomF_rf_importance <- importance(natSSP1262140rf_regress)
  natSSP1262140_randomF_rf_importance <- data.frame(names=row.names(natSSP1262140_randomF_rf_importance),natSSP1262140_randomF_rf_importance)
  natSSP1262140_randomF_rf_importance_total <- rbind(natSSP1262140_randomF_rf_importance_total,natSSP1262140_randomF_rf_importance)
  
  natSSP1262140_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP1262140rf_regress))
  natSSP1262140_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP1262140_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP1262140_randomF_rf_evaluation$AUC <- natSSP1262140_erf@auc
  natSSP1262140_randomF_rf_evaluation$cor <- natSSP1262140_erf@cor
  natSSP1262140_randomF_rf_evaluation$kappa <- max(natSSP1262140_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP1262140_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP1262140_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP1262140_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP1262140_randomF_evaluation_total <- rbind(natSSP1262140_randomF_evaluation_total,natSSP1262140_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP1262140_randomF_evaluation_total)
tmpSD <- apply(natSSP1262140_randomF_evaluation_total,2,sd)
natSSP1262140_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP1262140_randomF_evaluation_total_summarized,paste("nativeSSP1262140randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)

#####SSP1264160#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP1264160")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP1264160invasive_with_envi <- invasive_location
SSP1264160native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP1264160invasive_with_envi <- cbind(SSP1264160invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP1264160native_with_envi <- cbind(SSP1264160native_with_envi,native)
}

SSP1264160invasive_with_envi$pa <- 1
SSP1264160native_with_envi$pa <- 1

write.csv(SSP1264160invasive_with_envi, file="SSP1264160invasive_with_envi.csv")
write.csv(SSP1264160native_with_envi, file="SSP1264160native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP1264160invasive_with_envi <- na.omit(SSP1264160invasive_with_envi)
SSP1264160native_with_envi <- na.omit(SSP1264160native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP1264160invasive_with_envi),nrow(SSP1264160native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP1264160background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP1264160background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP1264160background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP1264160background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP1264160background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP1264160background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP1264160background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP1264160background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP1264160invasive_background <- SSP1264160background_invasive_locations
SSP1264160native_background <- SSP1264160background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP1264160invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP1264160background_invasive_locations))
  colnames(SSP1264160invasive_background_tmp) <- envi_filename
  SSP1264160invasive_background <- cbind(SSP1264160invasive_background,SSP1264160invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP1264160native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP1264160background_native_locations))
  colnames(SSP1264160native_background_tmp) <- envi_filename
  SSP1264160native_background <- cbind(SSP1264160native_background,SSP1264160native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP1264160invasive_background$pa <- 0
SSP1264160native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP1264160invasive_background <- na.omit(SSP1264160invasive_background)
SSP1264160native_background <- na.omit(SSP1264160native_background)




invSSP1264160_randomF_rf_importance_total <- data.frame()
invSSP1264160_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP1264160invasive_with_envi[,colnames(SSP1264160invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP1264160invasive_background[,colnames(SSP1264160invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP1264160rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP1264160_randomF_rf_importance <- importance(invSSP1264160rf_regress)
  invSSP1264160_randomF_rf_importance <- data.frame(names=row.names(invSSP1264160_randomF_rf_importance),invSSP1264160_randomF_rf_importance)
  invSSP1264160_randomF_rf_importance_total <- rbind(invSSP1264160_randomF_rf_importance_total,invSSP1264160_randomF_rf_importance)
  
  invSSP1264160_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP1264160rf_regress))
  invSSP1264160_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP1264160_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP1264160_randomF_rf_evaluation$AUC <- invSSP1264160_erf@auc
  invSSP1264160_randomF_rf_evaluation$cor <- invSSP1264160_erf@cor
  invSSP1264160_randomF_rf_evaluation$kappa <- max(invSSP1264160_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP1264160_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP1264160_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP1264160_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP1264160_randomF_evaluation_total <- rbind(invSSP1264160_randomF_evaluation_total,invSSP1264160_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP1264160_randomF_evaluation_total)
tmpSD <- apply(invSSP1264160_randomF_evaluation_total,2,sd)
invSSP1264160_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP1264160_randomF_evaluation_total_summarized,paste("InvasiveSSP1264160randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)

natSSP1264160_randomF_rf_importance_total <- data.frame()
natSSP1264160_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP1264160native_with_envi[,colnames(SSP1264160native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP1264160native_background[,colnames(SSP1264160native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP1264160rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP1264160_randomF_rf_importance <- importance(natSSP1264160rf_regress)
  natSSP1264160_randomF_rf_importance <- data.frame(names=row.names(natSSP1264160_randomF_rf_importance),natSSP1264160_randomF_rf_importance)
  natSSP1264160_randomF_rf_importance_total <- rbind(natSSP1264160_randomF_rf_importance_total,natSSP1264160_randomF_rf_importance)
  
  natSSP1264160_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP1264160rf_regress))
  natSSP1264160_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP1264160_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP1264160_randomF_rf_evaluation$AUC <- natSSP1264160_erf@auc
  natSSP1264160_randomF_rf_evaluation$cor <- natSSP1264160_erf@cor
  natSSP1264160_randomF_rf_evaluation$kappa <- max(natSSP1264160_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP1264160_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP1264160_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP1264160_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP1264160_randomF_evaluation_total <- rbind(natSSP1264160_randomF_evaluation_total,natSSP1264160_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP1264160_randomF_evaluation_total)
tmpSD <- apply(natSSP1264160_randomF_evaluation_total,2,sd)
natSSP1264160_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP1264160_randomF_evaluation_total_summarized,paste("nativeSSP1264160randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)

#####SSP1266180#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP1266180")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP1266180invasive_with_envi <- invasive_location
SSP1266180native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP1266180invasive_with_envi <- cbind(SSP1266180invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP1266180native_with_envi <- cbind(SSP1266180native_with_envi,native)
}

SSP1266180invasive_with_envi$pa <- 1
SSP1266180native_with_envi$pa <- 1

write.csv(SSP1266180invasive_with_envi, file="SSP1266180invasive_with_envi.csv")
write.csv(SSP1266180native_with_envi, file="SSP1266180native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP1266180invasive_with_envi <- na.omit(SSP1266180invasive_with_envi)
SSP1266180native_with_envi <- na.omit(SSP1266180native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP1266180invasive_with_envi),nrow(SSP1266180native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP1266180background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP1266180background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP1266180background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP1266180background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP1266180background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP1266180background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP1266180background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP1266180background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP1266180invasive_background <- SSP1266180background_invasive_locations
SSP1266180native_background <- SSP1266180background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP1266180invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP1266180background_invasive_locations))
  colnames(SSP1266180invasive_background_tmp) <- envi_filename
  SSP1266180invasive_background <- cbind(SSP1266180invasive_background,SSP1266180invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP1266180native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP1266180background_native_locations))
  colnames(SSP1266180native_background_tmp) <- envi_filename
  SSP1266180native_background <- cbind(SSP1266180native_background,SSP1266180native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP1266180invasive_background$pa <- 0
SSP1266180native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP1266180invasive_background <- na.omit(SSP1266180invasive_background)
SSP1266180native_background <- na.omit(SSP1266180native_background)




invSSP1266180_randomF_rf_importance_total <- data.frame()
invSSP1266180_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP1266180invasive_with_envi[,colnames(SSP1266180invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP1266180invasive_background[,colnames(SSP1266180invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP1266180rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP1266180_randomF_rf_importance <- importance(invSSP1266180rf_regress)
  invSSP1266180_randomF_rf_importance <- data.frame(names=row.names(invSSP1266180_randomF_rf_importance),invSSP1266180_randomF_rf_importance)
  invSSP1266180_randomF_rf_importance_total <- rbind(invSSP1266180_randomF_rf_importance_total,invSSP1266180_randomF_rf_importance)
  
  invSSP1266180_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP1266180rf_regress))
  invSSP1266180_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP1266180_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP1266180_randomF_rf_evaluation$AUC <- invSSP1266180_erf@auc
  invSSP1266180_randomF_rf_evaluation$cor <- invSSP1266180_erf@cor
  invSSP1266180_randomF_rf_evaluation$kappa <- max(invSSP1266180_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP1266180_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP1266180_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP1266180_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP1266180_randomF_evaluation_total <- rbind(invSSP1266180_randomF_evaluation_total,invSSP1266180_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP1266180_randomF_evaluation_total)
tmpSD <- apply(invSSP1266180_randomF_evaluation_total,2,sd)
invSSP1266180_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP1266180_randomF_evaluation_total_summarized,paste("InvasiveSSP1266180randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)


natSSP1266180_randomF_rf_importance_total <- data.frame()
natSSP1266180_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP1266180native_with_envi[,colnames(SSP1266180native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP1266180native_background[,colnames(SSP1266180native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP1266180rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP1266180_randomF_rf_importance <- importance(natSSP1266180rf_regress)
  natSSP1266180_randomF_rf_importance <- data.frame(names=row.names(natSSP1266180_randomF_rf_importance),natSSP1266180_randomF_rf_importance)
  natSSP1266180_randomF_rf_importance_total <- rbind(natSSP1266180_randomF_rf_importance_total,natSSP1266180_randomF_rf_importance)
  
  natSSP1266180_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP1266180rf_regress))
  natSSP1266180_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP1266180_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP1266180_randomF_rf_evaluation$AUC <- natSSP1266180_erf@auc
  natSSP1266180_randomF_rf_evaluation$cor <- natSSP1266180_erf@cor
  natSSP1266180_randomF_rf_evaluation$kappa <- max(natSSP1266180_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP1266180_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP1266180_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP1266180_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP1266180_randomF_evaluation_total <- rbind(natSSP1266180_randomF_evaluation_total,natSSP1266180_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP1266180_randomF_evaluation_total)
tmpSD <- apply(natSSP1266180_randomF_evaluation_total,2,sd)
natSSP1266180_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP1266180_randomF_evaluation_total_summarized,paste("nativeSSP1266180randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)


#####SSP1268100#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP1268100")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP1268100invasive_with_envi <- invasive_location
SSP1268100native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP1268100invasive_with_envi <- cbind(SSP1268100invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP1268100native_with_envi <- cbind(SSP1268100native_with_envi,native)
}

SSP1268100invasive_with_envi$pa <- 1
SSP1268100native_with_envi$pa <- 1

write.csv(SSP1268100invasive_with_envi, file="SSP1268100invasive_with_envi.csv")
write.csv(SSP1268100native_with_envi, file="SSP1268100native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP1268100invasive_with_envi <- na.omit(SSP1268100invasive_with_envi)
SSP1268100native_with_envi <- na.omit(SSP1268100native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP1268100invasive_with_envi),nrow(SSP1268100native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP1268100background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP1268100background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP1268100background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP1268100background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP1268100background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP1268100background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP1268100background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP1268100background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP1268100invasive_background <- SSP1268100background_invasive_locations
SSP1268100native_background <- SSP1268100background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP1268100invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP1268100background_invasive_locations))
  colnames(SSP1268100invasive_background_tmp) <- envi_filename
  SSP1268100invasive_background <- cbind(SSP1268100invasive_background,SSP1268100invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP1268100native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP1268100background_native_locations))
  colnames(SSP1268100native_background_tmp) <- envi_filename
  SSP1268100native_background <- cbind(SSP1268100native_background,SSP1268100native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP1268100invasive_background$pa <- 0
SSP1268100native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP1268100invasive_background <- na.omit(SSP1268100invasive_background)
SSP1268100native_background <- na.omit(SSP1268100native_background)




invSSP1268100_randomF_rf_importance_total <- data.frame()
invSSP1268100_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP1268100invasive_with_envi[,colnames(SSP1268100invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP1268100invasive_background[,colnames(SSP1268100invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP1268100rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP1268100_randomF_rf_importance <- importance(invSSP1268100rf_regress)
  invSSP1268100_randomF_rf_importance <- data.frame(names=row.names(invSSP1268100_randomF_rf_importance),invSSP1268100_randomF_rf_importance)
  invSSP1268100_randomF_rf_importance_total <- rbind(invSSP1268100_randomF_rf_importance_total,invSSP1268100_randomF_rf_importance)
  
  invSSP1268100_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP1268100rf_regress))
  invSSP1268100_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP1268100_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP1268100_randomF_rf_evaluation$AUC <- invSSP1268100_erf@auc
  invSSP1268100_randomF_rf_evaluation$cor <- invSSP1268100_erf@cor
  invSSP1268100_randomF_rf_evaluation$kappa <- max(invSSP1268100_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP1268100_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP1268100_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP1268100_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP1268100_randomF_evaluation_total <- rbind(invSSP1268100_randomF_evaluation_total,invSSP1268100_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP1268100_randomF_evaluation_total)
tmpSD <- apply(invSSP1268100_randomF_evaluation_total,2,sd)
invSSP1268100_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP1268100_randomF_evaluation_total_summarized,paste("InvasiveSSP1268100randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)




natSSP1268100_randomF_rf_importance_total <- data.frame()
natSSP1268100_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP1268100native_with_envi[,colnames(SSP1268100native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP1268100native_background[,colnames(SSP1268100native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP1268100rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP1268100_randomF_rf_importance <- importance(natSSP1268100rf_regress)
  natSSP1268100_randomF_rf_importance <- data.frame(names=row.names(natSSP1268100_randomF_rf_importance),natSSP1268100_randomF_rf_importance)
  natSSP1268100_randomF_rf_importance_total <- rbind(natSSP1268100_randomF_rf_importance_total,natSSP1268100_randomF_rf_importance)
  
  natSSP1268100_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP1268100rf_regress))
  natSSP1268100_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP1268100_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP1268100_randomF_rf_evaluation$AUC <- natSSP1268100_erf@auc
  natSSP1268100_randomF_rf_evaluation$cor <- natSSP1268100_erf@cor
  natSSP1268100_randomF_rf_evaluation$kappa <- max(natSSP1268100_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP1268100_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP1268100_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP1268100_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP1268100_randomF_evaluation_total <- rbind(natSSP1268100_randomF_evaluation_total,natSSP1268100_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP1268100_randomF_evaluation_total)
tmpSD <- apply(natSSP1268100_randomF_evaluation_total,2,sd)
natSSP1268100_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP1268100_randomF_evaluation_total_summarized,paste("nativeSSP1268100randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)


#####SSP2452140#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP2452140")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP2452140invasive_with_envi <- invasive_location
SSP2452140native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP2452140invasive_with_envi <- cbind(SSP2452140invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP2452140native_with_envi <- cbind(SSP2452140native_with_envi,native)
}

SSP2452140invasive_with_envi$pa <- 1
SSP2452140native_with_envi$pa <- 1

write.csv(SSP2452140invasive_with_envi, file="SSP2452140invasive_with_envi.csv")
write.csv(SSP2452140native_with_envi, file="SSP2452140native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP2452140invasive_with_envi <- na.omit(SSP2452140invasive_with_envi)
SSP2452140native_with_envi <- na.omit(SSP2452140native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP2452140invasive_with_envi),nrow(SSP2452140native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP2452140background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP2452140background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP2452140background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP2452140background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP2452140background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP2452140background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP2452140background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP2452140background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP2452140invasive_background <- SSP2452140background_invasive_locations
SSP2452140native_background <- SSP2452140background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP2452140invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP2452140background_invasive_locations))
  colnames(SSP2452140invasive_background_tmp) <- envi_filename
  SSP2452140invasive_background <- cbind(SSP2452140invasive_background,SSP2452140invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP2452140native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP2452140background_native_locations))
  colnames(SSP2452140native_background_tmp) <- envi_filename
  SSP2452140native_background <- cbind(SSP2452140native_background,SSP2452140native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP2452140invasive_background$pa <- 0
SSP2452140native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP2452140invasive_background <- na.omit(SSP2452140invasive_background)
SSP2452140native_background <- na.omit(SSP2452140native_background)




invSSP2452140_randomF_rf_importance_total <- data.frame()
invSSP2452140_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP2452140invasive_with_envi[,colnames(SSP2452140invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP2452140invasive_background[,colnames(SSP2452140invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP2452140rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP2452140_randomF_rf_importance <- importance(invSSP2452140rf_regress)
  invSSP2452140_randomF_rf_importance <- data.frame(names=row.names(invSSP2452140_randomF_rf_importance),invSSP2452140_randomF_rf_importance)
  invSSP2452140_randomF_rf_importance_total <- rbind(invSSP2452140_randomF_rf_importance_total,invSSP2452140_randomF_rf_importance)
  
  invSSP2452140_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP2452140rf_regress))
  invSSP2452140_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP2452140_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP2452140_randomF_rf_evaluation$AUC <- invSSP2452140_erf@auc
  invSSP2452140_randomF_rf_evaluation$cor <- invSSP2452140_erf@cor
  invSSP2452140_randomF_rf_evaluation$kappa <- max(invSSP2452140_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP2452140_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP2452140_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP2452140_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP2452140_randomF_evaluation_total <- rbind(invSSP2452140_randomF_evaluation_total,invSSP2452140_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP2452140_randomF_evaluation_total)
tmpSD <- apply(invSSP2452140_randomF_evaluation_total,2,sd)
invSSP2452140_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP2452140_randomF_evaluation_total_summarized,paste("InvasiveSSP2452140randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)




natSSP2452140_randomF_rf_importance_total <- data.frame()
natSSP2452140_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP2452140native_with_envi[,colnames(SSP2452140native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP2452140native_background[,colnames(SSP2452140native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP2452140rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP2452140_randomF_rf_importance <- importance(natSSP2452140rf_regress)
  natSSP2452140_randomF_rf_importance <- data.frame(names=row.names(natSSP2452140_randomF_rf_importance),natSSP2452140_randomF_rf_importance)
  natSSP2452140_randomF_rf_importance_total <- rbind(natSSP2452140_randomF_rf_importance_total,natSSP2452140_randomF_rf_importance)
  
  natSSP2452140_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP2452140rf_regress))
  natSSP2452140_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP2452140_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP2452140_randomF_rf_evaluation$AUC <- natSSP2452140_erf@auc
  natSSP2452140_randomF_rf_evaluation$cor <- natSSP2452140_erf@cor
  natSSP2452140_randomF_rf_evaluation$kappa <- max(natSSP2452140_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP2452140_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP2452140_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP2452140_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP2452140_randomF_evaluation_total <- rbind(natSSP2452140_randomF_evaluation_total,natSSP2452140_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP2452140_randomF_evaluation_total)
tmpSD <- apply(natSSP2452140_randomF_evaluation_total,2,sd)
natSSP2452140_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP2452140_randomF_evaluation_total_summarized,paste("nativeSSP2452140randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)

#####SSP2454160#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP2454160")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP2454160invasive_with_envi <- invasive_location
SSP2454160native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP2454160invasive_with_envi <- cbind(SSP2454160invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP2454160native_with_envi <- cbind(SSP2454160native_with_envi,native)
}

SSP2454160invasive_with_envi$pa <- 1
SSP2454160native_with_envi$pa <- 1

write.csv(SSP2454160invasive_with_envi, file="SSP2454160invasive_with_envi.csv")
write.csv(SSP2454160native_with_envi, file="SSP2454160native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP2454160invasive_with_envi <- na.omit(SSP2454160invasive_with_envi)
SSP2454160native_with_envi <- na.omit(SSP2454160native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP2454160invasive_with_envi),nrow(SSP2454160native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP2454160background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP2454160background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP2454160background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP2454160background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP2454160background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP2454160background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP2454160background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP2454160background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP2454160invasive_background <- SSP2454160background_invasive_locations
SSP2454160native_background <- SSP2454160background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP2454160invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP2454160background_invasive_locations))
  colnames(SSP2454160invasive_background_tmp) <- envi_filename
  SSP2454160invasive_background <- cbind(SSP2454160invasive_background,SSP2454160invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP2454160native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP2454160background_native_locations))
  colnames(SSP2454160native_background_tmp) <- envi_filename
  SSP2454160native_background <- cbind(SSP2454160native_background,SSP2454160native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP2454160invasive_background$pa <- 0
SSP2454160native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP2454160invasive_background <- na.omit(SSP2454160invasive_background)
SSP2454160native_background <- na.omit(SSP2454160native_background)




invSSP2454160_randomF_rf_importance_total <- data.frame()
invSSP2454160_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP2454160invasive_with_envi[,colnames(SSP2454160invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP2454160invasive_background[,colnames(SSP2454160invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP2454160rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP2454160_randomF_rf_importance <- importance(invSSP2454160rf_regress)
  invSSP2454160_randomF_rf_importance <- data.frame(names=row.names(invSSP2454160_randomF_rf_importance),invSSP2454160_randomF_rf_importance)
  invSSP2454160_randomF_rf_importance_total <- rbind(invSSP2454160_randomF_rf_importance_total,invSSP2454160_randomF_rf_importance)
  
  invSSP2454160_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP2454160rf_regress))
  invSSP2454160_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP2454160_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP2454160_randomF_rf_evaluation$AUC <- invSSP2454160_erf@auc
  invSSP2454160_randomF_rf_evaluation$cor <- invSSP2454160_erf@cor
  invSSP2454160_randomF_rf_evaluation$kappa <- max(invSSP2454160_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP2454160_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP2454160_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP2454160_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP2454160_randomF_evaluation_total <- rbind(invSSP2454160_randomF_evaluation_total,invSSP2454160_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP2454160_randomF_evaluation_total)
tmpSD <- apply(invSSP2454160_randomF_evaluation_total,2,sd)
invSSP2454160_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP2454160_randomF_evaluation_total_summarized,paste("InvasiveSSP2454160randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)




natSSP2454160_randomF_rf_importance_total <- data.frame()
natSSP2454160_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP2454160native_with_envi[,colnames(SSP2454160native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP2454160native_background[,colnames(SSP2454160native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP2454160rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP2454160_randomF_rf_importance <- importance(natSSP2454160rf_regress)
  natSSP2454160_randomF_rf_importance <- data.frame(names=row.names(natSSP2454160_randomF_rf_importance),natSSP2454160_randomF_rf_importance)
  natSSP2454160_randomF_rf_importance_total <- rbind(natSSP2454160_randomF_rf_importance_total,natSSP2454160_randomF_rf_importance)
  
  natSSP2454160_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP2454160rf_regress))
  natSSP2454160_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP2454160_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP2454160_randomF_rf_evaluation$AUC <- natSSP2454160_erf@auc
  natSSP2454160_randomF_rf_evaluation$cor <- natSSP2454160_erf@cor
  natSSP2454160_randomF_rf_evaluation$kappa <- max(natSSP2454160_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP2454160_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP2454160_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP2454160_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP2454160_randomF_evaluation_total <- rbind(natSSP2454160_randomF_evaluation_total,natSSP2454160_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP2454160_randomF_evaluation_total)
tmpSD <- apply(natSSP2454160_randomF_evaluation_total,2,sd)
natSSP2454160_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP2454160_randomF_evaluation_total_summarized,paste("nativeSSP2454160randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)

#####SSP2456180#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP2456180")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP2456180invasive_with_envi <- invasive_location
SSP2456180native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP2456180invasive_with_envi <- cbind(SSP2456180invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP2456180native_with_envi <- cbind(SSP2456180native_with_envi,native)
}

SSP2456180invasive_with_envi$pa <- 1
SSP2456180native_with_envi$pa <- 1

write.csv(SSP2456180invasive_with_envi, file="SSP2456180invasive_with_envi.csv")
write.csv(SSP2456180native_with_envi, file="SSP2456180native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP2456180invasive_with_envi <- na.omit(SSP2456180invasive_with_envi)
SSP2456180native_with_envi <- na.omit(SSP2456180native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP2456180invasive_with_envi),nrow(SSP2456180native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP2456180background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP2456180background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP2456180background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP2456180background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP2456180background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP2456180background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP2456180background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP2456180background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP2456180invasive_background <- SSP2456180background_invasive_locations
SSP2456180native_background <- SSP2456180background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP2456180invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP2456180background_invasive_locations))
  colnames(SSP2456180invasive_background_tmp) <- envi_filename
  SSP2456180invasive_background <- cbind(SSP2456180invasive_background,SSP2456180invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP2456180native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP2456180background_native_locations))
  colnames(SSP2456180native_background_tmp) <- envi_filename
  SSP2456180native_background <- cbind(SSP2456180native_background,SSP2456180native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP2456180invasive_background$pa <- 0
SSP2456180native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP2456180invasive_background <- na.omit(SSP2456180invasive_background)
SSP2456180native_background <- na.omit(SSP2456180native_background)




invSSP2456180_randomF_rf_importance_total <- data.frame()
invSSP2456180_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP2456180invasive_with_envi[,colnames(SSP2456180invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP2456180invasive_background[,colnames(SSP2456180invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP2456180rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP2456180_randomF_rf_importance <- importance(invSSP2456180rf_regress)
  invSSP2456180_randomF_rf_importance <- data.frame(names=row.names(invSSP2456180_randomF_rf_importance),invSSP2456180_randomF_rf_importance)
  invSSP2456180_randomF_rf_importance_total <- rbind(invSSP2456180_randomF_rf_importance_total,invSSP2456180_randomF_rf_importance)
  
  invSSP2456180_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP2456180rf_regress))
  invSSP2456180_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP2456180_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP2456180_randomF_rf_evaluation$AUC <- invSSP2456180_erf@auc
  invSSP2456180_randomF_rf_evaluation$cor <- invSSP2456180_erf@cor
  invSSP2456180_randomF_rf_evaluation$kappa <- max(invSSP2456180_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP2456180_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP2456180_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP2456180_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP2456180_randomF_evaluation_total <- rbind(invSSP2456180_randomF_evaluation_total,invSSP2456180_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP2456180_randomF_evaluation_total)
tmpSD <- apply(invSSP2456180_randomF_evaluation_total,2,sd)
invSSP2456180_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP2456180_randomF_evaluation_total_summarized,paste("InvasiveSSP2456180randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)



natSSP2456180_randomF_rf_importance_total <- data.frame()
natSSP2456180_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP2456180native_with_envi[,colnames(SSP2456180native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP2456180native_background[,colnames(SSP2456180native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP2456180rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP2456180_randomF_rf_importance <- importance(natSSP2456180rf_regress)
  natSSP2456180_randomF_rf_importance <- data.frame(names=row.names(natSSP2456180_randomF_rf_importance),natSSP2456180_randomF_rf_importance)
  natSSP2456180_randomF_rf_importance_total <- rbind(natSSP2456180_randomF_rf_importance_total,natSSP2456180_randomF_rf_importance)
  
  natSSP2456180_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP2456180rf_regress))
  natSSP2456180_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP2456180_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP2456180_randomF_rf_evaluation$AUC <- natSSP2456180_erf@auc
  natSSP2456180_randomF_rf_evaluation$cor <- natSSP2456180_erf@cor
  natSSP2456180_randomF_rf_evaluation$kappa <- max(natSSP2456180_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP2456180_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP2456180_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP2456180_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP2456180_randomF_evaluation_total <- rbind(natSSP2456180_randomF_evaluation_total,natSSP2456180_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP2456180_randomF_evaluation_total)
tmpSD <- apply(natSSP2456180_randomF_evaluation_total,2,sd)
natSSP2456180_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP2456180_randomF_evaluation_total_summarized,paste("nativeSSP2456180randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)

#####SSP2458100#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP2458100")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP2458100invasive_with_envi <- invasive_location
SSP2458100native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP2458100invasive_with_envi <- cbind(SSP2458100invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP2458100native_with_envi <- cbind(SSP2458100native_with_envi,native)
}

SSP2458100invasive_with_envi$pa <- 1
SSP2458100native_with_envi$pa <- 1

write.csv(SSP2458100invasive_with_envi, file="SSP2458100invasive_with_envi.csv")
write.csv(SSP2458100native_with_envi, file="SSP2458100native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP2458100invasive_with_envi <- na.omit(SSP2458100invasive_with_envi)
SSP2458100native_with_envi <- na.omit(SSP2458100native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP2458100invasive_with_envi),nrow(SSP2458100native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP2458100background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP2458100background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP2458100background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP2458100background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP2458100background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP2458100background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP2458100background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP2458100background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP2458100invasive_background <- SSP2458100background_invasive_locations
SSP2458100native_background <- SSP2458100background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP2458100invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP2458100background_invasive_locations))
  colnames(SSP2458100invasive_background_tmp) <- envi_filename
  SSP2458100invasive_background <- cbind(SSP2458100invasive_background,SSP2458100invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP2458100native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP2458100background_native_locations))
  colnames(SSP2458100native_background_tmp) <- envi_filename
  SSP2458100native_background <- cbind(SSP2458100native_background,SSP2458100native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP2458100invasive_background$pa <- 0
SSP2458100native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP2458100invasive_background <- na.omit(SSP2458100invasive_background)
SSP2458100native_background <- na.omit(SSP2458100native_background)




invSSP2458100_randomF_rf_importance_total <- data.frame()
invSSP2458100_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP2458100invasive_with_envi[,colnames(SSP2458100invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP2458100invasive_background[,colnames(SSP2458100invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP2458100rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP2458100_randomF_rf_importance <- importance(invSSP2458100rf_regress)
  invSSP2458100_randomF_rf_importance <- data.frame(names=row.names(invSSP2458100_randomF_rf_importance),invSSP2458100_randomF_rf_importance)
  invSSP2458100_randomF_rf_importance_total <- rbind(invSSP2458100_randomF_rf_importance_total,invSSP2458100_randomF_rf_importance)
  
  invSSP2458100_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP2458100rf_regress))
  invSSP2458100_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP2458100_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP2458100_randomF_rf_evaluation$AUC <- invSSP2458100_erf@auc
  invSSP2458100_randomF_rf_evaluation$cor <- invSSP2458100_erf@cor
  invSSP2458100_randomF_rf_evaluation$kappa <- max(invSSP2458100_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP2458100_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP2458100_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP2458100_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP2458100_randomF_evaluation_total <- rbind(invSSP2458100_randomF_evaluation_total,invSSP2458100_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP2458100_randomF_evaluation_total)
tmpSD <- apply(invSSP2458100_randomF_evaluation_total,2,sd)
invSSP2458100_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP2458100_randomF_evaluation_total_summarized,paste("InvasiveSSP2458100randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)




natSSP2458100_randomF_rf_importance_total <- data.frame()
natSSP2458100_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP2458100native_with_envi[,colnames(SSP2458100native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP2458100native_background[,colnames(SSP2458100native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP2458100rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP2458100_randomF_rf_importance <- importance(natSSP2458100rf_regress)
  natSSP2458100_randomF_rf_importance <- data.frame(names=row.names(natSSP2458100_randomF_rf_importance),natSSP2458100_randomF_rf_importance)
  natSSP2458100_randomF_rf_importance_total <- rbind(natSSP2458100_randomF_rf_importance_total,natSSP2458100_randomF_rf_importance)
  
  natSSP2458100_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP2458100rf_regress))
  natSSP2458100_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP2458100_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP2458100_randomF_rf_evaluation$AUC <- natSSP2458100_erf@auc
  natSSP2458100_randomF_rf_evaluation$cor <- natSSP2458100_erf@cor
  natSSP2458100_randomF_rf_evaluation$kappa <- max(natSSP2458100_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP2458100_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP2458100_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP2458100_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP2458100_randomF_evaluation_total <- rbind(natSSP2458100_randomF_evaluation_total,natSSP2458100_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP2458100_randomF_evaluation_total)
tmpSD <- apply(natSSP2458100_randomF_evaluation_total,2,sd)
natSSP2458100_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP2458100_randomF_evaluation_total_summarized,paste("nativeSSP2458100randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)


#####SSP3702140#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP3702140")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP3702140invasive_with_envi <- invasive_location
SSP3702140native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP3702140invasive_with_envi <- cbind(SSP3702140invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP3702140native_with_envi <- cbind(SSP3702140native_with_envi,native)
}

SSP3702140invasive_with_envi$pa <- 1
SSP3702140native_with_envi$pa <- 1

write.csv(SSP3702140invasive_with_envi, file="SSP3702140invasive_with_envi.csv")
write.csv(SSP3702140native_with_envi, file="SSP3702140native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP3702140invasive_with_envi <- na.omit(SSP3702140invasive_with_envi)
SSP3702140native_with_envi <- na.omit(SSP3702140native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP3702140invasive_with_envi),nrow(SSP3702140native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP3702140background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP3702140background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP3702140background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP3702140background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP3702140background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP3702140background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP3702140background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP3702140background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP3702140invasive_background <- SSP3702140background_invasive_locations
SSP3702140native_background <- SSP3702140background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP3702140invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP3702140background_invasive_locations))
  colnames(SSP3702140invasive_background_tmp) <- envi_filename
  SSP3702140invasive_background <- cbind(SSP3702140invasive_background,SSP3702140invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP3702140native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP3702140background_native_locations))
  colnames(SSP3702140native_background_tmp) <- envi_filename
  SSP3702140native_background <- cbind(SSP3702140native_background,SSP3702140native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP3702140invasive_background$pa <- 0
SSP3702140native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP3702140invasive_background <- na.omit(SSP3702140invasive_background)
SSP3702140native_background <- na.omit(SSP3702140native_background)




invSSP3702140_randomF_rf_importance_total <- data.frame()
invSSP3702140_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP3702140invasive_with_envi[,colnames(SSP3702140invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP3702140invasive_background[,colnames(SSP3702140invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP3702140rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP3702140_randomF_rf_importance <- importance(invSSP3702140rf_regress)
  invSSP3702140_randomF_rf_importance <- data.frame(names=row.names(invSSP3702140_randomF_rf_importance),invSSP3702140_randomF_rf_importance)
  invSSP3702140_randomF_rf_importance_total <- rbind(invSSP3702140_randomF_rf_importance_total,invSSP3702140_randomF_rf_importance)
  
  invSSP3702140_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP3702140rf_regress))
  invSSP3702140_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP3702140_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP3702140_randomF_rf_evaluation$AUC <- invSSP3702140_erf@auc
  invSSP3702140_randomF_rf_evaluation$cor <- invSSP3702140_erf@cor
  invSSP3702140_randomF_rf_evaluation$kappa <- max(invSSP3702140_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP3702140_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP3702140_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP3702140_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP3702140_randomF_evaluation_total <- rbind(invSSP3702140_randomF_evaluation_total,invSSP3702140_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP3702140_randomF_evaluation_total)
tmpSD <- apply(invSSP3702140_randomF_evaluation_total,2,sd)
invSSP3702140_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP3702140_randomF_evaluation_total_summarized,paste("InvasiveSSP3702140randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)


natSSP3702140_randomF_rf_importance_total <- data.frame()
natSSP3702140_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP3702140native_with_envi[,colnames(SSP3702140native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP3702140native_background[,colnames(SSP3702140native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP3702140rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP3702140_randomF_rf_importance <- importance(natSSP3702140rf_regress)
  natSSP3702140_randomF_rf_importance <- data.frame(names=row.names(natSSP3702140_randomF_rf_importance),natSSP3702140_randomF_rf_importance)
  natSSP3702140_randomF_rf_importance_total <- rbind(natSSP3702140_randomF_rf_importance_total,natSSP3702140_randomF_rf_importance)
  
  natSSP3702140_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP3702140rf_regress))
  natSSP3702140_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP3702140_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP3702140_randomF_rf_evaluation$AUC <- natSSP3702140_erf@auc
  natSSP3702140_randomF_rf_evaluation$cor <- natSSP3702140_erf@cor
  natSSP3702140_randomF_rf_evaluation$kappa <- max(natSSP3702140_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP3702140_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP3702140_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP3702140_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP3702140_randomF_evaluation_total <- rbind(natSSP3702140_randomF_evaluation_total,natSSP3702140_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP3702140_randomF_evaluation_total)
tmpSD <- apply(natSSP3702140_randomF_evaluation_total,2,sd)
natSSP3702140_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP3702140_randomF_evaluation_total_summarized,paste("nativeSSP3702140randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)

#####SSP3704160#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP3704160")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP3704160invasive_with_envi <- invasive_location
SSP3704160native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP3704160invasive_with_envi <- cbind(SSP3704160invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP3704160native_with_envi <- cbind(SSP3704160native_with_envi,native)
}

SSP3704160invasive_with_envi$pa <- 1
SSP3704160native_with_envi$pa <- 1

write.csv(SSP3704160invasive_with_envi, file="SSP3704160invasive_with_envi.csv")
write.csv(SSP3704160native_with_envi, file="SSP3704160native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP3704160invasive_with_envi <- na.omit(SSP3704160invasive_with_envi)
SSP3704160native_with_envi <- na.omit(SSP3704160native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP3704160invasive_with_envi),nrow(SSP3704160native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP3704160background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP3704160background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP3704160background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP3704160background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP3704160background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP3704160background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP3704160background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP3704160background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP3704160invasive_background <- SSP3704160background_invasive_locations
SSP3704160native_background <- SSP3704160background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP3704160invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP3704160background_invasive_locations))
  colnames(SSP3704160invasive_background_tmp) <- envi_filename
  SSP3704160invasive_background <- cbind(SSP3704160invasive_background,SSP3704160invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP3704160native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP3704160background_native_locations))
  colnames(SSP3704160native_background_tmp) <- envi_filename
  SSP3704160native_background <- cbind(SSP3704160native_background,SSP3704160native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP3704160invasive_background$pa <- 0
SSP3704160native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP3704160invasive_background <- na.omit(SSP3704160invasive_background)
SSP3704160native_background <- na.omit(SSP3704160native_background)




invSSP3704160_randomF_rf_importance_total <- data.frame()
invSSP3704160_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP3704160invasive_with_envi[,colnames(SSP3704160invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP3704160invasive_background[,colnames(SSP3704160invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP3704160rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP3704160_randomF_rf_importance <- importance(invSSP3704160rf_regress)
  invSSP3704160_randomF_rf_importance <- data.frame(names=row.names(invSSP3704160_randomF_rf_importance),invSSP3704160_randomF_rf_importance)
  invSSP3704160_randomF_rf_importance_total <- rbind(invSSP3704160_randomF_rf_importance_total,invSSP3704160_randomF_rf_importance)
  
  invSSP3704160_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP3704160rf_regress))
  invSSP3704160_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP3704160_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP3704160_randomF_rf_evaluation$AUC <- invSSP3704160_erf@auc
  invSSP3704160_randomF_rf_evaluation$cor <- invSSP3704160_erf@cor
  invSSP3704160_randomF_rf_evaluation$kappa <- max(invSSP3704160_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP3704160_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP3704160_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP3704160_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP3704160_randomF_evaluation_total <- rbind(invSSP3704160_randomF_evaluation_total,invSSP3704160_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP3704160_randomF_evaluation_total)
tmpSD <- apply(invSSP3704160_randomF_evaluation_total,2,sd)
invSSP3704160_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP3704160_randomF_evaluation_total_summarized,paste("InvasiveSSP3704160randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)



natSSP3704160_randomF_rf_importance_total <- data.frame()
natSSP3704160_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP3704160native_with_envi[,colnames(SSP3704160native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP3704160native_background[,colnames(SSP3704160native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP3704160rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP3704160_randomF_rf_importance <- importance(natSSP3704160rf_regress)
  natSSP3704160_randomF_rf_importance <- data.frame(names=row.names(natSSP3704160_randomF_rf_importance),natSSP3704160_randomF_rf_importance)
  natSSP3704160_randomF_rf_importance_total <- rbind(natSSP3704160_randomF_rf_importance_total,natSSP3704160_randomF_rf_importance)
  
  natSSP3704160_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP3704160rf_regress))
  natSSP3704160_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP3704160_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP3704160_randomF_rf_evaluation$AUC <- natSSP3704160_erf@auc
  natSSP3704160_randomF_rf_evaluation$cor <- natSSP3704160_erf@cor
  natSSP3704160_randomF_rf_evaluation$kappa <- max(natSSP3704160_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP3704160_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP3704160_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP3704160_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP3704160_randomF_evaluation_total <- rbind(natSSP3704160_randomF_evaluation_total,natSSP3704160_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP3704160_randomF_evaluation_total)
tmpSD <- apply(natSSP3704160_randomF_evaluation_total,2,sd)
natSSP3704160_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP3704160_randomF_evaluation_total_summarized,paste("nativeSSP3704160randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)

#####SSP3706180#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP3706180")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP3706180invasive_with_envi <- invasive_location
SSP3706180native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP3706180invasive_with_envi <- cbind(SSP3706180invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP3706180native_with_envi <- cbind(SSP3706180native_with_envi,native)
}

SSP3706180invasive_with_envi$pa <- 1
SSP3706180native_with_envi$pa <- 1

write.csv(SSP3706180invasive_with_envi, file="SSP3706180invasive_with_envi.csv")
write.csv(SSP3706180native_with_envi, file="SSP3706180native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP3706180invasive_with_envi <- na.omit(SSP3706180invasive_with_envi)
SSP3706180native_with_envi <- na.omit(SSP3706180native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP3706180invasive_with_envi),nrow(SSP3706180native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP3706180background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP3706180background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP3706180background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP3706180background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP3706180background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP3706180background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP3706180background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP3706180background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP3706180invasive_background <- SSP3706180background_invasive_locations
SSP3706180native_background <- SSP3706180background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP3706180invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP3706180background_invasive_locations))
  colnames(SSP3706180invasive_background_tmp) <- envi_filename
  SSP3706180invasive_background <- cbind(SSP3706180invasive_background,SSP3706180invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP3706180native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP3706180background_native_locations))
  colnames(SSP3706180native_background_tmp) <- envi_filename
  SSP3706180native_background <- cbind(SSP3706180native_background,SSP3706180native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP3706180invasive_background$pa <- 0
SSP3706180native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP3706180invasive_background <- na.omit(SSP3706180invasive_background)
SSP3706180native_background <- na.omit(SSP3706180native_background)




invSSP3706180_randomF_rf_importance_total <- data.frame()
invSSP3706180_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP3706180invasive_with_envi[,colnames(SSP3706180invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP3706180invasive_background[,colnames(SSP3706180invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP3706180rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP3706180_randomF_rf_importance <- importance(invSSP3706180rf_regress)
  invSSP3706180_randomF_rf_importance <- data.frame(names=row.names(invSSP3706180_randomF_rf_importance),invSSP3706180_randomF_rf_importance)
  invSSP3706180_randomF_rf_importance_total <- rbind(invSSP3706180_randomF_rf_importance_total,invSSP3706180_randomF_rf_importance)
  
  invSSP3706180_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP3706180rf_regress))
  invSSP3706180_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP3706180_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP3706180_randomF_rf_evaluation$AUC <- invSSP3706180_erf@auc
  invSSP3706180_randomF_rf_evaluation$cor <- invSSP3706180_erf@cor
  invSSP3706180_randomF_rf_evaluation$kappa <- max(invSSP3706180_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP3706180_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP3706180_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP3706180_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP3706180_randomF_evaluation_total <- rbind(invSSP3706180_randomF_evaluation_total,invSSP3706180_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP3706180_randomF_evaluation_total)
tmpSD <- apply(invSSP3706180_randomF_evaluation_total,2,sd)
invSSP3706180_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP3706180_randomF_evaluation_total_summarized,paste("InvasiveSSP3706180randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)





natSSP3706180_randomF_rf_importance_total <- data.frame()
natSSP3706180_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP3706180native_with_envi[,colnames(SSP3706180native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP3706180native_background[,colnames(SSP3706180native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP3706180rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP3706180_randomF_rf_importance <- importance(natSSP3706180rf_regress)
  natSSP3706180_randomF_rf_importance <- data.frame(names=row.names(natSSP3706180_randomF_rf_importance),natSSP3706180_randomF_rf_importance)
  natSSP3706180_randomF_rf_importance_total <- rbind(natSSP3706180_randomF_rf_importance_total,natSSP3706180_randomF_rf_importance)
  
  natSSP3706180_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP3706180rf_regress))
  natSSP3706180_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP3706180_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP3706180_randomF_rf_evaluation$AUC <- natSSP3706180_erf@auc
  natSSP3706180_randomF_rf_evaluation$cor <- natSSP3706180_erf@cor
  natSSP3706180_randomF_rf_evaluation$kappa <- max(natSSP3706180_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP3706180_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP3706180_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP3706180_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP3706180_randomF_evaluation_total <- rbind(natSSP3706180_randomF_evaluation_total,natSSP3706180_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP3706180_randomF_evaluation_total)
tmpSD <- apply(natSSP3706180_randomF_evaluation_total,2,sd)
natSSP3706180_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP3706180_randomF_evaluation_total_summarized,paste("nativeSSP3706180randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)

#####SSP3708100#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP3708100")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP3708100invasive_with_envi <- invasive_location
SSP3708100native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP3708100invasive_with_envi <- cbind(SSP3708100invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP3708100native_with_envi <- cbind(SSP3708100native_with_envi,native)
}

SSP3708100invasive_with_envi$pa <- 1
SSP3708100native_with_envi$pa <- 1

write.csv(SSP3708100invasive_with_envi, file="SSP3708100invasive_with_envi.csv")
write.csv(SSP3708100native_with_envi, file="SSP3708100native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP3708100invasive_with_envi <- na.omit(SSP3708100invasive_with_envi)
SSP3708100native_with_envi <- na.omit(SSP3708100native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP3708100invasive_with_envi),nrow(SSP3708100native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP3708100background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP3708100background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP3708100background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP3708100background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP3708100background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP3708100background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP3708100background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP3708100background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP3708100invasive_background <- SSP3708100background_invasive_locations
SSP3708100native_background <- SSP3708100background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP3708100invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP3708100background_invasive_locations))
  colnames(SSP3708100invasive_background_tmp) <- envi_filename
  SSP3708100invasive_background <- cbind(SSP3708100invasive_background,SSP3708100invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP3708100native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP3708100background_native_locations))
  colnames(SSP3708100native_background_tmp) <- envi_filename
  SSP3708100native_background <- cbind(SSP3708100native_background,SSP3708100native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP3708100invasive_background$pa <- 0
SSP3708100native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP3708100invasive_background <- na.omit(SSP3708100invasive_background)
SSP3708100native_background <- na.omit(SSP3708100native_background)




invSSP3708100_randomF_rf_importance_total <- data.frame()
invSSP3708100_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP3708100invasive_with_envi[,colnames(SSP3708100invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP3708100invasive_background[,colnames(SSP3708100invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP3708100rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP3708100_randomF_rf_importance <- importance(invSSP3708100rf_regress)
  invSSP3708100_randomF_rf_importance <- data.frame(names=row.names(invSSP3708100_randomF_rf_importance),invSSP3708100_randomF_rf_importance)
  invSSP3708100_randomF_rf_importance_total <- rbind(invSSP3708100_randomF_rf_importance_total,invSSP3708100_randomF_rf_importance)
  
  invSSP3708100_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP3708100rf_regress))
  invSSP3708100_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP3708100_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP3708100_randomF_rf_evaluation$AUC <- invSSP3708100_erf@auc
  invSSP3708100_randomF_rf_evaluation$cor <- invSSP3708100_erf@cor
  invSSP3708100_randomF_rf_evaluation$kappa <- max(invSSP3708100_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP3708100_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP3708100_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP3708100_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP3708100_randomF_evaluation_total <- rbind(invSSP3708100_randomF_evaluation_total,invSSP3708100_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP3708100_randomF_evaluation_total)
tmpSD <- apply(invSSP3708100_randomF_evaluation_total,2,sd)
invSSP3708100_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP3708100_randomF_evaluation_total_summarized,paste("InvasiveSSP3708100randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)





natSSP3708100_randomF_rf_importance_total <- data.frame()
natSSP3708100_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP3708100native_with_envi[,colnames(SSP3708100native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP3708100native_background[,colnames(SSP3708100native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP3708100rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP3708100_randomF_rf_importance <- importance(natSSP3708100rf_regress)
  natSSP3708100_randomF_rf_importance <- data.frame(names=row.names(natSSP3708100_randomF_rf_importance),natSSP3708100_randomF_rf_importance)
  natSSP3708100_randomF_rf_importance_total <- rbind(natSSP3708100_randomF_rf_importance_total,natSSP3708100_randomF_rf_importance)
  
  natSSP3708100_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP3708100rf_regress))
  natSSP3708100_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP3708100_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP3708100_randomF_rf_evaluation$AUC <- natSSP3708100_erf@auc
  natSSP3708100_randomF_rf_evaluation$cor <- natSSP3708100_erf@cor
  natSSP3708100_randomF_rf_evaluation$kappa <- max(natSSP3708100_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP3708100_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP3708100_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP3708100_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP3708100_randomF_evaluation_total <- rbind(natSSP3708100_randomF_evaluation_total,natSSP3708100_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP3708100_randomF_evaluation_total)
tmpSD <- apply(natSSP3708100_randomF_evaluation_total,2,sd)
natSSP3708100_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP3708100_randomF_evaluation_total_summarized,paste("nativeSSP3708100randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)



#####SSP5852140#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP5852140")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP5852140invasive_with_envi <- invasive_location
SSP5852140native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP5852140invasive_with_envi <- cbind(SSP5852140invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP5852140native_with_envi <- cbind(SSP5852140native_with_envi,native)
}

SSP5852140invasive_with_envi$pa <- 1
SSP5852140native_with_envi$pa <- 1

write.csv(SSP5852140invasive_with_envi, file="SSP5852140invasive_with_envi.csv")
write.csv(SSP5852140native_with_envi, file="SSP5852140native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP5852140invasive_with_envi <- na.omit(SSP5852140invasive_with_envi)
SSP5852140native_with_envi <- na.omit(SSP5852140native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP5852140invasive_with_envi),nrow(SSP5852140native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP5852140background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP5852140background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP5852140background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP5852140background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP5852140background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP5852140background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP5852140background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP5852140background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP5852140invasive_background <- SSP5852140background_invasive_locations
SSP5852140native_background <- SSP5852140background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP5852140invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP5852140background_invasive_locations))
  colnames(SSP5852140invasive_background_tmp) <- envi_filename
  SSP5852140invasive_background <- cbind(SSP5852140invasive_background,SSP5852140invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP5852140native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP5852140background_native_locations))
  colnames(SSP5852140native_background_tmp) <- envi_filename
  SSP5852140native_background <- cbind(SSP5852140native_background,SSP5852140native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP5852140invasive_background$pa <- 0
SSP5852140native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP5852140invasive_background <- na.omit(SSP5852140invasive_background)
SSP5852140native_background <- na.omit(SSP5852140native_background)




invSSP5852140_randomF_rf_importance_total <- data.frame()
invSSP5852140_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP5852140invasive_with_envi[,colnames(SSP5852140invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP5852140invasive_background[,colnames(SSP5852140invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP5852140rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP5852140_randomF_rf_importance <- importance(invSSP5852140rf_regress)
  invSSP5852140_randomF_rf_importance <- data.frame(names=row.names(invSSP5852140_randomF_rf_importance),invSSP5852140_randomF_rf_importance)
  invSSP5852140_randomF_rf_importance_total <- rbind(invSSP5852140_randomF_rf_importance_total,invSSP5852140_randomF_rf_importance)
  
  invSSP5852140_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP5852140rf_regress))
  invSSP5852140_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP5852140_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP5852140_randomF_rf_evaluation$AUC <- invSSP5852140_erf@auc
  invSSP5852140_randomF_rf_evaluation$cor <- invSSP5852140_erf@cor
  invSSP5852140_randomF_rf_evaluation$kappa <- max(invSSP5852140_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP5852140_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP5852140_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP5852140_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP5852140_randomF_evaluation_total <- rbind(invSSP5852140_randomF_evaluation_total,invSSP5852140_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP5852140_randomF_evaluation_total)
tmpSD <- apply(invSSP5852140_randomF_evaluation_total,2,sd)
invSSP5852140_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP5852140_randomF_evaluation_total_summarized,paste("InvasiveSSP5852140randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)





natSSP5852140_randomF_rf_importance_total <- data.frame()
natSSP5852140_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP5852140native_with_envi[,colnames(SSP5852140native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP5852140native_background[,colnames(SSP5852140native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP5852140rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP5852140_randomF_rf_importance <- importance(natSSP5852140rf_regress)
  natSSP5852140_randomF_rf_importance <- data.frame(names=row.names(natSSP5852140_randomF_rf_importance),natSSP5852140_randomF_rf_importance)
  natSSP5852140_randomF_rf_importance_total <- rbind(natSSP5852140_randomF_rf_importance_total,natSSP5852140_randomF_rf_importance)
  
  natSSP5852140_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP5852140rf_regress))
  natSSP5852140_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP5852140_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP5852140_randomF_rf_evaluation$AUC <- natSSP5852140_erf@auc
  natSSP5852140_randomF_rf_evaluation$cor <- natSSP5852140_erf@cor
  natSSP5852140_randomF_rf_evaluation$kappa <- max(natSSP5852140_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP5852140_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP5852140_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP5852140_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP5852140_randomF_evaluation_total <- rbind(natSSP5852140_randomF_evaluation_total,natSSP5852140_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP5852140_randomF_evaluation_total)
tmpSD <- apply(natSSP5852140_randomF_evaluation_total,2,sd)
natSSP5852140_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP5852140_randomF_evaluation_total_summarized,paste("nativeSSP5852140randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)

#####SSP5854160#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP5854160")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP5854160invasive_with_envi <- invasive_location
SSP5854160native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP5854160invasive_with_envi <- cbind(SSP5854160invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP5854160native_with_envi <- cbind(SSP5854160native_with_envi,native)
}

SSP5854160invasive_with_envi$pa <- 1
SSP5854160native_with_envi$pa <- 1

write.csv(SSP5854160invasive_with_envi, file="SSP5854160invasive_with_envi.csv")
write.csv(SSP5854160native_with_envi, file="SSP5854160native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP5854160invasive_with_envi <- na.omit(SSP5854160invasive_with_envi)
SSP5854160native_with_envi <- na.omit(SSP5854160native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP5854160invasive_with_envi),nrow(SSP5854160native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP5854160background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP5854160background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP5854160background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP5854160background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP5854160background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP5854160background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP5854160background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP5854160background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP5854160invasive_background <- SSP5854160background_invasive_locations
SSP5854160native_background <- SSP5854160background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP5854160invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP5854160background_invasive_locations))
  colnames(SSP5854160invasive_background_tmp) <- envi_filename
  SSP5854160invasive_background <- cbind(SSP5854160invasive_background,SSP5854160invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP5854160native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP5854160background_native_locations))
  colnames(SSP5854160native_background_tmp) <- envi_filename
  SSP5854160native_background <- cbind(SSP5854160native_background,SSP5854160native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP5854160invasive_background$pa <- 0
SSP5854160native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP5854160invasive_background <- na.omit(SSP5854160invasive_background)
SSP5854160native_background <- na.omit(SSP5854160native_background)




invSSP5854160_randomF_rf_importance_total <- data.frame()
invSSP5854160_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP5854160invasive_with_envi[,colnames(SSP5854160invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP5854160invasive_background[,colnames(SSP5854160invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP5854160rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP5854160_randomF_rf_importance <- importance(invSSP5854160rf_regress)
  invSSP5854160_randomF_rf_importance <- data.frame(names=row.names(invSSP5854160_randomF_rf_importance),invSSP5854160_randomF_rf_importance)
  invSSP5854160_randomF_rf_importance_total <- rbind(invSSP5854160_randomF_rf_importance_total,invSSP5854160_randomF_rf_importance)
  
  invSSP5854160_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP5854160rf_regress))
  invSSP5854160_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP5854160_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP5854160_randomF_rf_evaluation$AUC <- invSSP5854160_erf@auc
  invSSP5854160_randomF_rf_evaluation$cor <- invSSP5854160_erf@cor
  invSSP5854160_randomF_rf_evaluation$kappa <- max(invSSP5854160_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP5854160_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP5854160_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP5854160_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP5854160_randomF_evaluation_total <- rbind(invSSP5854160_randomF_evaluation_total,invSSP5854160_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP5854160_randomF_evaluation_total)
tmpSD <- apply(invSSP5854160_randomF_evaluation_total,2,sd)
invSSP5854160_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP5854160_randomF_evaluation_total_summarized,paste("InvasiveSSP5854160randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)





natSSP5854160_randomF_rf_importance_total <- data.frame()
natSSP5854160_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP5854160native_with_envi[,colnames(SSP5854160native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP5854160native_background[,colnames(SSP5854160native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP5854160rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP5854160_randomF_rf_importance <- importance(natSSP5854160rf_regress)
  natSSP5854160_randomF_rf_importance <- data.frame(names=row.names(natSSP5854160_randomF_rf_importance),natSSP5854160_randomF_rf_importance)
  natSSP5854160_randomF_rf_importance_total <- rbind(natSSP5854160_randomF_rf_importance_total,natSSP5854160_randomF_rf_importance)
  
  natSSP5854160_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP5854160rf_regress))
  natSSP5854160_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP5854160_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP5854160_randomF_rf_evaluation$AUC <- natSSP5854160_erf@auc
  natSSP5854160_randomF_rf_evaluation$cor <- natSSP5854160_erf@cor
  natSSP5854160_randomF_rf_evaluation$kappa <- max(natSSP5854160_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP5854160_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP5854160_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP5854160_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP5854160_randomF_evaluation_total <- rbind(natSSP5854160_randomF_evaluation_total,natSSP5854160_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP5854160_randomF_evaluation_total)
tmpSD <- apply(natSSP5854160_randomF_evaluation_total,2,sd)
natSSP5854160_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP5854160_randomF_evaluation_total_summarized,paste("nativeSSP5854160randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)

#####SSP5856180#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP5856180")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP5856180invasive_with_envi <- invasive_location
SSP5856180native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP5856180invasive_with_envi <- cbind(SSP5856180invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP5856180native_with_envi <- cbind(SSP5856180native_with_envi,native)
}

SSP5856180invasive_with_envi$pa <- 1
SSP5856180native_with_envi$pa <- 1

write.csv(SSP5856180invasive_with_envi, file="SSP5856180invasive_with_envi.csv")
write.csv(SSP5856180native_with_envi, file="SSP5856180native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP5856180invasive_with_envi <- na.omit(SSP5856180invasive_with_envi)
SSP5856180native_with_envi <- na.omit(SSP5856180native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP5856180invasive_with_envi),nrow(SSP5856180native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP5856180background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP5856180background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP5856180background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP5856180background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP5856180background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP5856180background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP5856180background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP5856180background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP5856180invasive_background <- SSP5856180background_invasive_locations
SSP5856180native_background <- SSP5856180background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP5856180invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP5856180background_invasive_locations))
  colnames(SSP5856180invasive_background_tmp) <- envi_filename
  SSP5856180invasive_background <- cbind(SSP5856180invasive_background,SSP5856180invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP5856180native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP5856180background_native_locations))
  colnames(SSP5856180native_background_tmp) <- envi_filename
  SSP5856180native_background <- cbind(SSP5856180native_background,SSP5856180native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP5856180invasive_background$pa <- 0
SSP5856180native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP5856180invasive_background <- na.omit(SSP5856180invasive_background)
SSP5856180native_background <- na.omit(SSP5856180native_background)




invSSP5856180_randomF_rf_importance_total <- data.frame()
invSSP5856180_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP5856180invasive_with_envi[,colnames(SSP5856180invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP5856180invasive_background[,colnames(SSP5856180invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP5856180rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP5856180_randomF_rf_importance <- importance(invSSP5856180rf_regress)
  invSSP5856180_randomF_rf_importance <- data.frame(names=row.names(invSSP5856180_randomF_rf_importance),invSSP5856180_randomF_rf_importance)
  invSSP5856180_randomF_rf_importance_total <- rbind(invSSP5856180_randomF_rf_importance_total,invSSP5856180_randomF_rf_importance)
  
  invSSP5856180_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP5856180rf_regress))
  invSSP5856180_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP5856180_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP5856180_randomF_rf_evaluation$AUC <- invSSP5856180_erf@auc
  invSSP5856180_randomF_rf_evaluation$cor <- invSSP5856180_erf@cor
  invSSP5856180_randomF_rf_evaluation$kappa <- max(invSSP5856180_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP5856180_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP5856180_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP5856180_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP5856180_randomF_evaluation_total <- rbind(invSSP5856180_randomF_evaluation_total,invSSP5856180_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP5856180_randomF_evaluation_total)
tmpSD <- apply(invSSP5856180_randomF_evaluation_total,2,sd)
invSSP5856180_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP5856180_randomF_evaluation_total_summarized,paste("InvasiveSSP5856180randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)





natSSP5856180_randomF_rf_importance_total <- data.frame()
natSSP5856180_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP5856180native_with_envi[,colnames(SSP5856180native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP5856180native_background[,colnames(SSP5856180native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP5856180rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP5856180_randomF_rf_importance <- importance(natSSP5856180rf_regress)
  natSSP5856180_randomF_rf_importance <- data.frame(names=row.names(natSSP5856180_randomF_rf_importance),natSSP5856180_randomF_rf_importance)
  natSSP5856180_randomF_rf_importance_total <- rbind(natSSP5856180_randomF_rf_importance_total,natSSP5856180_randomF_rf_importance)
  
  natSSP5856180_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP5856180rf_regress))
  natSSP5856180_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP5856180_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP5856180_randomF_rf_evaluation$AUC <- natSSP5856180_erf@auc
  natSSP5856180_randomF_rf_evaluation$cor <- natSSP5856180_erf@cor
  natSSP5856180_randomF_rf_evaluation$kappa <- max(natSSP5856180_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP5856180_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP5856180_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP5856180_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP5856180_randomF_evaluation_total <- rbind(natSSP5856180_randomF_evaluation_total,natSSP5856180_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP5856180_randomF_evaluation_total)
tmpSD <- apply(natSSP5856180_randomF_evaluation_total,2,sd)
natSSP5856180_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP5856180_randomF_evaluation_total_summarized,paste("nativeSSP5856180randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)

#####SSP5858100#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts/FutureBCData/SSP5858100")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

SSP5858100invasive_with_envi <- invasive_location
SSP5858100native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  SSP5858100invasive_with_envi <- cbind(SSP5858100invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  SSP5858100native_with_envi <- cbind(SSP5858100native_with_envi,native)
}

SSP5858100invasive_with_envi$pa <- 1
SSP5858100native_with_envi$pa <- 1

write.csv(SSP5858100invasive_with_envi, file="SSP5858100invasive_with_envi.csv")
write.csv(SSP5858100native_with_envi, file="SSP5858100native_with_envi.csv")

#Remove rows with missing data for observations points.
SSP5858100invasive_with_envi <- na.omit(SSP5858100invasive_with_envi)
SSP5858100native_with_envi <- na.omit(SSP5858100native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(SSP5858100invasive_with_envi),nrow(SSP5858100native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
SSP5858100background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP5858100background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP5858100background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP5858100background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

SSP5858100background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(SSP5858100background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
SSP5858100background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
SSP5858100background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










SSP5858100invasive_background <- SSP5858100background_invasive_locations
SSP5858100native_background <- SSP5858100background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  SSP5858100invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP5858100background_invasive_locations))
  colnames(SSP5858100invasive_background_tmp) <- envi_filename
  SSP5858100invasive_background <- cbind(SSP5858100invasive_background,SSP5858100invasive_background_tmp)
  #Extract environmental values at native species locations.
  SSP5858100native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),SSP5858100background_native_locations))
  colnames(SSP5858100native_background_tmp) <- envi_filename
  SSP5858100native_background <- cbind(SSP5858100native_background,SSP5858100native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
SSP5858100invasive_background$pa <- 0
SSP5858100native_background$pa <- 0



#Remove rows with missing data for observations points.
SSP5858100invasive_background <- na.omit(SSP5858100invasive_background)
SSP5858100native_background <- na.omit(SSP5858100native_background)




invSSP5858100_randomF_rf_importance_total <- data.frame()
invSSP5858100_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP5858100invasive_with_envi[,colnames(SSP5858100invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP5858100invasive_background[,colnames(SSP5858100invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  invSSP5858100rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invSSP5858100_randomF_rf_importance <- importance(invSSP5858100rf_regress)
  invSSP5858100_randomF_rf_importance <- data.frame(names=row.names(invSSP5858100_randomF_rf_importance),invSSP5858100_randomF_rf_importance)
  invSSP5858100_randomF_rf_importance_total <- rbind(invSSP5858100_randomF_rf_importance_total,invSSP5858100_randomF_rf_importance)
  
  invSSP5858100_erf <- suppressWarnings(evaluate(testpres,testbackgr,invSSP5858100rf_regress))
  invSSP5858100_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invSSP5858100_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invSSP5858100_randomF_rf_evaluation$AUC <- invSSP5858100_erf@auc
  invSSP5858100_randomF_rf_evaluation$cor <- invSSP5858100_erf@cor
  invSSP5858100_randomF_rf_evaluation$kappa <- max(invSSP5858100_erf@kappa)
  # Calculate Yule's Q.
  tmp <- invSSP5858100_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invSSP5858100_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invSSP5858100_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  invSSP5858100_randomF_evaluation_total <- rbind(invSSP5858100_randomF_evaluation_total,invSSP5858100_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invSSP5858100_randomF_evaluation_total)
tmpSD <- apply(invSSP5858100_randomF_evaluation_total,2,sd)
invSSP5858100_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invSSP5858100_randomF_evaluation_total_summarized,paste("InvasiveSSP5858100randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)





natSSP5858100_randomF_rf_importance_total <- data.frame()
natSSP5858100_randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- SSP5858100native_with_envi[,colnames(SSP5858100native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- SSP5858100native_background[,colnames(SSP5858100native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(pres_subset,5)
  pres_train <- pres_subset[group!=1,]
  pres_test <- pres_subset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(abs_subset,5)
  backgr_train <- abs_subset[group!=1,]
  backgr_test <- abs_subset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envi_train <- rbind(pres_train,backgr_train)
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- natasive_with_envi[,colnames(natasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- natasive_background[,colnames(natasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  # 
  # #Construct a training and testing set for the presence data.
  # group <- kfold(pres_subset,5)
  # pres_train <- pres_subset[group!=1,]
  # pres_test <- pres_subset[group==1,]
  # 
  # #Construct a training and testing set for the pseudo-absence data.
  # group <- kfold(abs_subset,5)
  # backgr_train <- abs_subset[group!=1,]
  # backgr_test <- abs_subset[group==1,]
  # 
  #Construct presence / pseudo-absence training sets.
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  natSSP5858100rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natSSP5858100_randomF_rf_importance <- importance(natSSP5858100rf_regress)
  natSSP5858100_randomF_rf_importance <- data.frame(names=row.names(natSSP5858100_randomF_rf_importance),natSSP5858100_randomF_rf_importance)
  natSSP5858100_randomF_rf_importance_total <- rbind(natSSP5858100_randomF_rf_importance_total,natSSP5858100_randomF_rf_importance)
  
  natSSP5858100_erf <- suppressWarnings(evaluate(testpres,testbackgr,natSSP5858100rf_regress))
  natSSP5858100_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natSSP5858100_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natSSP5858100_randomF_rf_evaluation$AUC <- natSSP5858100_erf@auc
  natSSP5858100_randomF_rf_evaluation$cor <- natSSP5858100_erf@cor
  natSSP5858100_randomF_rf_evaluation$kappa <- max(natSSP5858100_erf@kappa)
  # Calculate Yule's Q.
  tmp <- natSSP5858100_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natSSP5858100_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natSSP5858100_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  natSSP5858100_randomF_evaluation_total <- rbind(natSSP5858100_randomF_evaluation_total,natSSP5858100_randomF_rf_evaluation)
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natSSP5858100_randomF_evaluation_total)
tmpSD <- apply(natSSP5858100_randomF_evaluation_total,2,sd)
natSSP5858100_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natSSP5858100_randomF_evaluation_total_summarized,paste("nativeSSP5858100randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)


