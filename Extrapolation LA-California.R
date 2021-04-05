#Extrapolation from LA to California
##########Data Processing###########
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species")

ca_input <- read.csv("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/CaliforniaObservations.csv")
InvasivePlants <- read.csv("InvasivePlants.csv")
InvasivePlants <- InvasivePlants$ï..ScientificName
InvasivePlants
NativePlants <- read.csv("NativePlants.csv")
NativePlants <- NativePlants$ï..ScientificName
NativePlants


ca_InvasiveObservations <- ca_input[ca_input$verbatimScientificName %in% InvasivePlants, ]
ca_NativeObservations <- ca_input[ca_input$verbatimScientificName %in% NativePlants, ]


write.csv(ca_InvasiveObservations, file="ca_InvasiveObservations")
write.csv(ca_NativeObservations, file="ca_NativeObservations")



ca_amt <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/amt.tif")
ca_ap <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/ap.tif")
ca_bldfie <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/bldfie.tif")
ca_cecsol <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/cecsol.tif")
ca_clyppt <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/clyppt.tif")
ca_hfp <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/hfp.tif")
ca_imprv <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/imprv.tif")
ca_isoth <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/isoth.tif")
ca_mdr <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/mdr.tif")
ca_mtcm <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/mtcm.tif")
ca_mtcq <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/mtcq.tif")
ca_mtdq <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/mtdq.tif")
ca_mtwarmq <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/mtwarmq.tif")
ca_mtwetq <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/mtwetq.tif")
ca_mtwm <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/mtwm.tif")
ca_ntot <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/ntot.tif")
ca_orcdrc <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/orcdrc.tif")
ca_pcq <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/pcq.tif")
ca_pdm <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/pdm.tif")
ca_pdq <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/pdq.tif")
ca_phihox <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/phihox.tif")
ca_ps <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/ps.tif")
ca_ptrcv <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/ptrcv.tif")
ca_pwarmq <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/pwarmq.tif")
ca_pwetq <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/pwetq.tif")
ca_pwm <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/pwm.tif")
ca_sndppt <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/sndppt.tif")
ca_tar <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/tar.tif")
ca_ts <- raster("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/California/ts.tif")


ca_envi_map_layers <- list.files("./California", full.names = TRUE)

ca_envi_layers_stack <- stack(c(ca_envi_map_layers))

#Extracting envi values from rasters at location of native plants
ca_invasive_location <- ca_InvasiveObservations[, c("decimalLongitude", "decimalLatitude")]
ca_native_location <- ca_NativeObservations[, c("decimalLongitude", "decimalLatitude")]



ca_invasive_with_envi <- ca_invasive_location
ca_native_with_envi <- ca_native_location
for(ca_envi_map_layer in ca_envi_map_layers){
  ca_envi_filename <- gsub("./California/","",gsub(".tif","",ca_envi_map_layer))
  # invasive species
  ca_invasive <- as.data.frame(raster::extract(raster(ca_envi_map_layer),ca_invasive_location))
  colnames(ca_invasive) <- ca_envi_filename
  ca_invasive_with_envi <- cbind(ca_invasive_with_envi,ca_invasive)
  # native species
  ca_native <- as.data.frame(raster::extract(raster(ca_envi_map_layer),ca_native_location))
  colnames(ca_native) <- ca_envi_filename
  ca_native_with_envi <- cbind(ca_native_with_envi,ca_native)
}

ca_invasive_with_envi$pa <- 1
ca_native_with_envi$pa <- 1

write.csv(ca_invasive_with_envi, file="ca_invasive_with_envi.csv")
write.csv(ca_native_with_envi, file="ca_native_with_envi.csv")

#Remove rows with missing data for observations points.
ca_invasive_with_envi <- na.omit(ca_invasive_with_envi)
ca_native_with_envi <- na.omit(ca_native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(ca_invasive_with_envi),nrow(ca_native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
ca_background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(ca_background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
ca_background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
ca_background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

ca_background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(ca_background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
ca_background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
ca_background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










ca_invasive_background <- ca_background_invasive_locations
ca_native_background <- ca_background_native_locations
for(ca_envi_map_layer in ca_envi_map_layers){
  ca_envi_filename <- gsub("./California/","",gsub(".tif","",ca_envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  ca_invasive_background_tmp <- as.data.frame(raster::extract(raster(ca_envi_map_layer),ca_background_invasive_locations))
  colnames(ca_invasive_background_tmp) <- ca_envi_filename
  ca_invasive_background <- cbind(ca_invasive_background,ca_invasive_background_tmp)
  #Extract environmental values at native species locations.
  ca_native_background_tmp <- as.data.frame(raster::extract(raster(ca_envi_map_layer),ca_background_native_locations))
  colnames(ca_native_background_tmp) <- ca_envi_filename
  ca_native_background <- cbind(ca_native_background,ca_native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
ca_invasive_background$pa <- 0
ca_native_background$pa <- 0



#Remove rows with missing data for observations points.
ca_invasive_background <- na.omit(ca_invasive_background)
ca_native_background <- na.omit(ca_native_background)




########Model Evaluation#########
#Now used ALL LA data (rather than 80%) to train the model
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/EnvironmentalFactors")

envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)

redo_invasive_with_envi <- invasive_location
redo_native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  redo_invasive_with_envi <- cbind(redo_invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  redo_native_with_envi <- cbind(redo_native_with_envi,native)
}

redo_invasive_with_envi$pa <- 1
redo_native_with_envi$pa <- 1

write.csv(redo_invasive_with_envi, file="redo_invasive_with_envi.csv")
write.csv(redo_native_with_envi, file="redo_native_with_envi.csv")

#Remove rows with missing data for observations points.
redo_invasive_with_envi <- na.omit(redo_invasive_with_envi)
redo_native_with_envi <- na.omit(redo_native_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(redo_invasive_with_envi),nrow(redo_native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
redo_background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(redo_background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
redo_background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
redo_background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

redo_background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(redo_background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
redo_background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
redo_background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










redo_invasive_background <- redo_background_invasive_locations
redo_native_background <- redo_background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  redo_invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),redo_background_invasive_locations))
  colnames(redo_invasive_background_tmp) <- envi_filename
  redo_invasive_background <- cbind(redo_invasive_background,redo_invasive_background_tmp)
  #Extract environmental values at native species locations.
  redo_native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),redo_background_native_locations))
  colnames(redo_native_background_tmp) <- envi_filename
  redo_native_background <- cbind(redo_native_background,redo_native_background_tmp)
}

#Designate a presence/absence column for the pseudo-absence points.
redo_invasive_background$pa <- 0
redo_native_background$pa <- 0



#Remove rows with missing data for observations points.
redo_invasive_background <- na.omit(redo_invasive_background)
redo_native_background <- na.omit(redo_native_background)




invext__randomF_rf_importance_total <- data.frame()
invext__randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- redo_invasive_with_envi[,colnames(redo_invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- redo_invasive_background[,colnames(redo_invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.

  pres_train <- pres_subset
  pres_test <- ca_invasive_with_envi[,colnames(ca_invasive_with_envi) %in% c(envi_filename,"pa")]
  pres_test <- pres_test[sample(nrow(pres_test),sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the pseudo-absence data.
 
  backgr_train <- abs_subset
  backgr_test <- ca_invasive_background[,colnames(ca_invasive_background) %in% c(envi_filename,"pa")]
  backgr_test <- backgr_test[sample(nrow(backgr_test),10*sample_num),] #Subsample rows.
  
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
  
  invext_rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invext__randomF_rf_importance <- importance(invext_rf_regress)
  invext__randomF_rf_importance <- data.frame(names=row.names(invext__randomF_rf_importance),invext__randomF_rf_importance)
  invext__randomF_rf_importance_total <- rbind(invext__randomF_rf_importance_total,invext__randomF_rf_importance)
  
  invext__erf <- suppressWarnings(evaluate(testpres,testbackgr,invext_rf_regress))
  invext__randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invext__randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invext__randomF_rf_evaluation$AUC <- invext__erf@auc
  invext__randomF_rf_evaluation$cor <- invext__erf@cor
  invext__randomF_rf_evaluation$kappa <- max(invext__erf@kappa)
  # Calculate Yule's Q.
  tmp <- invext__erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invext__randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invext__randomF_rf_evaluation$TSS <- mean(invext__erf@TPR,na.rm=T)+mean(invext__erf@TNR,na.rm=T)-1
  invext__randomF_evaluation_total <- rbind(invext__randomF_evaluation_total,invext__randomF_rf_evaluation)
}

p <- as.matrix(invext_rf_regress$importance)   
ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]]) 
dotchart(p[ord,1], main="Invasive Variable Importance")  

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invext__randomF_evaluation_total)
tmpSD <- apply(invext__randomF_evaluation_total,2,sd)
invext__randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invext__randomF_evaluation_total_summarized,paste("InvasiveCALIF_randomFParsimonyFinalRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)





natext__randomF_rf_importance_total <- data.frame()
natext__randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- redo_native_with_envi[,colnames(redo_native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- redo_native_background[,colnames(redo_native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  
  pres_train <- pres_subset
  pres_test <- ca_native_with_envi[,colnames(ca_native_with_envi) %in% c(envi_filename,"pa")]
  # pres_test <- pres_test[sample(nrow(pres_test),sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the pseudo-absence data.
  
  backgr_train <- abs_subset
  backgr_test <- ca_native_background[,colnames(ca_native_background) %in% c(envi_filename,"pa")]
  # backgr_test <- backgr_test[sample(nrow(backgr_test),10*sample_num),] #Subsample rows.
  
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
  
  natext_rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natext__randomF_rf_importance <- importance(natext_rf_regress)
  natext__randomF_rf_importance <- data.frame(names=row.names(natext__randomF_rf_importance),natext__randomF_rf_importance)
  natext__randomF_rf_importance_total <- rbind(natext__randomF_rf_importance_total,natext__randomF_rf_importance)
  
  natext__erf <- suppressWarnings(evaluate(testpres,testbackgr,natext_rf_regress))
  natext__randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natext__randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natext__randomF_rf_evaluation$AUC <- natext__erf@auc
  natext__randomF_rf_evaluation$cor <- natext__erf@cor
  natext__randomF_rf_evaluation$kappa <- max(natext__erf@kappa)
  # Calculate Yule's Q.
  tmp <- natext__erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natext__randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natext__randomF_rf_evaluation$TSS <- mean(natext__erf@TPR,na.rm=T)+mean(natext__erf@TNR,na.rm=T)-1
  natext__randomF_evaluation_total <- rbind(natext__randomF_evaluation_total,natext__randomF_rf_evaluation)
  
  
  p <- as.matrix(natext_rf_regress$importance)   
  ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]]) 
  dotchart(p[ord,1], main="Native Variable Importance")  
  
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natext__randomF_evaluation_total)
tmpSD <- apply(natext__randomF_evaluation_total,2,sd)
natext__randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natext__randomF_evaluation_total_summarized,paste("nativeCALIF_randomFParsimonyFinalRFEvaluations2.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)

































