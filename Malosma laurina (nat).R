setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species")
require(raster)
# install.packages("raster")
require(sp)
# install.packages("sp")
require(rgdal)
# install.packages("rgdal")
require(rfUtilities)
# install.packages("rfUtilities")
require(randomForest)
# install.packages("randomForest")
require(dismo)
# install.packages("dismo")
library(plyr)
# install.packages("plyr")
library(dplyr)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("cowplot")
library(cowplot)
# install.packages("maptools")
library(maptools)
# install.packages("rgeos")
library(rgeos)
# install.packages('reprtree')



input <- read.csv("OriginalPlantData.csv")
InvasivePlants <- read.csv("InvasivePlants.csv")
InvasivePlants <- InvasivePlants$ï..ScientificName
#I have no idea why the column name is like that...Weird Chinese characters:/
#and the system is really stubborn XD Even if I changed the language in R and RStudio
#To English,it still returns me Chinese in the console
#Which is totally fine but will cause some translation issues as I'll need to covert the error
#messages back to English without adequate knowledge on computer science jargons XD

#####Find Species with the Most data#####
plant_freq <- sort(table(input$scientificName))
#1 Malosma laurina Engl. (NATIVE)
#2 Heteromeles arbutifolia (Lindl.) M.Roem (nat-not in list)
#3 Eriogonum fasciculatum (nat-not in list)
#4 Quercus agrifolia Nee (NATIVE)
#5 Artemisia californica (nat-not in the list)
#6 
#7 Nicotiana glauca (!!!!!INVASIVE BUT NOT ON THE LIST!!!!)
#8 Ricinus communis (!!!!!INVASIVE BUT NOT ON THE LIST!!!!)
#1X Marrubium vulgar (INVASIVE)


####Decision to run:
#Invasive:
#1. Nicotiana glauca (NGinv)
#2. Ricinus communis (MLnat)
#3. Marrubium vulgar (MLnat)

#Native:
#1. Malosma laurina Engl.(MLnat)
#2. Heteromeles arbutifolia (HAnat)
#3. Eriogonum fasciculatum (EFnat)


######DON'T CARE TO MUCH ABOUT NAME MATHCES--THAT MAY BE A PROBLEM WITH YOUR SEARCH METHOD AND NOT THE DATA ITSELF

Malosmalaurina <- "Malosma laurina"
MLnatObservations <- input[input$verbatimScientificName %in% Malosmalaurina, ]

write.csv(MLnatObservations, file="MLnatObservations")

envi_map_layers <- list.files("./EnvironmentalFactors", full.names = TRUE)



#Creating a stack of the environmental rasters
#envi_map_layers <- data.frame(envi_map_layers = unlist(envi_map_layers))
envi_layers_stack <- stack(c(envi_map_layers))

#Extracting envi values from rasters at location of native plants
MLnat_location <- MLnatObservations[, c("decimalLongitude", "decimalLatitude")]








MLnat_with_envi <- MLnat_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./EnvironmentalFactors/","",gsub(".tif","",envi_map_layer))
  # MLnat species
  MLnat <- as.data.frame(raster::extract(raster(envi_map_layer),MLnat_location))
  colnames(MLnat) <- envi_filename
  MLnat_with_envi <- cbind(MLnat_with_envi,MLnat)
}

MLnat_with_envi$pa <- 1

# write.csv(MLnat_with_envi, file="MLnat_with_envi.csv")
# write.csv(native_with_envi, file="native_with_envi.csv")

#Remove rows with missing data for observations points.
MLnat_with_envi <- na.omit(MLnat_with_envi)

# MLnat_with_envi$X <- NULL
# native_with_envi$X <- NULL

#Create a set of pseudo-absence points for extracting environmental values.
#Designate the longitude and latitude bounds of the study area.
#Doesit matter if the extent of maplayers isn't as big?
long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or MLnat species observations.
obs_num <- 10*max(nrow(MLnat_with_envi))

#Create empty dataframe for random background points for MLnats.
#Make it much larger than the set of presence points.
background_MLnat_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(background_MLnat_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
background_MLnat_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
background_MLnat_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










MLnat_background <- background_MLnat_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./EnvironmentalFactors/","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at MLnat species locations.
  MLnat_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),background_MLnat_locations))
  colnames(MLnat_background_tmp) <- envi_filename
  MLnat_background <- cbind(MLnat_background,MLnat_background_tmp)
  #Extract environmental values at native species locations.
}

MLnat_background$pa <- 0



#Remove rows with missing data for observations points.
MLnat_background <- na.omit(MLnat_background)


#Run random forest models on your MLnat species data.
rf_importance_total <- data.frame() # Initialize an empty data frame to collect random forest statistics.
model_num <- 100 #Designate how many data subsets to run random forest on.
sample_num <- 100 #Designate how many points to subsample your data on.


envi_filename <- gsub("./EnvironmentalFactors/","",gsub(".tif","",envi_map_layers))
#This is the version with rf.modelSel
for(i in 1:model_num){
  pres_subset <- MLnat_with_envi[,colnames(MLnat_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- MLnat_background[,colnames(MLnat_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
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
  
  #Parsimonious random forest model
  #PARSIMONIOUS!!
  rf_regress <- suppressWarnings(rf.modelSel(envi_train[,colnames(envi_train) %in% envi_filename], envi_train$pa, imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  rf_importance <- importance(rf_regress$rf.final) #rf.final- pre-set name!! Only the DOT works!!
  rf_importance <- data.frame(names=row.names(rf_importance),rf_importance)
  rf_importance_total <- rbind(rf_importance_total,rf_importance)
}

#Summary statistics on the frequency and importance of environmental parameters in random forest model.
tmp <- as.data.frame(table(rf_importance_total$names))
colnames(tmp) <- c("Variable","Freq")


#########UNDERSTAND THIS LINE!!!
rf_importance_total_sum <- ddply(rf_importance_total, .(names), plyr::summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
#########UNDERSTAND THE LINE ABOVE!!!



colnames(rf_importance_total_sum) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
rf_importance_total_sum <- left_join(tmp,rf_importance_total_sum)
#To save aggregated data frame.
write.table(rf_importance_total_sum,"MLnatVarImp.txt",quote=FALSE,sep="\t",row.names = FALSE)




#####"Redo" Code RF evaluation#####
# envi_map_layers <- list.files(".", pattern=".tif", full.names = TRUE)
envi_map_layers <- list.files("./EnvironmentalFactors", full.names = TRUE)

redo_MLnat_with_envi <- MLnat_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./EnvironmentalFactors/","",gsub(".tif","",envi_map_layer))
  # MLnat species
  MLnat <- as.data.frame(raster::extract(raster(envi_map_layer),MLnat_location))
  colnames(MLnat) <- envi_filename
  redo_MLnat_with_envi <- cbind(redo_MLnat_with_envi,MLnat)
  # native species
}

redo_MLnat_with_envi$pa <- 1
# 
# write.csv(redo_MLnat_with_envi, file="redo_MLnat_with_envi.csv")
# write.csv(redo_native_with_envi, file="redo_native_with_envi.csv")

#Remove rows with missing data for observations points.
redo_MLnat_with_envi <- na.omit(redo_MLnat_with_envi)


long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or MLnat species observations.
obs_num <- 10*max(nrow(redo_MLnat_with_envi))

#Create empty dataframe for random background points for MLnats.
#Make it much larger than the set of presence points.
redo_background_MLnat_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(redo_background_MLnat_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
redo_background_MLnat_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
redo_background_MLnat_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)










redo_MLnat_background <- redo_background_MLnat_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./EnvironmentalFactors/","",gsub(".tif","",envi_map_layer))
  #Get short names for environmental layers.
  #Extract environmental values at MLnat species locations.
  redo_MLnat_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),redo_background_MLnat_locations))
  colnames(redo_MLnat_background_tmp) <- envi_filename
  redo_MLnat_background <- cbind(redo_MLnat_background,redo_MLnat_background_tmp)
  #Extract environmental values at native species locations.
}

#Designate a presence/absence column for the pseudo-absence points.
redo_MLnat_background$pa <- 0



#Remove rows with missing data for observations points.
redo_MLnat_background <- na.omit(redo_MLnat_background)




MLnat__randomF_rf_importance_total <- data.frame()
MLnat__randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./EnvironmentalFactors/","",gsub(".tif","",envi_map_layers))
for(i in 1:model_num){
  pres_subset <- redo_MLnat_with_envi[,colnames(redo_MLnat_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- redo_MLnat_background[,colnames(redo_MLnat_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
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
  # pres_subset <- MLnat_with_envi[,colnames(MLnat_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- MLnat_background[,colnames(MLnat_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
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
  
  MLnat_rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  MLnat__randomF_rf_importance <- importance(MLnat_rf_regress)
  MLnat__randomF_rf_importance <- data.frame(names=row.names(MLnat__randomF_rf_importance),MLnat__randomF_rf_importance)
  MLnat__randomF_rf_importance_total <- rbind(MLnat__randomF_rf_importance_total,MLnat__randomF_rf_importance)
  
  MLnat__erf <- suppressWarnings(evaluate(testpres,testbackgr,MLnat_rf_regress))
  MLnat__randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(MLnat__randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  MLnat__randomF_rf_evaluation$AUC <- MLnat__erf@auc
  MLnat__randomF_rf_evaluation$cor <- MLnat__erf@cor
  MLnat__randomF_rf_evaluation$kappa <- max(MLnat__erf@kappa)
  # Calculate Yule's Q.
  tmp <- MLnat__erf@OR
  tmp[!is.finite(tmp)] <- NA 
  MLnat__randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  MLnat__randomF_rf_evaluation$TSS <- mean(MLnat__erf@TPR,na.rm=T)+mean(MLnat__erf@TNR,na.rm=T)-1
  MLnat__randomF_evaluation_total <- rbind(MLnat__randomF_evaluation_total,MLnat__randomF_rf_evaluation)
}

# p <- as.matrix(MLnat_rf_regress$importance)   
# ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]]) 
# dotchart(p[ord,1], main="MLnat Variable Importance")  
# 
#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(MLnat__randomF_evaluation_total)
tmpSD <- apply(MLnat__randomF_evaluation_total,2,sd)
MLnat__randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(MLnat__randomF_evaluation_total_summarized,paste("MLnatRFEvaluations.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)





#####Current Predictions#####
# MLnat_with_envi <- read.csv("MLnat_with_envi.csv")
# native_with_envi <- read.csv("native_with_envi.csv")

pres_subset <- MLnat_with_envi[,colnames(MLnat_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
abs_subset <- MLnat_background[,colnames(MLnat_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
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

#Parsimonious random forest model
#PARSIMONIOUS!!
envi_train <- na.omit(envi_train)
for_prediction_rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))

##Attempt to tackle the error: variables in the training data missing in newdata
# save(for_prediction_rf_regress, file= "for_prediction_rf_regress.RData")



# #fix the filenames in predictor
# shortened_envi_map_layers <- gsub("./EnvironmentalFactors/","",gsub(".tif","",envi_map_layers))
# setwd("D:/Science Research/2020-2021 Research Project/Scripts/EnvironmentalFactors")
# 
# shortened_envi_layers_stack <- stack(c(shortened_envi_map_layers))
# shortened_envi_layers_stack <- stack("./EnvironmentalFactors/",c(shortened_envi_map_layers))


test_prediction <- predict(envi_layers_stack,for_prediction_rf_regress)

plot(test_prediction, main='Prediction of Current Observations')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- test_prediction
tr <- threshold(erf,"spec_sens")
plot(pr > tr, main='Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')




#####Partial Dependence#####
RFp1Total <- data.frame()
RFp2Total <- data.frame()
RFp3Total <- data.frame()
RFp4Total <- data.frame()
RFp5Total <- data.frame()
RFp6Total <- data.frame()
RFp7Total <- data.frame()
RFp8Total <- data.frame()
RFp9Total <- data.frame()
RFp10Total <- data.frame()
RFp11Total <- data.frame()
RFp12Total <- data.frame()
RFp13Total <- data.frame()
RFp14Total <- data.frame()
RFp15Total <- data.frame()
RFp16Total <- data.frame()
RFp17Total <- data.frame()
RFp18Total <- data.frame()
RFp19Total <- data.frame()
RFp20Total <- data.frame()
RFp21Total <- data.frame()
RFp22Total <- data.frame()
RFp23Total <- data.frame()
RFp24Total <- data.frame()
RFp25Total <- data.frame()
RFp26Total <- data.frame()
RFp27Total <- data.frame()
RFp28Total <- data.frame()
RFp29Total <- data.frame()


rf_importance_total <- data.frame() # Initialize an empty data frame to collect random forest statistics.
#This is with tuneRF
for(i in 1:model_num){
  pres_subset <- MLnat_with_envi[,colnames(MLnat_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- MLnat_background[,colnames(MLnat_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
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
  
  #Parsimonious random forest model
  #PARSIMONIOUS!!
  rf_regress <- suppressWarnings(tuneRF(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  rf_importance <- importance(rf_regress)
  rf_importance <- data.frame(names=row.names(rf_importance),rf_importance)
  rf_importance_total <- rbind(rf_importance_total,rf_importance)
  
  # #Store partial response curve for random forest model.
  RFp1 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="amt"))
  RFp1Total <- rbind(RFp1Total,RFp1)
  RFp2 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="ap"))
  RFp2Total <- rbind(RFp2Total,RFp2)
  RFp3 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="bldfie"))
  RFp3Total <- rbind(RFp3Total,RFp3)
  RFp4 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="cecsol"))
  RFp4Total <- rbind(RFp4Total,RFp4)
  RFp5 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="clyppt"))
  RFp5Total <- rbind(RFp5Total,RFp5)
  RFp6 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="hfp"))
  RFp6Total <- rbind(RFp6Total,RFp6)
  RFp7 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="imprv"))
  RFp7Total <- rbind(RFp7Total,RFp7)
  RFp8 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="isoth"))
  RFp8Total <- rbind(RFp8Total,RFp8)
  RFp9 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="mdr"))
  RFp9Total <- rbind(RFp9Total,RFp9)
  RFp10 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="mtcm"))
  RFp10Total <- rbind(RFp10Total,RFp10)
  RFp11 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="mtcq"))
  RFp11Total <- rbind(RFp11Total,RFp11)
  RFp12 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="mtdq"))
  RFp12Total <- rbind(RFp12Total,RFp12)
  RFp13 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="mtwarmq"))
  RFp13Total <- rbind(RFp13Total,RFp13)
  RFp14 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="mtwetq"))
  RFp14Total <- rbind(RFp14Total,RFp14)
  RFp15 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="mtwm"))
  RFp15Total <- rbind(RFp15Total,RFp15)
  RFp16 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="ntot"))
  RFp16Total <- rbind(RFp16Total,RFp16)
  RFp17 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="orcdrc"))
  RFp17Total <- rbind(RFp17Total,RFp17)
  RFp18 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="pcq"))
  RFp18Total <- rbind(RFp18Total,RFp18)
  RFp19 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="pdm"))
  RFp19Total <- rbind(RFp19Total,RFp19)
  RFp20 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="pdq"))
  RFp20Total <- rbind(RFp20Total,RFp20)
  RFp21 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="phihox"))
  RFp21Total <- rbind(RFp21Total,RFp21)
  RFp22 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="ps"))
  RFp22Total <- rbind(RFp22Total,RFp22)
  RFp23 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="ptrcv"))
  RFp23Total <- rbind(RFp23Total,RFp23)
  RFp24 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="pwarmq"))
  RFp24Total <- rbind(RFp24Total,RFp24)
  RFp25 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="pwetq"))
  RFp25Total <- rbind(RFp25Total,RFp25)
  RFp26 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="pwm"))
  RFp26Total <- rbind(RFp26Total,RFp26)
  RFp27 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="sndppt"))
  RFp27Total <- rbind(RFp27Total,RFp27)
  RFp28 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="tar"))
  RFp28Total <- rbind(RFp28Total,RFp28)
  RFp29 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="ts"))
  RFp29Total <- rbind(RFp29Total,RFp29)
}

#Rename partial response dataframe variables for plotting.
colnames(RFp1Total) <- c("amt","Detection Probability")
colnames(RFp2Total) <- c("ap","Detection Probability")
colnames(RFp3Total) <- c("bldfie","Detection Probability")
colnames(RFp4Total) <- c("cecsol","Detection Probability")
colnames(RFp5Total) <- c("clyppt","Detection Probability")
colnames(RFp6Total) <- c("hfp","Detection Probability")
colnames(RFp7Total) <- c("imprv","Detection Probability")
colnames(RFp8Total) <- c("isoth","Detection Probability")
colnames(RFp9Total) <- c("mdr","Detection Probability")
colnames(RFp10Total) <- c("mtcm","Detection Probability")
colnames(RFp11Total) <- c("mtcq","Detection Probability")
colnames(RFp12Total) <- c("mtdq","Detection Probability")
colnames(RFp13Total) <- c("mtwarmq","Detection Probability")
colnames(RFp14Total) <- c("mtwetq","Detection Probability")
colnames(RFp15Total) <- c("mtwm","Detection Probability")
colnames(RFp16Total) <- c("ntot","Detection Probability")
colnames(RFp17Total) <- c("orcdrc","Detection Probability")
colnames(RFp18Total) <- c("pcq","Detection Probability")
colnames(RFp19Total) <- c("pdm","Detection Probability")
colnames(RFp20Total) <- c("pdq","Detection Probability")
colnames(RFp21Total) <- c("phihox","Detection Probability")
colnames(RFp22Total) <- c("ps","Detection Probability")
colnames(RFp23Total) <- c("ptrcv","Detection Probability")
colnames(RFp24Total) <- c("pwarmq","Detection Probability")
colnames(RFp25Total) <- c("pwetq","Detection Probability")
colnames(RFp26Total) <- c("pwm","Detection Probability")
colnames(RFp27Total) <- c("sndppt","Detection Probability")
colnames(RFp28Total) <- c("tar","Detection Probability")
colnames(RFp29Total) <- c("ts","Detection Probability")



#Create 2d histograms with best-fit splines for the partial response curves.
#Understand the default inputs (or if they are defaults)
RFp1Plot <- ggplot(RFp1Total, aes(x=amt, y=`Detection Probability`) )+xlab("amt")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp2Plot <- ggplot(RFp2Total, aes(x=ap, y=`Detection Probability`) )+xlab("ap")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp2Total))+theme_bw(base_size=25)
RFp3Plot <- ggplot(RFp3Total, aes(x=bldfie, y=`Detection Probability`) )+xlab("bldfi")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp3Total))+theme_bw(base_size=25)
RFp4Plot <- ggplot(RFp4Total, aes(x=cecsol, y=`Detection Probability`) )+xlab("cecsol")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp4Total))+theme_bw(base_size=25)
RFp5Plot <- ggplot(RFp5Total, aes(x=clyppt, y=`Detection Probability`) )+xlab("clyppt")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp5Total))+theme_bw(base_size=25)
RFp6Plot <- ggplot(RFp6Total, aes(x=hfp, y=`Detection Probability`) )+xlab("hfp")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp6Total))+theme_bw(base_size=25)
RFp7Plot <- ggplot(RFp7Total, aes(x=imprv, y=`Detection Probability`) )+xlab("imprv")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp8Plot <- ggplot(RFp8Total, aes(x=isoth, y=`Detection Probability`) )+xlab("isoth")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp9Plot <- ggplot(RFp9Total, aes(x=mdr, y=`Detection Probability`) )+xlab("mdr")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp10Plot <- ggplot(RFp10Total, aes(x=mtcm, y=`Detection Probability`) )+xlab("mtcm")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp11Plot <- ggplot(RFp11Total, aes(x=mtcq, y=`Detection Probability`) )+xlab("mtcq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp12Plot <- ggplot(RFp12Total, aes(x=mtdq, y=`Detection Probability`) )+xlab("mtdq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp13Plot <- ggplot(RFp13Total, aes(x=mtwarmq, y=`Detection Probability`) )+xlab("mtwarmq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp14Plot <- ggplot(RFp14Total, aes(x=mtwetq, y=`Detection Probability`) )+xlab("mtwetq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp15Plot <- ggplot(RFp15Total, aes(x=mtwm, y=`Detection Probability`) )+xlab("mtwm")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp16Plot <- ggplot(RFp16Total, aes(x=ntot, y=`Detection Probability`) )+xlab("ntot")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp17Plot <- ggplot(RFp17Total, aes(x=orcdrc, y=`Detection Probability`) )+xlab("orcdrc")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp18Plot <- ggplot(RFp18Total, aes(x=pcq, y=`Detection Probability`) )+xlab("pcq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp19Plot <- ggplot(RFp19Total, aes(x=pdm, y=`Detection Probability`) )+xlab("pdm")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp20Plot <- ggplot(RFp20Total, aes(x=pdq, y=`Detection Probability`) )+xlab("pdq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp21Plot <- ggplot(RFp21Total, aes(x=phihox, y=`Detection Probability`) )+xlab("phihox")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp22Plot <- ggplot(RFp22Total, aes(x=ps, y=`Detection Probability`) )+xlab("ps")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp23Plot <- ggplot(RFp23Total, aes(x=ptrcv, y=`Detection Probability`) )+xlab("ptrcv")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp24Plot <- ggplot(RFp24Total, aes(x=pwarmq, y=`Detection Probability`) )+xlab("pwarmq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp25Plot <- ggplot(RFp25Total, aes(x=pwetq, y=`Detection Probability`) )+xlab("pwetq")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp26Plot <- ggplot(RFp26Total, aes(x=pwm, y=`Detection Probability`) )+xlab("pwm")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp27Plot <- ggplot(RFp27Total, aes(x=sndppt, y=`Detection Probability`) )+xlab("sndppt")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp28Plot <- ggplot(RFp28Total, aes(x=tar, y=`Detection Probability`) )+xlab("tar")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp29Plot <- ggplot(RFp29Total, aes(x=ts, y=`Detection Probability`) )+xlab("ts")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)


RFPlots <- plot_grid(RFp1Plot,RFp2Plot,RFp3Plot,RFp4Plot,RFp5Plot,RFp6Plot,
                     RFp7Plot,RFp8Plot,RFp9Plot,RFp10Plot,RFp11Plot,RFp12Plot,
                     RFp13Plot,RFp14Plot,RFp15Plot,RFp16Plot,RFp17Plot,RFp18Plot,
                     RFp19Plot,RFp20Plot,RFp21Plot,RFp22Plot,RFp23Plot,RFp24Plot,
                     RFp25Plot,RFp26Plot,RFp27Plot,RFp28Plot,RFp29Plot,ncol=2,labels="AUTO")
png(paste("MLnatPartialDependence.png",sep=""),width=2*800,height=400*length(envi_filename))

# save(RFp1Plot, file = "RFp1Plot")
# save(RFPlots, file = "RFPlots")


RFPlots
print(RFPlots)
dev.off()