rm(list=ls())

setwd("D:/Science Research/2020-2021 Research Project/Scripts")
require(raster)
require(sp)
require(rgdal)
require(rfUtilities)
require(randomForest)
require(dismo)
library(plyr)
library(dplyr)

# input <- read.csv("OriginalPlantData.csv")
# InvasivePlants <- read.csv("InvasivePlants.csv")
# InvasivePlants <- InvasivePlants$﻿ScientificName
# #I have no idea why the column name is like that...Weird Chinese characters:/
# #and the system is really stubborn XD Even if I changed the language in R and RStudio
# #To English,it still returns me Chinese in the console
# #Which is totally fine but will cause some translation issues as I'll need to covert the error
# #messages back to English without adequate knowledge on computer science jargons XD
# InvasivePlants
# NativePlants <- read.csv("NativePlants.csv")
# NativePlants <- NativePlants$﻿ScientificName
# NativePlants
# 
# 
# InvasiveObservations <- input[input$verbatimScientificName %in% InvasivePlants, ]
# NativeObservations <- input[input$verbatimScientificName %in% NativePlants, ]
# 
# print(NativeObservations)
# 
# write.csv(InvasiveObservations, file="InvasiveObservations")
# write.csv(NativeObservations, file="NativeObservations")
# 
# 
# 
# # install.packages("raster")
# # install.packages("rgdal")
# hfp <- raster("hfp_ca_100m_wgs84.tif")
# LA_hfp <- crop(hfp, extent(-119,-117,33,35 ))
# writeRaster(LA_hfp, "hfp_LA.tif", overwrite=TRUE)
# 
# amt <- raster("bio1_ca_100m_wgs84.tif")
# LA_amt <- crop(amt, extent(-119,-117,33,35 ))
# writeRaster(LA_amt, "amt_LA.tif", overwrite=TRUE)
# 
# ap <- raster("bio12_ca_100m_wgs84.tif")
# LA_ap <- crop(ap, extent(-119,-117,33,35 ))
# writeRaster(LA_ap, "ap_LA.tif", overwrite=TRUE)
# 
# ntot <- raster("ntot_ca_100m_wgs84.tif")
# LA_ntot <- crop(ntot, extent(-119,-117,33,35 ))
# writeRaster(LA_ntot, "ntot_LA.tif", overwrite=TRUE)
# 
# ptrcv <- raster("EE_ptrcv_CA_wgs84_100m.tif")
# LA_ptrcv <- crop(ptrcv, extent(-119,-117,33,35 ))
# writeRaster(LA_ptrcv, "ptrcv_LA.tif", overwrite=TRUE)
# 
# phihox <- raster("phihox_ca_100m_wgs84.tif")
# LA_phihox <- crop(phihox, extent(-119,-117,33,35 ))
# writeRaster(LA_phihox, "phihox_LA.tif", overwrite=TRUE)
# 
# 
# envi_map_layers <- list.files(pattern="_LA", full.names = TRUE)
# 
# 
# 
# 
# #Creating a stack of the environmental rasters
# #envi_map_layers <- data.frame(envi_map_layers = unlist(envi_map_layers))
# envi_layers_stack <- stack(c(envi_map_layers))
# 
# #Extracting envi values from rasters at location of native plants
# invasive_location <- InvasiveObservations[, c("decimalLongitude", "decimalLatitude")]
# native_location <- NativeObservations[, c("decimalLongitude", "decimalLatitude")]
# 
# 
# invasive_with_envi <- invasive_location
# native_with_envi <- native_location
# for(envi_map_layer in envi_map_layers){
#   envi_filename <- gsub("./","",gsub("_LA.tif","",envi_map_layer))
#   # invasive species
#   invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
#   colnames(invasive) <- envi_filename
#   invasive_with_envi <- cbind(invasive_with_envi,invasive)
#   # native species
#   native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
#   colnames(native) <- envi_filename
#   native_with_envi <- cbind(native_with_envi,native)
# }
# 
# invasive_with_envi$pa <- 1
# native_with_envi$pa <- 1
# 
# write.csv(invasive_with_envi, file="invasive_with_envi.csv")
# write.csv(native_with_envi, file="native_with_envi.csv")
# 
# #Remove rows with missing data for observations points.
# invasive_with_envi <- na.omit(invasive_with_envi)
# native_with_envi <- na.omit(native_with_envi)
# 
# 
# 
# #Create a set of pseudo-absence points for extracting environmental values.
# #Designate the longitude and latitude bounds of the study area.
# #Doesit matter if the extent of maplayers isn't as big?
# long_min <- -119
# long_max <- -117
# lat_min <- 33
# lat_max <- 35
# #Get the larger of the two numbers: native or invasive species observations.
# obs_num <- 10*max(nrow(invasive_with_envi),nrow(native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
background_native_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(background_native_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
background_native_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
background_native_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

#Question: since the randomPoints function in dismo is replaced by runif, will that
#affect the results? (now there're no longer masks either)
#After all the runif is not dependent on the presense data, while randomPoints is


# install.packages("dismo")
# get the file names
# files <- list.files(path=paste(system.file(package="dismo"), '/ex',
#                                sep=''),  pattern='grd',  full.names=TRUE )
# we use the first file to create a RasterLayer
# mask_invasive <- SpatialPointsDataFrame(invasive_location,data = invasive_location)
# mask_invasive <- raster(extent(mask_invasive))
# set.seed(2020)
# invasive_absence <- randomPoints(mask_invasive, 100)
# # Warning message:
# #   In .couldBeLonLat(x, warnings = warnings) :
# #   CRS is NA. Assuming it is longitude/latitude
# 
# # install.packages("tidyverse")
# library(tidyverse)
# #in the native platns data, there are some points without location info.
# native_location_without_na <- drop_na(native_location)
# mask_native <- SpatialPointsDataFrame(native_location_without_na, data = native_location_without_na)
# set.seed(2020)
# native_absence <- randomPoints(mask_native, 100)
# 
# 
# invasive_absence <- as.data.frame(invasive_absence)
# native_absence <- as.data.frame(native_absence)
# invasive_abs_with_envi <- invasive_absence
# native_abs_with_envi <- native_absence
native_background <- background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub("_LA.tif","",envi_map_layer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  invasive_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),background_invasive_locations))
  colnames(invasive_background_tmp) <- envi_filename
  invasive_background <- cbind(invasive_background,invasive_background_tmp)
  #Extract environmental values at native species locations.
  native_background_tmp <- as.data.frame(raster::extract(raster(envi_map_layer),background_native_locations))
  colnames(native_background_tmp) <- envi_filename
  native_background <- cbind(native_background,native_background_tmp)
}

# 
# for(envi_map_layer in envi_map_layers){
#   envi_filename <- gsub("./","",gsub("_LA.tif","",envi_map_layer))
#   # invasive species
#   invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_absence))
#   colnames(invasive) <- envi_filename
#   invasive_abs_with_envi <- cbind(invasive_abs_with_envi,invasive)
#   # native species
#   native <- as.data.frame(raster::extract(raster(envi_map_layer), native_absence))
#   colnames(native) <- envi_filename
#   native_abs_with_envi <- cbind(native_abs_with_envi,native)
# }
# 
# print(invasive_abs_with_envi)
# write.csv(invasive_abs_with_envi, file="invasive_abs_with_envi.csv")
# write.csv(native_abs_with_envi, file="native_abs_with_envi.csv")
# 


#Designate a presence/absence column for the pseudo-absence points.
native_background$pa <- 0



#Remove rows with missing data for observations points.
native_background <- na.omit(native_background)


#Run random forest models on your invasive species data.
rf_importance_total <- data.frame() # Initialize an empty data frame to collect random forest statistics.
model_num <- 100 #Designate how many data subsets to run random forest on.
sample_num <- 100 #Designate how many points to subsample your data on.

#####THIS REALLY REALLY IMPROTANT AND USEFUL GIANT FOR LOOP!!!

envi_filename <- gsub("./","",gsub("_LA.tif","",envi_map_layers))
#How to replicate processes? For such a great loop like this one, 
#how can I make it "reusable"? 
#Right now, becuase of some kind of dataframe clash, it only allows me to 
#run it after I delete the existing data
#I guess my problem is that
#1. I have problems with dataframe in later steps
#2. I want the loop to overwrtie the existing data, as I'm trying to revert the 
#data back to the "original state" to make sure things are right
#3. but the loop says that the names don't match anymore
for(i in 1:model_num){
  pres_subset <- native_with_envi[,colnames(native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- native_background[,colnames(native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
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
rf_importance_total <- ddply(rf_importance_total, .(names), plyr::summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
#########UNDERSTAND THE LINE ABOVE!!!



colnames(rf_importance_total) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
rf_importance_total <- left_join(tmp,rf_importance_total)
#To save aggregated data frame.
write.table(rf_importance_total,"NativeRFImportance.txt",quote=FALSE,sep="\t",row.names = FALSE)

#Try to run the model testings on Evans's website----make this look well-thoguht 
#-through and DONE TO THE BEST OF MY ABILITY


#Attempt to plot the variable improtances for the invasive plants
p <- as.matrix(rf_regress$importance)   
ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]]) 
dotchart(p[ord,1], main="Scaled Variable Importance", pch=20)  

#Plot the raster? Predict?
#(Below) direct predict function usage from https://rspatial.org/raster/sdm/6_sdm_methods.html
#Not sure what would be the proper predictor here
pr <- predict(pres_test, rf_regress)

#(Below) an alternative predict(ion) from https://evansmurphy.wixsite.com/evansspatial/random-forest-sdm
#somehow the stack() and paste() has been telling me that rf_regress$importance is null/empty...
#Even after I saved rf_regress as a file in the workspace, it is still returning the same error.Hmmm..
xvars <- stack(paste(getwd(), paste(rownames(rf_regress$importance), "img", sep="."), sep="/"))
predict(xvars, rf_regress, "model.probs.img", type="prob", index=2, na.rm=TRUE, overwrite=TRUE, progress="window")

#Have not modified yet. Since "model.probs.img" isn't generated yet, these lines won't work anyways
r <- raster("model.probs.img")
plot(r)
plot( sdata[sdata$Present == "1" ,], add = TRUE, col="red", pch=20, cex=0.75)
plot( sdata[sdata$Present == "0" ,], add = TRUE, col="blue", pch=20, cex=0.75)


#################################################
#################################################
#################################################
#################################################
#################################################

#Plan (as of 12/04/2020): 
#1. Figure out data visualization (don't forget to run rf for native plants)
#2. Recognize quantifying analysis of the importance (already calculated) and relatedness (?)
#   of the environmental factors and the spread
#3. Think about more in-depth use of randomForest model/maplayers/etc.
#   There must be much more to the story
#4. Read on ecological invasive plants topics. Truly understand how the model works
#5. Find ways to extend/improve/consolidate results


