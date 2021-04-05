rm(list=ls())

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
InvasivePlants
NativePlants <- read.csv("NativePlants.csv")
NativePlants <- NativePlants$ï..ScientificName
NativePlants


InvasiveObservations <- input[input$verbatimScientificName %in% InvasivePlants, ]
NativeObservations <- input[input$verbatimScientificName %in% NativePlants, ]

print(NativeObservations)

write.csv(InvasiveObservations, file="InvasiveObservations")
write.csv(NativeObservations, file="NativeObservations")



# install.packages("raster")
# install.packages("rgdal")
hfp <- raster("hfp_ca_100m_wgs84.tif")
LA_hfp <- crop(hfp, extent(-119,-117,33,35 ))
writeRaster(LA_hfp, "hfp.tif", overwrite=TRUE)

amt <- raster("bio1_ca_100m_wgs84.tif")
LA_amt <- crop(amt, extent(-119,-117,33,35 ))
writeRaster(LA_amt, "amt.tif", overwrite=TRUE)

mdr <- raster("bio2_ca_100m_wgs84.tif")
LA_mdr <- crop(mdr, extent(-119,-117,33,35 ))
writeRaster(LA_mdr, "mdr.tif", overwrite=TRUE)

isoth <- raster("bio3_ca_100m_wgs84.tif")
LA_isoth <- crop(isoth, extent(-119,-117,33,35 ))
writeRaster(LA_isoth, "isoth.tif", overwrite=TRUE)

ts <- raster("bio4_ca_100m_wgs84.tif")
LA_ts <- crop(ts, extent(-119,-117,33,35 ))
writeRaster(LA_ts, "ts.tif", overwrite=TRUE)

mtwm <- raster("bio5_ca_100m_wgs84.tif")
LA_mtwm <- crop(mtwm, extent(-119,-117,33,35 ))
writeRaster(LA_mtwm, "mtwm.tif", overwrite=TRUE)

mtcm <- raster("bio6_ca_100m_wgs84.tif")
LA_mtcm <- crop(mtcm, extent(-119,-117,33,35 ))
writeRaster(LA_mtcm, "mtcm.tif", overwrite=TRUE)

tar <- raster("bio7_ca_100m_wgs84.tif")
LA_tar <- crop(tar, extent(-119,-117,33,35 ))
writeRaster(LA_tar, "tar.tif", overwrite=TRUE)

mtwetq <- raster("bio8_ca_100m_wgs84.tif")
LA_mtwetq <- crop(mtwetq, extent(-119,-117,33,35 ))
writeRaster(LA_mtwetq, "mtwetq.tif", overwrite=TRUE)

mtdq <- raster("bio9_ca_100m_wgs84.tif")
LA_mtdq <- crop(mtdq, extent(-119,-117,33,35 ))
writeRaster(LA_mtdq, "mtdq.tif", overwrite=TRUE)

mtwarmq <- raster("bio10_ca_100m_wgs84.tif")
LA_mtwarmq <- crop(mtwarmq, extent(-119,-117,33,35 ))
writeRaster(LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)

mtcq <- raster("bio11_ca_100m_wgs84.tif")
LA_mtcq <- crop(mtcq, extent(-119,-117,33,35 ))
writeRaster(LA_mtcq, "mtcq.tif", overwrite=TRUE)

ap <- raster("bio12_ca_100m_wgs84.tif")
LA_ap <- crop(ap, extent(-119,-117,33,35 ))
writeRaster(LA_ap, "ap.tif", overwrite=TRUE)

pwm <- raster("bio13_ca_100m_wgs84.tif")
LA_pwm <- crop(pwm, extent(-119,-117,33,35 ))
writeRaster(LA_pwm, "pwm.tif", overwrite=TRUE)

pdm <- raster("bio14_ca_100m_wgs84.tif")
LA_pdm <- crop(pdm, extent(-119,-117,33,35 ))
writeRaster(LA_pdm, "pdm.tif", overwrite=TRUE)

ps <- raster("bio15_ca_100m_wgs84.tif")
LA_ps <- crop(ps, extent(-119,-117,33,35 ))
writeRaster(LA_ps, "ps.tif", overwrite=TRUE)

pwetq <- raster("bio16_ca_100m_wgs84.tif")
LA_pwetq <- crop(pwetq, extent(-119,-117,33,35 ))
writeRaster(LA_pwetq, "pwetq.tif", overwrite=TRUE)

pdq <- raster("bio17_ca_100m_wgs84.tif")
LA_pdq <- crop(pdq, extent(-119,-117,33,35 ))
writeRaster(LA_pdq, "pdq.tif", overwrite=TRUE)

pwarmq <- raster("bio18_ca_100m_wgs84.tif")
LA_pwarmq <- crop(pwarmq, extent(-119,-117,33,35 ))
writeRaster(LA_pwarmq, "pwarmq.tif", overwrite=TRUE)

pcq <- raster("bio19_ca_100m_wgs84.tif")
LA_pcq <- crop(pcq, extent(-119,-117,33,35 ))
writeRaster(LA_pcq, "pcq.tif", overwrite=TRUE)

phihox <- raster("phihox_ca_100m_wgs84.tif")
LA_phihox <- crop(phihox, extent(-119,-117,33,35 ))
writeRaster(LA_phihox, "phihox.tif", overwrite=TRUE)

orcdrc <- raster("orcdrc_ca_100m_wgs84.tif")
LA_orcdrc <- crop(orcdrc, extent(-119,-117,33,35 ))
writeRaster(LA_orcdrc, "orcdrc.tif", overwrite=TRUE)

cecsol <- raster("cecsol_ca_100m_wgs84.tif")
LA_cecsol <- crop(cecsol, extent(-119,-117,33,35 ))
writeRaster(LA_cecsol, "cecsol.tif", overwrite=TRUE)

sndppt <- raster("sndppt_ca_100m_wgs84.tif")
LA_sndppt <- crop(sndppt, extent(-119,-117,33,35 ))
writeRaster(LA_sndppt, "sndppt.tif", overwrite=TRUE)

clyppt <- raster("clyppt_ca_100m_wgs84.tif")
LA_clyppt <- crop(clyppt, extent(-119,-117,33,35 ))
writeRaster(LA_clyppt, "clyppt.tif", overwrite=TRUE)

bldfie <- raster("bldfie_ca_100m_wgs84.tif")
LA_bldfie <- crop(bldfie, extent(-119,-117,33,35 ))
writeRaster(LA_bldfie, "bldfie.tif", overwrite=TRUE)

ntot <- raster("ntot_ca_100m_wgs84.tif")
LA_ntot <- crop(ntot, extent(-119,-117,33,35 ))
writeRaster(LA_ntot, "ntot.tif", overwrite=TRUE)

imprv <- raster("EE_imprv_CA_wgs84_100m.tif")
LA_imprv <- crop(imprv, extent(-119,-117,33,35 ))
writeRaster(LA_imprv, "imprv.tif", overwrite=TRUE)

ptrcv <- raster("EE_ptrcv_CA_wgs84_100m.tif")
LA_ptrcv <- crop(ptrcv, extent(-119,-117,33,35 ))
writeRaster(LA_ptrcv, "ptrcv.tif", overwrite=TRUE)



envi_map_layers <- list.files("./EnvironmentalFactors", full.names = TRUE)



#Creating a stack of the environmental rasters
#envi_map_layers <- data.frame(envi_map_layers = unlist(envi_map_layers))
envi_layers_stack <- stack(c(envi_map_layers))

#Extracting envi values from rasters at location of native plants
invasive_location <- InvasiveObservations[, c("decimalLongitude", "decimalLatitude")]
native_location <- NativeObservations[, c("decimalLongitude", "decimalLatitude")]


invasive_with_envi <- invasive_location
native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./EnvironmentalFactors/","",gsub(".tif","",envi_map_layer))
  # invasive species
  invasive <- as.data.frame(raster::extract(raster(envi_map_layer),invasive_location))
  colnames(invasive) <- envi_filename
  invasive_with_envi <- cbind(invasive_with_envi,invasive)
  # native species
  native <- as.data.frame(raster::extract(raster(envi_map_layer),native_location))
  colnames(native) <- envi_filename
  native_with_envi <- cbind(native_with_envi,native)
}

invasive_with_envi$pa <- 1
native_with_envi$pa <- 1

write.csv(invasive_with_envi, file="invasive_with_envi.csv")
write.csv(native_with_envi, file="native_with_envi.csv")

#Remove rows with missing data for observations points.
invasive_with_envi <- na.omit(invasive_with_envi)
native_with_envi <- na.omit(native_with_envi)

# invasive_with_envi$X <- NULL
# native_with_envi$X <- NULL

#Create a set of pseudo-absence points for extracting environmental values.
#Designate the longitude and latitude bounds of the study area.
#Doesit matter if the extent of maplayers isn't as big?
long_min <- -119
long_max <- -117
lat_min <- 33
lat_max <- 35
#Get the larger of the two numbers: native or invasive species observations.
obs_num <- 10*max(nrow(invasive_with_envi),nrow(native_with_envi))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
background_invasive_locations <- data.frame(matrix(nrow=obs_num,ncol=2))
colnames(background_invasive_locations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
background_invasive_locations$decimalLongitude <- runif(obs_num,min=long_min,max=long_max)
background_invasive_locations$decimalLatitude <- runif(obs_num,min=lat_min,max=lat_max)

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
# mask_invasive <- SpatialPointsDataFrame(invasive_location,data = invasive_lo cation)
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
invasive_background <- background_invasive_locations
native_background <- background_native_locations
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./EnvironmentalFactors/","",gsub(".tif","",envi_map_layer)) #Get short names for environmental layers.
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
invasive_background$pa <- 0
native_background$pa <- 0



#Remove rows with missing data for observations points.
invasive_background <- na.omit(invasive_background)
native_background <- na.omit(native_background)


#Run random forest models on your invasive species data.
rf_importance_total <- data.frame() # Initialize an empty data frame to collect random forest statistics.
model_num <- 100 #Designate how many data subsets to run random forest on.
sample_num <- 100 #Designate how many points to subsample your data on.

#####THIS REALLY REALLY IMPROTANT AND USEFUL GIANT FOR LOOP!!!

# envi_filename <- gsub("./","",gsub("_LA.tif","",envi_map_layers))
#How to replicate processes? For such a great loop like this one, 
#how can I make it "reusable"? 
#Right now, becuase of some kind of dataframe clash, it only allows me to 
#run it after I delete the existing data
#I guess my problem is that
#1. I have problems with dataframe in later steps
#2. I want the loop to overwrtie the existing data, as I'm trying to revert the 
#data back to the "original state" to make sure things are right
#3. but the loop says that the names don't match anymore

#Initialize data frames to store partial response curves.

envi_filename <- gsub("./EnvironmentalFactors/","",gsub(".tif","",envi_map_layers))
#This is the version with rf.modelSel
for(i in 1:model_num){
  pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
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



rf_importance_total_sum <- ddply(rf_importance_total, .(names), plyr::summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))




colnames(rf_importance_total_sum) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
rf_importance_total_sum <- left_join(tmp,rf_importance_total_sum)
#To save aggregated data frame.
write.table(rf_importance_total_sum,"FINALInvVarImp1.txt",quote=FALSE,sep="\t",row.names = FALSE)







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
  pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
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
  RFp6 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames[(envi_train) %in% envi_filename],x.var="hfp"))
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


tmp <- as.data.frame(table(rf_importance_total$names))
colnames(tmp) <- c("Variable","Freq")


#########UNDERSTAND THIS LINE!!!
rf_importance_total_sum <- ddply(rf_importance_total, .(names), plyr::summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
#########UNDERSTAND THE LINE ABOVE!!!



colnames(rf_importance_total_sum) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
rf_importance_total_sum <- left_join(tmp,rf_importance_total_sum)
#To save aggregated data frame.
write.table(rf_importance_total_sum,"InvasiveRFImportanceMoreVariablesTuneRF1.txt",quote=FALSE,sep="\t",row.names = FALSE)

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
png(paste("RFPlotsInvasivesMoreVariables.png",sep=""),width=2*800,height=400*length(envi_filename))

# save(RFp1Plot, file = "RFp1Plot")
# save(RFPlots, file = "RFPlots")


RFPlots
print(RFPlots)
dev.off()



######Predictions using randomForest()######
# invasive_with_envi <- read.csv("invasive_with_envi.csv")
# native_with_envi <- read.csv("native_with_envi.csv")

pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
pres_subset <- pres_subset[sample(nrow(pres_subset),10*sample_num),] #Subsample rows.
abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
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


#Environmental Factors Layers
envi_layers_stack2 <- stack(c(envi_map_layers[17:29]))
plot(envi_layers_stack2)






##########randomForest() to calculate Yule's Q, AUC, Peasron's c and more#####
inv_randomF_rf_importance_total <- data.frame()
inv_randomF_evaluation_total <- data.frame()

for(i in 1:model_num){
  pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
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
  envi_train <- rbind(pres_train,backgr_train)
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  rf.regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  inv_randomF_rf_importance <- importance(rf.regress)
  inv_randomF_rf_importance <- data.frame(names=row.names(inv_randomF_rf_importance),inv_randomF_rf_importance)
  inv_randomF_rf_importance_total <- rbind(inv_randomF_rf_importance_total,inv_randomF_rf_importance)
  
  inv_erf <- suppressWarnings(evaluate(testpres,testbackgr,rf.regress))
  inv_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(inv_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  inv_randomF_rf_evaluation$AUC <- inv_erf@auc
  inv_randomF_rf_evaluation$cor <- inv_erf@cor
  inv_randomF_rf_evaluation$kappa <- max(inv_erf@kappa)
  # Calculate Yule's Q.
  tmp <- inv_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  inv_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  inv_randomF_rf_evaluation$TSS <- mean(inv_erf@TPR,na.rm=T)+mean(inv_erf@TNR,na.rm=T)-1
  inv_randomF_evaluation_total <- rbind(inv_randomF_evaluation_total,inv_randomF_rf_evaluation)
}

# for(i in 1:model_num){
#   #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
#   pres_subset <- invasive_with_envi[,colnames(invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
#   pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
#   abs_subset <- invasive_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
#   abs_subset <- abs_subset[sample(nrow(abs_subset),10*sample_num),] #Subsample rows.
#   
#   #Construct a training and testing set for the presence data.
#   group <- kfold(pres_subset,5)
#   pres_train <- pres_subset[group!=1,]
#   pres_test <- pres_subset[group==1,]
#   
#   #Construct a training and testing set for the pseudo-absence data.
#   group <- kfold(abs_subset,5)
#   backgr_train <- abs_subset[group!=1,]
#   backgr_test <- abs_subset[group==1,]
#   
#   #Construct presence / pseudo-absence training sets.
#   envi_train <- rbind(pres_train,backgr_train)
#   
#   testpres <- pres_test
#   testbackgr <- backgr_test
#   
#   rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
#   randomF_rf_importance <- importance(rf.regress)
#   randomF_rf_importance <- data.frame(names=row.names(randomF_rf_importance),randomF_rf_importance)
#   randomF_rf_importance_total <- rbind(randomF_rf_importance_total,randomF_rf_importance)
#   
#   erf <- suppressWarnings(evaluate(testpres,testbackgr,rf.regress))
#   randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
#   colnames(randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
#   randomF_rf_evaluation$AUC <- erf@auc
#   randomF_rf_evaluation$cor <- erf@cor
#   randomF_rf_evaluation$kappa <- max(erf@kappa)
#   # Calculate Yule's Q.
#   tmp <- erf@OR
#   tmp[!is.finite(tmp)] <- NA 
#   randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
#   randomF_rf_evaluation$TSS <- mean(erf@TPR,na.rm=T)+mean(erf@TNR,na.rm=T)-1
#   randomF_evaluation_total <- rbind(randomF_evaluation_total,randomF_rf_evaluation)
# }


#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(inv_randomF_evaluation_total)
tmpSD <-apply(inv_randomF_evaluation_total,2,sd)
inv_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(inv_randomF_evaluation_total_summarized,paste("randomFInvasiveParsimonyFinalRFEvaluationTEST.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)


##########AUC REDO with code from FutureEvaluation######
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




invredo__randomF_rf_importance_total <- data.frame()
invredo__randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- redo_invasive_with_envi[,colnames(redo_invasive_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- redo_invasive_background[,colnames(redo_invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
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
  
  invredo_rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  invredo__randomF_rf_importance <- importance(invredo_rf_regress)
  invredo__randomF_rf_importance <- data.frame(names=row.names(invredo__randomF_rf_importance),invredo__randomF_rf_importance)
  invredo__randomF_rf_importance_total <- rbind(invredo__randomF_rf_importance_total,invredo__randomF_rf_importance)
  
  invredo__erf <- suppressWarnings(evaluate(testpres,testbackgr,invredo_rf_regress))
  invredo__randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(invredo__randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  invredo__randomF_rf_evaluation$AUC <- invredo__erf@auc
  invredo__randomF_rf_evaluation$cor <- invredo__erf@cor
  invredo__randomF_rf_evaluation$kappa <- max(invredo__erf@kappa)
  # Calculate Yule's Q.
  tmp <- invredo__erf@OR
  tmp[!is.finite(tmp)] <- NA 
  invredo__randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  invredo__randomF_rf_evaluation$TSS <- mean(invredo__erf@TPR,na.rm=T)+mean(invredo__erf@TNR,na.rm=T)-1
  invredo__randomF_evaluation_total <- rbind(invredo__randomF_evaluation_total,invredo__randomF_rf_evaluation)
}

p <- as.matrix(invredo_rf_regress$importance)   
ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]]) 
dotchart(p[ord,1], main="Invasive Variable Importance")  

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(invredo__randomF_evaluation_total)
tmpSD <- apply(invredo__randomF_evaluation_total,2,sd)
invredo__randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(invredo__randomF_evaluation_total_summarized,paste("Invasiveredo_randomFParsimonyFinalRFEvaluationsTEST.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)





natredo__randomF_rf_importance_total <- data.frame()
natredo__randomF_evaluation_total <- data.frame()

envi_filename <- gsub("./","",gsub(".tif","",envi_map_layers))

for(i in 1:model_num){
  pres_subset <- redo_native_with_envi[,colnames(redo_native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  abs_subset <- redo_native_background[,colnames(redo_native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
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
  
  natredo_rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  natredo__randomF_rf_importance <- importance(natredo_rf_regress)
  natredo__randomF_rf_importance <- data.frame(names=row.names(natredo__randomF_rf_importance),natredo__randomF_rf_importance)
  natredo__randomF_rf_importance_total <- rbind(natredo__randomF_rf_importance_total,natredo__randomF_rf_importance)
  
  natredo__erf <- suppressWarnings(evaluate(testpres,testbackgr,natredo_rf_regress))
  natredo__randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(natredo__randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  natredo__randomF_rf_evaluation$AUC <- natredo__erf@auc
  natredo__randomF_rf_evaluation$cor <- natredo__erf@cor
  natredo__randomF_rf_evaluation$kappa <- max(natredo__erf@kappa)
  # Calculate Yule's Q.
  tmp <- natredo__erf@OR
  tmp[!is.finite(tmp)] <- NA 
  natredo__randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  natredo__randomF_rf_evaluation$TSS <- mean(natredo__erf@TPR,na.rm=T)+mean(natredo__erf@TNR,na.rm=T)-1
  natredo__randomF_evaluation_total <- rbind(natredo__randomF_evaluation_total,natredo__randomF_rf_evaluation)
  
  
  p <- as.matrix(natredo_rf_regress$importance)   
  ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]]) 
  dotchart(p[ord,1], main="Native Variable Importance")  
  
}

#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(natredo__randomF_evaluation_total)
tmpSD <- apply(natredo__randomF_evaluation_total,2,sd)
natredo__randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(natredo__randomF_evaluation_total_summarized,paste("nativeredo_randomFParsimonyFinalRFEvaluationsTEST.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)




######Accessories using randomForest()#####
plot(for_prediction_rf_regress, main= "Invasive Change of OOB Error over Trees")
plot(nat_for_prediction_rf_regress, main= "Native Change of OOB Error over Trees")

varImpPlot(for_prediction_rf_regress)
varImpPlot(for_prediction_rf_regress)

#Plot actual observations
#Invasive
plot(invasive_location, main='Invasive Current Observations', xlab="Longitude",ylab="Latitude", col="#006600", pch=20)
plot(LA_spdf, add=TRUE,border='dark grey')

plot(native_location, main='Native Current Observations', xlab="Longitude",ylab="Latitude", col="#006600", pch=20)
plot(LA_spdf, add=TRUE,border='dark grey')

#This is rf regress
p <- as.matrix(rf_regress$importance)   
ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]]) 
dotchart(p[ord,1], main="Invasive Variable Importance")  

p <- as.matrix(invredo_rf_regress$importance)   
ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]]) 
dotchart(p[ord,1], main="Invasive Variable Importance")  


p <- as.matrix(natredo_rf_regress$importance)   
ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]]) 
dotchart(p[ord,1], main="Native Variable Importance")  


par(mfrow=c(1,1))




######Native part######
nat_rf_importance_total <- data.frame() # Initialize an empty data frame to collect random forest statistics.
#The native loop
for(i in 1:model_num){
  nat_pres_subset <- native_with_envi[,colnames(native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  nat_pres_subset <- nat_pres_subset[sample(nrow(nat_pres_subset),sample_num),] #Subsample rows.
  nat_abs_subset <- native_background[,colnames(native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  nat_abs_subset <- nat_abs_subset[sample(nrow(nat_abs_subset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  nat_group <- kfold(nat_pres_subset,5)
  nat_pres_train <- nat_pres_subset[nat_group!=1,]
  nat_pres_test <- nat_pres_subset[nat_group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  nat_group <- kfold(nat_abs_subset,5)
  nat_backgr_train <- nat_abs_subset[nat_group!=1,]
  nat_backgr_test <- nat_abs_subset[nat_group==1,]
  
  #Construct presence / pseudo-absence training sets.
  nat_envi_train <- rbind(nat_pres_train,nat_backgr_train)
  
  #Parsimonious random forest model
  #PARSIMONIOUS!!
  nat_rf_regress <- suppressWarnings(rf.modelSel(nat_envi_train[,colnames(nat_envi_train) %in% envi_filename], nat_envi_train$pa, imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  nat_rf_importance <- importance(nat_rf_regress$rf.final) #rf.final- pre-set name!! Only the DOT works!!
  nat_rf_importance <- data.frame(names=row.names(nat_rf_importance),nat_rf_importance)
  nat_rf_importance_total <- rbind(nat_rf_importance_total,nat_rf_importance)
}

p <- as.matrix(nat_rf_regress$importance)   
ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]]) 
dotchart(p[ord,1], main="Native Variable Importance")  




#Summary statistics on the frequency and importance of environmental parameters in random forest model.
nat_tmp <- as.data.frame(table(nat_rf_importance_total$names))
colnames(nat_tmp) <- c("Variable","Freq")


#########UNDERSTAND THIS LINE!!!
nat_rf_importance_total_sum <- ddply(nat_rf_importance_total, .(names), plyr::summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
#########UNDERSTAND THE LINE ABOVE!!!



colnames(nat_rf_importance_total_sum) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
nat_rf_importance_total_sum <- left_join(nat_tmp,nat_rf_importance_total_sum)
#To save aggregated data frame.
write.table(nat_rf_importance_total_sum,"FINALNatVarImp2.txt",quote=FALSE,sep="\t",row.names = FALSE)


#####Native AuC, Yule's Q,and more#####
nat_randomF_rf_importance_total <- data.frame()
nat_randomF_evaluation_total <- data.frame()

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
  # #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
  # pres_subset <- native_with_envi[,colnames(native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  # pres_subset <- pres_subset[sample(nrow(pres_subset),sample_num),] #Subsample rows.
  # abs_subset <- native_background[,colnames(native_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
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
  envi_train <- rbind(pres_train,backgr_train)
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  rf.regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  # rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  nat_randomF_rf_importance <- importance(rf.regress)
  nat_randomF_rf_importance <- data.frame(names=row.names(nat_randomF_rf_importance),nat_randomF_rf_importance)
  nat_randomF_rf_importance_total <- rbind(nat_randomF_rf_importance_total,nat_randomF_rf_importance)
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  nat_erf <- suppressWarnings(evaluate(testpres,testbackgr,rf.regress))
  nat_randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(nat_randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  nat_randomF_rf_evaluation$AUC <- nat_erf@auc
  nat_randomF_rf_evaluation$cor <- nat_erf@cor
  nat_randomF_rf_evaluation$kappa <- max(nat_erf@kappa)
  # Calculate Yule's Q.
  tmp <- nat_erf@OR
  tmp[!is.finite(tmp)] <- NA 
  nat_randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  nat_randomF_rf_evaluation$TSS <- mean(nat_erf@TPR,na.rm=T)+mean(nat_erf@TNR,na.rm=T)-1
  nat_randomF_evaluation_total <- rbind(nat_randomF_evaluation_total,nat_randomF_rf_evaluation)
}


#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(nat_randomF_evaluation_total)
tmpSD <- apply(nat_randomF_evaluation_total,2,sd)
nat_randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(nat_randomF_evaluation_total_summarized,paste("randomFnativeParsimonyFinalRFEvaluationzz.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)

#####Native Variable Dependence Plots#####
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


nat_rf_importance_total <- data.frame() # Initialize an empty data frame to collect random forest statistics.
#This is with tuneRF
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


tmp <- as.data.frame(table(rf_importance_total$names))
colnames(tmp) <- c("Variable","Freq")


#########UNDERSTAND THIS LINE!!!
rf_importance_total_sum <- ddply(rf_importance_total, .(names), plyr::summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
#########UNDERSTAND THE LINE ABOVE!!!



colnames(rf_importance_total_sum) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
rf_importance_total_sum <- left_join(tmp,rf_importance_total_sum)
#To save aggregated data frame.
write.table(rf_importance_total_sum,"nativeRFImportanceMoreVariablesTuneRF1.txt",quote=FALSE,sep="\t",row.names = FALSE)

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
png(paste("RFPlotsnativesMoreVariables.png",sep=""),width=2*800,height=400*length(envi_filename))




# save(RFp1Plot, file = "RFp1Plot")
# save(RFPlots, file = "RFPlots")


RFPlots
print(RFPlots)
dev.off()

######Native Predictions######
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
nat_for_prediction_rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))

##Attempt to tackle the error: variables in the training data missing in newdata
# save(for_prediction_rf_regress, file= "for_prediction_rf_regress.RData")



# #fix the filenames in predictor
# shortened_envi_map_layers <- gsub("./EnvironmentalFactors/","",gsub(".tif","",envi_map_layers))
# setwd("D:/Science Research/2020-2021 Research Project/Scripts/EnvironmentalFactors")
# 
# shortened_envi_layers_stack <- stack(c(shortened_envi_map_layers))
# shortened_envi_layers_stack <- stack("./EnvironmentalFactors/",c(shortened_envi_map_layers))
testpres <- pres_test
testbackgr <- backgr_test

current_envi_map_layers <- list.files("./EnvironmentalFactors", pattern='.tif',full.names = TRUE)
current_envi_filename <- gsub("./EnvironmentalFactors/","",gsub(".tif","",current_envi_map_layers)) #Get short names for environmental layers.
envi_layers_stack <- stack(c(current_envi_map_layers))

nat_erf <- suppressWarnings(evaluate(testpres,testbackgr,nat_for_prediction_rf_regress))
test_prediction <- predict(envi_layers_stack,nat_for_prediction_rf_regress)

plot(test_prediction, main='Prediction of Current Observations')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- test_prediction
tr <- threshold(nat_erf,"spec_sens")
plot(pr > tr, main='Current Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')


#Environmental Factors Layers
envi_layers_stack2 <- stack(c(envi_map_layers[17:29]))
plot(envi_layers_stack2)



######Future Predictions---- I CAN DO THIS!!!######
#Must enter all variables. They must correspond to every data point enetered.
amt <- raster("./bioclim/bioclim1.tif")
test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)

mdr <- raster("./bioclim/bioclim2.tif")
test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)

isoth <- raster("./bioclim/bioclim3.tif")
test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)

ts <- raster("./bioclim/bioclim4.tif")
test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)

mtwm <- raster("./bioclim/bioclim5.tif")
test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)

mtcm <- raster("./bioclim/bioclim6.tif")
test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)

tar <- raster("./bioclim/bioclim7.tif")
test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)

mtwetq <- raster("./bioclim/bioclim8.tif")
test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)

mtdq <- raster("./bioclim/bioclim9.tif")
test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)

mtwarmq <- raster("./bioclim/bioclim10.tif")
test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)

mtcq <- raster("./bioclim/bioclim11.tif")
test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)

ap <- raster("./bioclim/bioclim12.tif")
test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)

pwm <- raster("./bioclim/bioclim13.tif")
test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)

pdm <- raster("./bioclim/bioclim14.tif")
test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)

ps <- raster("./bioclim/bioclim15.tif")
test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)

pwetq <- raster("./FutureBCData/FutureTestRun/pwetq.tif")
test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)

pdq <- raster("./bioclim/bioclim17.tif")
test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)

pwarmq <- raster("./bioclim/bioclim18.tif")
test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)

pcq <- raster("./bioclim/bioclim19.tif")
test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)




# #Fitting raster extents (for stacking)
# #New Future Data 
# r1 <- raster("./FutureBCData/FutureTestRun/amt1.tif")
# plot(r1) #success
# #HumanFootPrint Data used as sample extent
# r2 <- raster("./FutureBCData/FutureTestRun/hfp1.tif")
# plot(r2)#success
# 
# #Cropping r1 so that its extent fits that of r2
# r1_cropped <- crop(r1,extent(r2))
# plot(r1_cropped) #success
# 
# #Stacking layers together
# test_stack <- stack(r1_cropped,r2)
# #Error in compareRaster(x) : different extent
# 
# 
# 
# #Let's try recropping using global file; use LA_hfp as extent sample
# amt <- raster("./FutureBCData/FutureTestRun/amt1.tif")
# amt_cropped <- crop(amt,extent(LA_hfp))
# hfp_cropped <- crop(hfp,extent(LA_hfp))





#Reprojecting with set extents
library(raster)
amt_repro <- projectRaster(amt, LA_hfp,method = 'bilinear')
test_stack <- stack(c(amt_repro,LA_hfp))
plot(test_stack)
#YEEEEEEEEEEEEEEEEEEEEEEEEEESSSS! Reprojection worked!! It just takes some time...
#Now I don't even need to convert using gdal
#Just reproject everything would be fine!



#Creating a stack of the environmental rasters
#envi_map_layers <- data.frame(envi_map_layers = unlist(envi_map_layers))
envi_map_layers <- list.files("./FutureBCData/SSP1262140", pattern='.tif',full.names = TRUE)
envi_filename <- gsub("./FutureBCData/SSP1262140/","",gsub(".tif","",envi_map_layers)) #Get short names for environmental layers.

envi_layers_stack <- stack(c(envi_map_layers))


#Run Predictions for Invasive and Native
# for_prediction_rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
invasive_future_prediction <- predict(envi_layers_stack,for_prediction_rf_regress)
#Run the for_prediction_rf_regress to sample the right data first
testpres <- pres_test
testbackgr <- backgr_test
inv_predict_erf <- suppressWarnings(evaluate(testpres,testbackgr,for_prediction_rf_regress))

par(mfrow=c(1,1))

plot(invasive_future_prediction, main='Invasive SSP 126 2021-2040 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- invasive_future_prediction
tr <- threshold(inv_predict_erf,"spec_sens")

plot(pr > tr, main='Invasive SSP 126 2021-2040 P/A')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')


#Native
# nat_for_prediction_rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
native_future_prediction <- predict(envi_layers_stack,nat_for_prediction_rf_regress)

plot(native_future_prediction, main='Native SSP 126 2021-2040 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 126 2021-2040 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')






gdal_translate -b 1 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif amt.tif
gdal_translate -b 2 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif mdr.tif
gdal_translate -b 3 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif isoth.tif
gdal_translate -b 4 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif ts.tif
gdal_translate -b 5 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif mtwm.tif
gdal_translate -b 6 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif mtcm.tif
gdal_translate -b 7 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif tar.tif
gdal_translate -b 8 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif mtwetq.tif
gdal_translate -b 9 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif mtdq.tif
gdal_translate -b 10 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif mtwarmq.tif
gdal_translate -b 11 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif mtcq.tif
gdal_translate -b 12 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif ap.tif
gdal_translate -b 13 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif pwm.tif
gdal_translate -b 14 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif pdm.tif
gdal_translate -b 15 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif ps.tif
gdal_translate -b 16 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif pwetq.tif
gdal_translate -b 17 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif pdq.tif
gdal_translate -b 18 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif pwarmq.tif
gdal_translate -b 19 wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.tif pcq.tif

######Distribution for Current AUCs######
#invasive current
invAUC <- data.frame(invredo__randomF_evaluation_total$AUC)
invAUCPlot <- ggplot(invAUC, aes(x=AUC)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + xlim(0,1)
print(invAUCPlot + ggtitle("Invasive AUC Distrubution Current"))

#native current
natAUC <- data.frame(natredo__randomF_evaluation_total$AUC)
natAUCPlot <- ggplot(natAUC, aes(x=AUC)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + xlim(0,1)
print(natAUCPlot + ggtitle("Native AUC Distrubution Current"))




#####A Decision Tree#####
reprtree:::plot.getTree(invredo_rf_regress)

