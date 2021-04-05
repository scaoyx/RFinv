

#############Start of the script######
#This file contains all the data processing codes for both invasive and 
#native plants so far (I'm sure there'll be more)
#But the native rf model is not run here (I'll probably put it in a separate file)
#############Start of the script######
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
# install.packages("ggplot2")
library(ggplot2)
# install.packages("cowplot")
library(cowplot)
# install.packages("maptools")
library(maptools)
# install.packages("rgeos")
library(rgeos)



######Original Plant Data######
input <- read.csv("OriginalPlantData.csv")
InvasivePlants <- read.csv("InvasivePlants.csv")
InvasivePlants <- InvasivePlants$﻿ScientificName
#I have no idea why the column name is like that...Weird Chinese characters:/
#and the system is really stubborn XD Even if I changed the language in R and RStudio
#To English,it still returns me Chinese in the console
#Which is totally fine but will cause some translation issues as I'll need to covert the error
#messages back to English without adequate knowledge on computer science jargons XD
InvasivePlants
NativePlants <- read.csv("NativePlants.csv")
NativePlants <- NativePlants$﻿ScientificName
NativePlants


InvasiveObservations <- input[input$verbatimScientificName %in% InvasivePlants, ]
NativeObservations <- input[input$verbatimScientificName %in% NativePlants, ]

print(NativeObservations)

write.csv(InvasiveObservations, file="InvasiveObservations")
write.csv(NativeObservations, file="NativeObservations")




#####Cutting Layers#####
# install.packages("raster")
# install.packages("rgdal")
hfp <- raster("hfp_ca_100m_wgs84.tif")
LA_hfp <- crop(hfp, extent(-119,-117,33,35 ))
writeRaster(LA_hfp, "hfp_LA.tif", overwrite=TRUE)

amt <- raster("bio1_ca_100m_wgs84.tif")
LA_amt <- crop(amt, extent(-119,-117,33,35 ))
writeRaster(LA_amt, "amt_LA.tif", overwrite=TRUE)

ap <- raster("bio12_ca_100m_wgs84.tif")
LA_ap <- crop(ap, extent(-119,-117,33,35 ))
writeRaster(LA_ap, "ap_LA.tif", overwrite=TRUE)

ntot <- raster("ntot_ca_100m_wgs84.tif")
LA_ntot <- crop(ntot, extent(-119,-117,33,35 ))
writeRaster(LA_ntot, "ntot_LA.tif", overwrite=TRUE)

ptrcv <- raster("EE_ptrcv_CA_wgs84_100m.tif")
LA_ptrcv <- crop(ptrcv, extent(-119,-117,33,35 ))
writeRaster(LA_ptrcv, "ptrcv_LA.tif", overwrite=TRUE)

phihox <- raster("phihox_ca_100m_wgs84.tif")
LA_phihox <- crop(phihox, extent(-119,-117,33,35 ))
writeRaster(LA_phihox, "phihox_LA.tif", overwrite=TRUE)


envi_map_layers <- list.files(pattern="_LA", full.names = TRUE)



#Creating a stack of the environmental rasters
#envi_map_layers <- data.frame(envi_map_layers = unlist(envi_map_layers))
envi_layers_stack <- stack(c(envi_map_layers))


######Matching Locations with Plant Data and Preparing Data for Analysis######
#Extracting envi values from rasters at location of native plants
invasive_location <- InvasiveObservations[, c("decimalLongitude", "decimalLatitude")]
native_location <- NativeObservations[, c("decimalLongitude", "decimalLatitude")]


invasive_with_envi <- invasive_location
native_with_envi <- native_location
for(envi_map_layer in envi_map_layers){
  envi_filename <- gsub("./","",gsub("_LA.tif","",envi_map_layer))
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
invasive_background <- background_invasive_locations
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

#Initialize data frames to store partial response curves.
RFp1Total <- data.frame()
RFp2Total <- data.frame()
RFp3Total <- data.frame()
RFp4Total <- data.frame()
RFp5Total <- data.frame()
RFp6Total <- data.frame()

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


#########UNDERSTAND THIS LINE!!!
rf_importance_total_sum <- ddply(rf_importance_total, .(names), plyr::summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
#########UNDERSTAND THE LINE ABOVE!!!



colnames(rf_importance_total_sum) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
rf_importance_total_sum <- left_join(tmp,rf_importance_total_sum)
#To save aggregated data frame.
write.table(rf_importance_total_sum,"InvasiveRFImportance.txt",quote=FALSE,sep="\t",row.names = FALSE)


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
  
  #Store partial response curve for random forest model.
  RFp1 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="amt"))
  RFp1Total <- rbind(RFp1Total,RFp1)
  RFp2 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="ap"))
  RFp2Total <- rbind(RFp2Total,RFp2)
  RFp3 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="hfp"))
  RFp3Total <- rbind(RFp3Total,RFp3)
  RFp4 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="ntot"))
  RFp4Total <- rbind(RFp4Total,RFp4)
  RFp5 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="phihox"))
  RFp5Total <- rbind(RFp5Total,RFp5)
  RFp6 <- as.data.frame(partialPlot(rf_regress,pred.data=envi_train[,colnames(envi_train) %in% envi_filename],x.var="ptrcv"))
  RFp6Total <- rbind(RFp6Total,RFp6)
}

#Rename partial response dataframe variables for plotting.
colnames(RFp1Total) <- c("amt","Detection Probability")
colnames(RFp2Total) <- c("ap","Detection Probability")
colnames(RFp3Total) <- c("hfp","Detection Probability")
colnames(RFp4Total) <- c("ntot","Detection Probability")
colnames(RFp5Total) <- c("phihox","Detection Probability")
colnames(RFp6Total) <- c("ptrcv","Detection Probability")

#Create 2d histograms with best-fit splines for the partial response curves.
#Understand the default inputs (or if they are defaults)
RFp1Plot <- ggplot(RFp1Total, aes(x=amt, y=`Detection Probability`) )+xlab("amt")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+ scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp2Plot <- ggplot(RFp2Total, aes(x=ap, y=`Detection Probability`) )+xlab("ap")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp2Total))+theme_bw(base_size=25)
RFp3Plot <- ggplot(RFp3Total, aes(x=hfp, y=`Detection Probability`) )+xlab("hfp")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp3Total))+theme_bw(base_size=25)
RFp4Plot <- ggplot(RFp4Total, aes(x=ntot, y=`Detection Probability`) )+xlab("ntot")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp4Total))+theme_bw(base_size=25)
RFp5Plot <- ggplot(RFp5Total, aes(x=phihox, y=`Detection Probability`) )+xlab("phihox")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp5Total))+theme_bw(base_size=25)
RFp6Plot <- ggplot(RFp6Total, aes(x=ptrcv, y=`Detection Probability`) )+xlab("ptrcv")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp6Total))+theme_bw(base_size=25)
RFPlots <- plot_grid(RFp1Plot,RFp2Plot,RFp3Plot,RFp4Plot,RFp5Plot,RFp6Plot,ncol=2,labels="AUTO")
png(paste("RFPlotsInvasives.png",sep=""),width=2*800,height=400*length(envi_filename))


RFp1Plot <- ggplot(RFp1Total, aes(x=amt, y=`Detection Probability`) )+xlab("amt")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_color_gradient(low="blue", high="red")


# save(RFp1Plot, file = "RFp1Plot")
# save(RFPlots, file = "RFPlots")


RFPlots
print(RFPlots)
dev.off()

##############################If the graphs don't appear, it means that the laptop is having issues, not the code#####
#################Observation graphs ACTUAL####################
plot(invasive_location, cex=0.5, col='red')
#Had some problem saving the above plot as a png... had to use the RStudio function instead of code to get it down
legend("bottomright", 
       legend = "Invasive Observation", 
       col = "red", 
       pch = 1, 
       bty = "n", 
       pt.cex = 0.8, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F , 
       inset = 0.01)
LA_spdf <- readOGR(".","l.a. county region (v6)")
#Cautious! To read a .shp file you need MUCH MORE than just that .shp file! All the others in the .zip are necessary too!!
plot(LA_spdf)
plot(LA_spdf, add=TRUE)


#Native Actual Observations
plot(native_location, cex=0.5, col='green')
legend("bottomright", 
       legend = "Native Observation", 
       col = "green", 
       pch = 1, 
       bty = "n", 
       pt.cex = 0.8, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F , 
       inset = 0.01)
plot(LA_spdf, add=TRUE)


#Combined Invasive and Native Actual Observations
plot(invasive_location, cex=0.5, col='red')
points(native_location, cex=0.5, col='green')
legend("bottomright", 
       legend = c("Invasive Observation","Native Observation"), 
       col = c("red","green"), 
       pch = 1, 
       bty = "n", 
       pt.cex = 0.8, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F , 
       inset = 0.01)

plot(LA_spdf, add=TRUE)

#Combined Invasive and Native Actual Observations (This time Invasvive on top)
plot(native_location, cex=0.5, col='green')
points(invasive_location, cex=0.5, col='red')
legend("bottomright", 
       legend = c("Invasive Observation","Native Observation"), 
       col = c("red","green"), 
       pch = 1, 
       bty = "n", 
       pt.cex = 0.8, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F , 
       inset = 0.01)


plot(LA_spdf, add=TRUE)

###################EXPERIMENT! Prediction withOUT subsets of Random Forest##############
#Not sure if it will work without subset... I'm now basically dumping everything in
#With randomForest

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
for_prediction_rf_regress <- suppressWarnings(randomForest(envi_train[,colnames(envi_train) %in% envi_filename],envi_train$pa,stepFactor=1,plot=FALSE,doBest=TRUE))

##Attempt to tackle the error: variables in the training data missing in newdata
save(for_prediction_rf_regress, file= "for_prediction_rf_regress.RData")



#fix the filenames in predictor
shortened_envi_map_layers <- gsub('_LA','', envi_map_layers)
shortened_envi_layers_stack <- stack(c(shortened_envi_map_layers))


test_prediction <- predict(shortened_envi_layers_stack,for_prediction_rf_regress)

plot(test_prediction, main='Prediction of Current Observations')
plot(LA_spdf, add=TRUE,border='dark grey')

plot(pr > tr, main='Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')



######Accessories MOSTLY using randomForest()#####
plot(for_prediction_rf_regress, main= "Change of OOB Error over Trees")


p <- as.matrix(rf_regress$importance)   
ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]]) 
dotchart(p[ord,1], main="Invasive Variable Importance")  


#These two are unnecessary but TO BE UNDERSTOOD
plot(rf_importance_total, main="Variable Importance Distribution")
plot(rf_importance_total_sum, main="Variable Importance")


par(mfrow=c(1,1))

##FROM THE WEBSITE WITH "MODEL" FROM MR.LEVI###



# #Resume
# model <- pa ~ amt + ap + hfp + ntot + phihox + ptrcv
# rf1 <- randomForest(model, data=envi_train)
# pr <- predict(shortened_envi_layers_stack, rf1)
# erf <- evaluate(pres_test, backgr_test, rf1)
# erf
# 
# #Plotting
# par(mfrow=c(1,2))
# plot(pr, main='Prediction of Current Observations')
# plot(LA_spdf, add=TRUE,border='dark grey')
# tr <- threshold(erf, 'spec_sens')
# 
# #Understand what this second plot means
# plot(pr > tr, main='presence/absence')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-', cex=0.25)
# 
# 

##########randomForest() to calculate Yule's Q, AUC, Peasron's c and more#####
randomF_rf_importance_total <- data.frame()
randomF_evaluation_total <- data.frame()

for(i in 1:model_num){
  #Subsample presence and pseudo-absence points for training and testing sets for SDMs.
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
  
  testpres <- pres_test
  testbackgr <- backgr_test
  
  rf.regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  randomF_rf_importance <- importance(rf.regress)
  randomF_rf_importance <- data.frame(names=row.names(randomF_rf_importance),randomF_rf_importance)
  randomF_rf_importance_total <- rbind(randomF_rf_importance_total,randomF_rf_importance)
  
  erf <- suppressWarnings(evaluate(testpres,testbackgr,rf.regress))
  randomF_rf_evaluation <-  data.frame(matrix(nrow=1,ncol=5))
  colnames(randomF_rf_evaluation) <- c("AUC","cor","kappa","Q","TSS")
  randomF_rf_evaluation$AUC <- erf@auc
  randomF_rf_evaluation$cor <- erf@cor
  randomF_rf_evaluation$kappa <- max(erf@kappa)
  # Calculate Yule's Q.
  tmp <- erf@OR
  tmp[!is.finite(tmp)] <- NA 
  randomF_rf_evaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
  randomF_rf_evaluation$TSS <- mean(erf@TPR,na.rm=T)+mean(erf@TNR,na.rm=T)-1
  randomF_evaluation_total <- rbind(randomF_evaluation_total,randomF_rf_evaluation)
}

  
#Summarie Evaluations and save as an aggregated table
tmpMean <- colMeans(randomF_evaluation_total)
tmpSD <- apply(randomF_evaluation_total,2,sd)
randomF_evaluation_total_summarized <- as.data.frame(rbind(tmpMean,tmpSD))
#To save aggregated data frame.
write.table(randomF_evaluation_total_summarized,paste("randomFInvasiveParsimonyFinalRFEvaluation.txt",sep=""),quote=FALSE,sep="\t",row.names = TRUE)











#############Below are NATIVE ONES#############

nat_rf_importance_total <- data.frame() # Initialize an empty data frame to collect random forest statistics.
#The native loop
for(i in 1:model_num){
  nat_pres_subset <- native_with_envi[,colnames(native_with_envi) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
  nat_pres_subset <- nat_pres_subset[sample(nrow(nat_pres_subset),sample_num),] #Subsample rows.
  nat_abs_subset <- native_background[,colnames(invasive_background) %in% c(envi_filename,"pa")] #Remove latitude and longitude columns from presence point subset.
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
  envi_train <- rbind(nat_pres_train,nat_backgr_train)
  
  #Parsimonious random forest model
  #PARSIMONIOUS!!
  nat_rf_regress <- suppressWarnings(rf.modelSel(envi_train[,colnames(envi_train) %in% envi_filename], envi_train$pa, imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  nat_rf_importance <- importance(nat_rf_regress$rf.final) #rf.final- pre-set name!! Only the DOT works!!
  nat_rf_importance <- data.frame(names=row.names(nat_rf_importance),nat_rf_importance)
  nat_rf_importance_total <- rbind(nat_rf_importance_total,nat_rf_importance)
}





#Summary statistics on the frequency and importance of environmental parameters in random forest model.
nat_tmp <- as.data.frame(table(nat_rf_importance_total$names))
colnames(nat_tmp) <- c("Variable","Freq")


#########UNDERSTAND THIS LINE!!!
nat_rf_importance_total <- ddply(nat_rf_importance_total, .(names), plyr::summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
#########UNDERSTAND THE LINE ABOVE!!!



colnames(nat_rf_importance_total) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
nat_rf_importance_total <- left_join(nat_tmp,nat_rf_importance_total)
#To save aggregated data frame.
write.table(nat_rf_importance_total,"NativeRFImportance.txt",quote=FALSE,sep="\t",row.names = FALSE)


#Heat maps

############For Navtive heat maps##########################
#Some graphs
Is this applicable??
plot(rf_regress)


















###################Some Attempts about the Model, prediction, and other values###################################
#Some attempts
#Try to run the model testings on Evans's website----make this look well-thoguht 
#-through and DONE TO THE BEST OF MY ABILITY


#Attempt to plot the variable improtances for the invasive plants
nat_p <- as.matrix(nat_rf_regress$importance)   
nat_ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]]) 
dotchart(p[ord,1], main="Native Scaled Variable Importance", pch=20)  

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


###
###
###
###
###

#Plan (as of 12/04/2020): 
#1. Figure out data visualization (don't forget to run rf for native plants)
#2. Recognize quantifying analysis of the importance (already calculated) and relatedness (?)
#   of the environmental factors and the spread
#3. Think about more in-depth use of randomForest model/maplayers/etc.
#   There must be much more to the story
#4. Read on ecological invasive plants topics. Truly understand how the model works
#5. Find ways to extend/improve/consolidate results




















