rm(list=ls())
require(raster)
require(sp)
require(rgdal)
require(dismo)
require(rfUtilities)
require(randomForest)

setwd("D:/Science Research/2020-2021 Research Project/Scripts")

#Read in plant observation data.
input <- read.table("OriginalPlantData.csv", header=T, sep=",",as.is=T,skip=0,fill=T,quote="\"",check.names=F,encoding = "UTF-8")

#Read in invasive plant species list.
InvasivePlants <- read.table("InvasivePlants.csv", header=T, sep=",",as.is=T,skip=0,fill=T,quote="\"",check.names=F,encoding = "UTF-8")
InvasivePlants <- InvasivePlants$ <U+FEFF>ScientificName

#Read in native plant species list.
NativePlants <- read.table("NativePlants.csv", header=T, sep=",",as.is=T,skip=0,fill=T,quote="\"",check.names=F,encoding = "UTF-8")
NativePlants <- NativePlants$ <U+FEFF>ScientificName

#Filter plant observations to only contain invasives.
InvasiveObservations <- input[input$verbatimScientificName %in% InvasivePlants,]

#Filter plant observations to only contain natives.
NativeObservations <- input[input$verbatimScientificName %in% NativePlants,]

#Read in cropped map layers.
EnvLayers <- list.files(pattern="_LA", full.names = TRUE)

#Create a stack of map layers.
EnvStack <- stack(c(EnvLayers))

#Designate the locations of invasive and native plants.
InvasiveLocations <- InvasiveObservations[, c("decimalLongitude", "decimalLatitude")]
NativeLocations <- NativeObservations[, c("decimalLongitude", "decimalLatitude")]

#Extract the environmental map layer values at the native and invasive
#plant locations.
EnvInvasive <- InvasiveLocations
EnvNative <- NativeLocations
EnvFileNames <- gsub("./","",gsub("_LA.tif","",EnvLayers)) #Get short names for all environmental layers.
for(EnvLayer in EnvLayers){
  EnvFileName <- gsub("./","",gsub("_LA.tif","",EnvLayer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  EnvInvasiveTmp <- as.data.frame(raster::extract(raster(EnvLayer),InvasiveLocations))
  colnames(EnvInvasiveTmp) <- EnvFileName
  EnvInvasive <- cbind(EnvInvasive,EnvInvasiveTmp)
  #Extract environmental values at native species locations.
  EnvNativeTmp <- as.data.frame(raster::extract(raster(EnvLayer),NativeLocations))
  colnames(EnvNativeTmp) <- EnvFileName
  EnvNative <- cbind(EnvNative,EnvNativeTmp)
}

#Designate a presence/absence column for the observation points.
EnvInvasive$pa <- 1
EnvNative$pa <- 1

#Remove rows with missing data for observations points.
EnvInvasive <- na.omit(EnvInvasive)
EnvNative <- na.omit(EnvNative)

#Create a set of pseudo-absence points for extracting environmental values.
#Designate the longitude and latitude bounds of the study area.
LongMin <- -119
LongMax <- -117
LatMin <- 33
LatMax <- 35
#Get the larger of the two numbers: native or invasive species observations.
ObsNum <- 10*max(nrow(EnvInvasive),nrow(EnvNative))

#Create empty dataframe for random background points for invasives.
#Make it much larger than the set of presence points.
EnvInvasiveBGLocations <- data.frame(matrix(nrow=ObsNum,ncol=2))
colnames(EnvInvasiveBGLocations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
EnvInvasiveBGLocations$decimalLongitude <- runif(ObsNum,min=LongMin,max=LongMax)
EnvInvasiveBGLocations$decimalLatitude <- runif(ObsNum,min=LatMin,max=LatMax)

#Create empty dataframe for random background points for natives.
#Make it much larger than the set of presence points.
EnvNativeBGLocations <- data.frame(matrix(nrow=ObsNum,ncol=2))
colnames(EnvNativeBGLocations) <- c("decimalLongitude","decimalLatitude")
#Randomly generate longitude and latitude points.
EnvNativeBGLocations$decimalLongitude <- runif(ObsNum,min=LongMin,max=LongMax)
EnvNativeBGLocations$decimalLatitude <- runif(ObsNum,min=LatMin,max=LatMax)

#Extract the environmental map layer values at the native and invasive
#plant locations.
EnvInvasiveBG <- EnvInvasiveBGLocations
EnvNativeBG <- EnvNativeBGLocations
for(EnvLayer in EnvLayers){
  EnvFileName <- gsub("./","",gsub("_LA.tif","",EnvLayer)) #Get short names for environmental layers.
  #Extract environmental values at invasive species locations.
  EnvInvasiveBGTmp <- as.data.frame(raster::extract(raster(EnvLayer),EnvInvasiveBGLocations))
  colnames(EnvInvasiveBGTmp) <- EnvFileName
  EnvInvasiveBG <- cbind(EnvInvasiveBG,EnvInvasiveBGTmp)
  #Extract environmental values at native species locations.
  EnvNativeBGTmp <- as.data.frame(raster::extract(raster(EnvLayer),EnvNativeBGLocations))
  colnames(EnvNativeBGTmp) <- EnvFileName
  EnvNativeBG <- cbind(EnvNativeBG,EnvNativeBGTmp)
}

#Designate a presence/absence column for the pseudo-absence points.
EnvInvasiveBG$pa <- 0
EnvNativeBG$pa <- 0

#Remove rows with missing data for observations points.
EnvInvasiveBG <- na.omit(EnvInvasiveBG)
EnvNativeBG <- na.omit(EnvNativeBG)

#Run random forest models on your invasive species data.
RFImportanceTotal <- data.frame() # Initialize an empty data frame to collect random forest statistics.
modelNum <- 100 #Designate how many data subsets to run random forest on.
sampleNum <- 100 #Designate how many points to subsample your data on.
for(i in 1:model_num){
  presSubset <- EnvInvasive[,colnames(EnvInvasive) %in% c(EnvFileNames,"pa")] #Remove latitude and longitude columns from presence point subset.
  presSubset <- presSubset[sample(nrow(presSubset),sample_num),] #Subsample rows.
  absSubset <- EnvInvasiveBG[,colnames(EnvInvasiveBG) %in% c(EnvFileNames,"pa")] #Remove latitude and longitude columns from presence point subset.
  absSubset <- absSubset[sample(nrow(absSubset),10*sample_num),] #Subsample rows.
  
  #Construct a training and testing set for the presence data.
  group <- kfold(presSubset,5)
  pres_train <- presSubset[group!=1,]
  pres_test <- presSubset[group==1,]
  
  #Construct a training and testing set for the pseudo-absence data.
  group <- kfold(absSubset,5)
  backgr_train <- absSubset[group!=1,]
  backgr_test <- absSubset[group==1,]
  
  #Construct presence / pseudo-absence training sets.
  envtrain <- rbind(pres_train,backgr_train)
  
  #Parsimonious random forest model
  rf.regress <- suppressWarnings(rf.modelSel(envtrain[,colnames(envtrain) %in% EnvFileNames],envtrain$pa, imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
  RFImportance <- importance(rf.regress$rf.final)
  RFImportance <- data.frame(names=row.names(RFImportance),RFImportance)
  RFImportanceTotal <- rbind(RFImportanceTotal,RFImportance)
}
#Summary statistics on the frequency and importance of environmental parameters in random forest model.
tmp <- as.data.frame(table(RFImportanceTotal$names))
colnames(tmp) <- c("Variable","Freq")
RFImportanceTotal <- ddply(RFImportanceTotal, .(names), summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
colnames(RFImportanceTotal) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
RFImportanceTotal <- left_join(tmp,RFImportanceTotal)
#To save aggregated data frame.
write.table(RFImportanceTotal,"InvasiveRFImportance.txt",quote=FALSE,sep="\t",row.names = FALSE)
