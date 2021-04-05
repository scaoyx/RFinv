rm(list=ls())
require(raster)
require(sp)
require(rgdal)
require(dismo)
require(rfUtilities)
require(randomForest)
require(ggplot2)
require(cowplot)

setwd("/Users/levisimons/Desktop/InvasivePlants/")

#Read in plant observation data.
input <- read.table("OriginalPlantData.csv", header=T, sep=",",as.is=T,skip=0,fill=T,quote="\"",check.names=F,encoding = "UTF-8")

#Read in invasive plant species list.
InvasivePlants <- read.table("InvasivePlants.csv", header=T, sep=",",as.is=T,skip=0,fill=T,quote="\"",check.names=F,encoding = "UTF-8")
InvasivePlants <- InvasivePlants$ScientificName

#Read in native plant species list.
NativePlants <- read.table("NativePlants.csv", header=T, sep=",",as.is=T,skip=0,fill=T,quote="\"",check.names=F,encoding = "UTF-8")
NativePlants <- NativePlants$ScientificName

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
#Initialize data frames to store partial response curves.
RFp1Total <- data.frame()
RFp2Total <- data.frame()
RFp3Total <- data.frame()
RFp4Total <- data.frame()
RFp5Total <- data.frame()
RFp6Total <- data.frame()
modelNum <- 100 #Designate how many data subsets to run random forest on.
sampleNum <- 100 #Designate how many points to subsample your data on.
for(i in 1:modelNum){
  presSubset <- EnvInvasive[,colnames(EnvInvasive) %in% c(EnvFileNames,"pa")] #Remove latitude and longitude columns from presence point subset.
  presSubset <- presSubset[sample(nrow(presSubset),sampleNum),] #Subsample rows.
  absSubset <- EnvInvasiveBG[,colnames(EnvInvasiveBG) %in% c(EnvFileNames,"pa")] #Remove latitude and longitude columns from presence point subset.
  absSubset <- absSubset[sample(nrow(absSubset),10*sampleNum),] #Subsample rows.
  
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
  rf1 <- suppressWarnings(tuneRF(envtrain[,colnames(envtrain) %in% EnvFileNames],envtrain$pa,stepFactor=1,plot=FALSE,doBest=TRUE))
  RFImportance <- importance(rf1)
  RFImportance <- data.frame(names=row.names(RFImportance),RFImportance)
  RFImportanceTotal <- rbind(RFImportanceTotal,RFImportance)
  
  #Store partial response curve for random forest model.
  RFp1 <- as.data.frame(partialPlot(rf1,pred.data=envtrain[,colnames(envtrain) %in% EnvFileNames],x.var="amt"))
  RFp1Total <- rbind(RFp1Total,RFp1)
  RFp2 <- as.data.frame(partialPlot(rf1,pred.data=envtrain[,colnames(envtrain) %in% EnvFileNames],x.var="ap"))
  RFp2Total <- rbind(RFp2Total,RFp2)
  RFp3 <- as.data.frame(partialPlot(rf1,pred.data=envtrain[,colnames(envtrain) %in% EnvFileNames],x.var="hfp"))
  RFp3Total <- rbind(RFp3Total,RFp3)
  RFp4 <- as.data.frame(partialPlot(rf1,pred.data=envtrain[,colnames(envtrain) %in% EnvFileNames],x.var="ntot"))
  RFp4Total <- rbind(RFp4Total,RFp4)
  RFp5 <- as.data.frame(partialPlot(rf1,pred.data=envtrain[,colnames(envtrain) %in% EnvFileNames],x.var="phihox"))
  RFp5Total <- rbind(RFp5Total,RFp5)
  RFp6 <- as.data.frame(partialPlot(rf1,pred.data=envtrain[,colnames(envtrain) %in% EnvFileNames],x.var="ptrcv"))
  RFp6Total <- rbind(RFp6Total,RFp6)
}
#Summary statistics on the frequency and importance of environmental parameters in random forest model.
tmp <- as.data.frame(table(RFImportanceTotal$names))
colnames(tmp) <- c("Variable","Freq")
RFImportanceTotal <- ddply(RFImportanceTotal, .(names), summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
colnames(RFImportanceTotal) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
RFImportanceTotal <- left_join(tmp,RFImportanceTotal)
#To save aggregated data frame.
write.table(RFImportanceTotal,"InvasiveRFImportance.txt",quote=FALSE,sep="\t",row.names = FALSE)

#Rename partial response dataframe variables for plotting.
colnames(RFp1Total) <- c("amt","Detection Probability")
colnames(RFp2Total) <- c("ap","Detection Probability")
colnames(RFp3Total) <- c("hfp","Detection Probability")
colnames(RFp4Total) <- c("ntot","Detection Probability")
colnames(RFp5Total) <- c("phihox","Detection Probability")
colnames(RFp6Total) <- c("ptrcv","Detection Probability")

#Create 2d histograms with best-fit splines for the partial response curves.
RFp1Plot <- ggplot(RFp1Total, aes(x=amt, y=`Detection Probability`) )+xlab("amt")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp1Total))+theme_bw(base_size=25)
RFp2Plot <- ggplot(RFp2Total, aes(x=ap, y=`Detection Probability`) )+xlab("ap")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp2Total))+theme_bw(base_size=25)
RFp3Plot <- ggplot(RFp3Total, aes(x=hfp, y=`Detection Probability`) )+xlab("hfp")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp3Total))+theme_bw(base_size=25)
RFp4Plot <- ggplot(RFp4Total, aes(x=ntot, y=`Detection Probability`) )+xlab("ntot")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp4Total))+theme_bw(base_size=25)
RFp5Plot <- ggplot(RFp5Total, aes(x=phihox, y=`Detection Probability`) )+xlab("phihox")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp5Total))+theme_bw(base_size=25)
RFp6Plot <- ggplot(RFp6Total, aes(x=ptrcv, y=`Detection Probability`) )+xlab("ptrcv")+ylab("Detection\nProbability")+geom_bin2d(bins = 50)+scale_fill_continuous(type = "viridis")+stat_smooth(aes(y = `Detection Probability`, fill=`Detection Probability`),method="auto",color="violet",fill="red",n=0.1*nrow(RFp6Total))+theme_bw(base_size=25)
RFPlots <- plot_grid(RFp1Plot,RFp2Plot,RFp3Plot,RFp4Plot,RFp5Plot,RFp6Plot,ncol=2,labels="AUTO")
png(paste("RFPlotsInvasives.png",sep=""),width=2*800,height=400*length(EnvFileNames))
RFPlots
print(RFPlots)
dev.off()



