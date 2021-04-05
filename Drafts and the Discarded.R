
# set up a variable in each dataframe to designate 
# whether or not we're dealing with presence locations 
# or pseudo-absences locations.


#presence data
sample_Num <- 6651
invasive_pres_Subset <- invasive_with_envi
#adding a column called pa with values of all 1
invasive_pres_Subset$pa <- 1
#rearrange the sequence of columns so that pa is the first column
invasive_pres_Subset <- invasive_pres_Subset[,c(ncol(invasive_pres_Subset),1:ncol(invasive_pres_Subset)-1)]
#selecting samples from the dataframe...is this required in the context of this project?
invasive_pres_Subset <- invasive_pres_Subset[sample(nrow(invasive_pres_Subset),sample_Num),]

sample_Num <- 6539
native_pres_Subset <- native_with_envi
native_pres_Subset$pa <- 1
native_pres_Subset <- native_pres_Subset[,c(ncol(native_pres_Subset),1:ncol(native_pres_Subset)-1)]
native_pres_Subset <- native_pres_Subset[sample(nrow(native_pres_Subset),sample_Num),]

#pseudo-absence data
invasive_abs_Subset <- invasive_abs_with_envi
invasive_abs_Subset$pa <- 0
invasive_abs_Subset <- invasive_abs_Subset[,c(ncol(invasive_abs_Subset),1:ncol(invasive_abs_Subset)-1)]
# invasive_abs_Subset <- invasive_abs_Subset[sample(nrow(invasive_abs_Subset),10*sample_Num),]

native_abs_Subset <- native_abs_with_envi
native_abs_Subset$pa <- 0
native_abs_Subset <- native_abs_Subset[,c(ncol(native_abs_Subset),1:ncol(native_abs_Subset)-1)]


#Run random forests models for 
#both the invasive and native plant species.

#install.packages("randomForest")
library(randomForest)
library(raster)
library(sp)
# The following object is masked from ¡®package:dplyr¡¯:
#   
#   combine
# 
# The following object is masked from ¡®package:ggplot2¡¯:
#   
#   margin

#Construct a training and testing set for the presence data.
#k-fold partitioning of a data set for model testing 
#purposes. Each record in a matrix (or similar data structure) is randomly 
#assigned to a group. Group numbers are between 1 and k.
#Does it matter how many groups we have??
invasive_pres_group <- kfold(invasive_pres_Subset,5)
invasive_pres_train <- invasive_pres_Subset[invasive_pres_group!=1,]
invasive_pres_test <- invasive_pres_Subset[invasive_pres_group==1,]


#Construct a training and testing set for the pseudo-absence data.
invasive_abs_group <- kfold(invasive_abs_Subset,5)
#notice that the absence data is considered as "background"
invasive_backgr_train <- invasive_abs_Subset[invasive_abs_group!=1,]
invasive_backgr_test <- invasive_abs_Subset[invasive_abs_group==1,]

#Construct presence / pseudo-absence training sets.
#fix the column names...
invasive_backgr_train <- invasive_backgr_train %>% 
  rename(
    decimalLongitude = x,
    decimalLatitude = y
  )
envi_train <- rbind(invasive_pres_train,invasive_backgr_train)

invasive_testpres <- invasive_pres_test
invasive_testbackgr <- invasive_backgr_test

#excluding NA in predictors
envi_train[is.na(envi_train)] <- NA
envi_train <- na.omit(envi_train)

invasive_testpres[is.na(invasive_testpres)] <- NA
invasive_testpres <- na.omit(invasive_testpres)

invasive_testbackgr[is.na(invasive_testbackgr)] <- NA
invasive_testbackgr <- na.omit(invasive_testbackgr)


#Parsimonious random forest model
#########BEWARE! THE DATA IS NOT REPRESENTATIVE OF AN 
#########OPTIMAL SIMULATION ENVIRONMENT
#########AS THE ABSCENCE DATA IS ONLY 100
#########DUE TO UNFORTUNATE LIMTATIONS THAT I'M 
#########NOT QUITE SURE HOW TO SOLVE
#How should I determine the parameters here?
# install.packages("randomForest")


#Can't find rf.modelSel???
invasive_rf_regress <- rf.modelSel(envi_train[,4:ncol(envtrain)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1)

#Convert it to "factor" form to make randomForest run in "classification" form

envi_train_as_factors <- as.factor(envi_train)

invasive_rf_regress <- suppressWarnings(randomForest(envi_train[,4:ncol(envi_train)],envi_train[,c(1)], imp.scale="mir", parsimony=0.03,final.model=TRUE,seed=1))
invasive_RFImportance <- importance(invasive_rf_regress)
invasive_RFImportance <- data.frame(names=row.names(invasive_RFImportance),invasive_RFImportance)
invasive_RFImportanceTotal <- data.frame()
invasive_RFImportanceTotal <- rbind(invasive_RFImportanceTotal,invasive_RFImportance)
invasive_erf <- suppressWarnings(evaluate(invasive_testpres,invasive_testbackgr,invasive_rf_regress))

##Directly predicting??
#Convert it to "factor" form to make randomForest run in "classification" form
invasive_rf_regress_as_factors <- as.data.frame(invasive_rf_regress)
invasive_rf_regress_as_factors <- as.factor(invasive_rf_regress_as_factors)
predict(envi_layers_stack, invasive_rf_regress, "model.probs.img", type= "prob", index=2, na.rm=TRUE, overwrite=TRUE, progress="window")

invasive_RFEvaluation <-  data.frame(matrix(nrow=1,ncol=6))
colnames(RFEvaluation) <- c("AUC","cor","kappa","Q","TSS")
invasive_RFEvaluation$AUC <- invasive_erf@auc
invasive_RFEvaluation$cor <- invasive_erf@cor
invasive_RFEvaluation$kappa <- max(invasive_erf@kappa)
#need to do reading and understand why these values are picked and what they all mean


#To use ddply()
library(plyr)
#Summary statistics on the frequency and importance of environmental parameters in random forest model.
invasive_amt <- as.data.frame(table(invasive_RFImportanceTotal$names))
colnames(invasive_amt) <- c("Variable","Freq")
invasive_RFImportanceTotal <- ddply(invasive_RFImportanceTotal, .(names), summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
##*** understand ddply() and its parameters
colnames(invasive_RFImportanceTotal) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
#What's the importance of using left_join()?
invasive_RFImportanceTotal <- left_join(invasive_amt,invasive_RFImportanceTotal)

invasive_ap <- as.data.frame(table(invasive_RFImportanceTotal$names))
colnames(invasive_ap) <- c("Variable","Freq")
invasive_RFImportanceTotal <- ddply(invasive_RFImportanceTotal, .(names), summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
colnames(invasive_RFImportanceTotal) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
invasive_RFImportanceTotal <- left_join(invasive_ap,invasive_RFImportanceTotal)


invasive_hfp <- as.data.frame(table(invasive_RFImportanceTotal$names))
colnames(invasive_hfp) <- c("Variable","Freq")
invasive_RFImportanceTotal <- ddply(invasive_RFImportanceTotal, .(names), summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
colnames(invasive_RFImportanceTotal) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
invasive_RFImportanceTotal <- left_join(invasive_hfp,invasive_RFImportanceTotal)

invasive_ntot <- as.data.frame(table(invasive_RFImportanceTotal$names))
colnames(invasive_ntot) <- c("Variable","Freq")
invasive_RFImportanceTotal <- ddply(invasive_RFImportanceTotal, .(names), summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
colnames(invasive_RFImportanceTotal) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
invasive_RFImportanceTotal <- left_join(invasive_ntot,invasive_RFImportanceTotal)

invasive_phihox <- as.data.frame(table(invasive_RFImportanceTotal$names))
colnames(invasive_phihox) <- c("Variable","Freq")
invasive_RFImportanceTotal <- ddply(invasive_RFImportanceTotal, .(names), summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
colnames(invasive_RFImportanceTotal) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
invasive_RFImportanceTotal <- left_join(invasive_phihox,invasive_RFImportanceTotal)

invasive_ptrcv <- as.data.frame(table(invasive_RFImportanceTotal$names))
colnames(invasive_ptrcv) <- c("Variable","Freq")
invasive_RFImportanceTotal <- ddply(invasive_RFImportanceTotal, .(names), summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
colnames(invasive_RFImportanceTotal) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
invasive_RFImportanceTotal <- left_join(invasive_ptrcv,invasive_RFImportanceTotal)



#To save aggregated data frame.
write.table(invasive_RFImportanceTotal,paste(invasive_RFImportanceTotal,"invasive_RFImportance.txt",sep=""),quote=FALSE,sep="\t",row.names = FALSE)


# Calculate Yule's Q (used in the second randomForest in the coastlight example)
#Yule¡¯s Q is just the 2¡Á2 version of the gamma coefficient
#which tests how closely two pairs of data points ¡°match¡± and (for an association between points) 
#the strength of association
#Yule's Q is used when you have two dichotomous variables (e.g. responses that are yes/no)
#formula see https://www.statisticshowto.com/gamma-coefficient-goodman-kruskal/#YulesQ

#Is Gamma coefficient needed here instead, as the variables I'm testing
#doesn't really have yes/no values?
#or is it needed for some other variables
#The basic example given on the website
#does perdictions right after evaluating rf 

tmp <- erf@OR
tmp[!is.finite(tmp)] <- NA
RFEvaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
RFEvaluation$TSS <- mean(erf@TPR,na.rm=T)+mean(erf@TNR,na.rm=T)-1
RFEvaluationTotal <- rbind(RFEvaluationTotal,RFEvaluation)

#Store partial response data for each environmental factor in the random forest model.
if(species=="Plover"){
  RFp1 <- partial(rf.regress,pred.var = "Freshwater10mAligned",train=envtrain[,c(4:ncol(envtrain))])
  RFp1Total <- rbind(RFp1Total,RFp1)
}
RFp2 <- partial(rf.regress,pred.var = "LogSI10mAligned",train=envtrain[,c(4:ncol(envtrain))])
RFp2Total <- rbind(RFp2Total,RFp2)
RFp3 <- partial(rf.regress,pred.var = "SoCalBeachWidth10mAligned",train=envtrain[,c(4:ncol(envtrain))])
RFp3Total <- rbind(RFp3Total,RFp3)
}
#Summary statistics on the frequency and importance of environmental parameters in random forest model.
tmp <- as.data.frame(table(RFImportanceTotal$names))
colnames(tmp) <- c("Variable","Freq")
RFImportanceTotal <- ddply(RFImportanceTotal, .(names), summarize,  MeanIncNodePurity=mean(IncNodePurity), SDIncNodePurity=sd(IncNodePurity))
colnames(RFImportanceTotal) <- c("Variable","MeanIncNodePurity","SDIncNodePurity")
RFImportanceTotal <- left_join(tmp,RFImportanceTotal)
#To save aggregated data frame.
write.table(RFImportanceTotal,paste(species,"ParsimonyFinalRFImportance.txt",sep=""),quote=FALSE,sep="\t",row.names = FALSE)
tmpMean <- colMeans(RFEvaluationTotal)
tmpSD <- apply(RFEvaluationTotal,2,sd)
RFEvaluationTotal <- as.data.frame(rbind(tmpMean,tmpSD))





# Calculate Yule's Q.
tmp <- erf@OR
tmp[!is.finite(tmp)] <- NA 
RFEvaluation$Q <- (mean(tmp,na.rm=T)-1)/(mean(tmp,na.rm=T)+1)
RFEvaluation$TSS <- mean(erf@TPR,na.rm=T)+mean(erf@TNR,na.rm=T)-1
RFEvaluationTotal <- rbind(RFEvaluationTotal,RFEvaluation)
#Store partial response data for each environmental factor in the random forest model.
if(species=="Plover"){
  RFp1 <- partial(rf.regress,pred.var = "Freshwater10mAligned",train=envtrain[,c(4:ncol(envtrain))])
  RFp1Total <- rbind(RFp1Total,RFp1)
}
RFp2 <- partial(rf.regress,pred.var = "LogSI10mAligned",train=envtrain[,c(4:ncol(envtrain))])
RFp2Total <- rbind(RFp2Total,RFp2)
RFp3 <- partial(rf.regress,pred.var = "SoCalBeachWidth10mAligned",train=envtrain[,c(4:ncol(envtrain))])
RFp3Total <- rbind(RFp3Total,RFp3)
}                                                                             
