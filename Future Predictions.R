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
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP1262140")

# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)
# 


# setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant SpeciesSSP1262140")
SSP1262140_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP1262140_envi_layers_stack <- stack(c(SSP1262140_envi_map_layers))#,other_data))

SSP1262140_invasive_future_prediction <- predict(SSP1262140_envi_layers_stack,for_prediction_rf_regress)

plot(SSP1262140_invasive_future_prediction, main='Invasive SSP 126 2021-2040 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP1262140_invasive_future_prediction
tr <- threshold(inv_predict_erf,"spec_sens")

plot(pr > tr, main='Invasive SSP 126 2021-2040 P/A')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')



SSP1262140_native_future_prediction <- predict(SSP1262140_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP1262140_native_future_prediction, main='Native SSP 126 2021-2040 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP1262140_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 126 2021-2040 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')






#####SSP1264160#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP1264160")

# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)
# 
# 
# 
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP1264160")
SSP1264160_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP1264160_envi_layers_stack <- stack(c(SSP1264160_envi_map_layers,other_data))

# SSP1264160_invasive_future_prediction <- predict(SSP1264160_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP1264160_invasive_future_prediction, main='Invasive SSP 126 2041-2060 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP1264160_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 126 2041-2060 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')



SSP1264160_native_future_prediction <- predict(SSP1264160_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP1264160_native_future_prediction, main='Native SSP 126 2041-2060 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP1264160_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 126 2041-2060 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')





#####SSP1266180#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP1266180")

# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)

# setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP1266180")
SSP1266180_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP1266180_envi_layers_stack <- stack(c(SSP1266180_envi_map_layers,other_data))

# SSP1266180_invasive_future_prediction <- predict(SSP1266180_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP1266180_invasive_future_prediction, main='Invasive SSP 126 2061-2080 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP1266180_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 126 2061-2080 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')



SSP1266180_native_future_prediction <- predict(SSP1266180_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP1266180_native_future_prediction, main='Native SSP 126 2061-2080 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP1266180_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 126 2061-2080 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')






#####SSP1268100#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP1268100")

# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)


setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP1268100")
SSP1268100_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP1268100_envi_layers_stack <- stack(c(SSP1268100_envi_map_layers,other_data))

# SSP1268100_invasive_future_prediction <- predict(SSP1268100_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP1268100_invasive_future_prediction, main='Invasive SSP 126 2081-2100 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP1268100_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 126 2081-2100 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')
# 


SSP1268100_native_future_prediction <- predict(SSP1268100_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP1268100_native_future_prediction, main='Native SSP 126 2081-2100 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP1268100_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 126 2081-2100 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')


######SSP2452140#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP2452140")
# 
# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)

setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP2452140")
SSP2452140_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP2452140_envi_layers_stack <- stack(c(SSP2452140_envi_map_layers,other_data))
# 
# SSP2452140_invasive_future_prediction <- predict(SSP2452140_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP2452140_invasive_future_prediction, main='Invasive SSP 245 2021-2040 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP2452140_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 245 2021-2040 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')



SSP2452140_native_future_prediction <- predict(SSP2452140_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP2452140_native_future_prediction, main='Native SSP 245 2021-2040 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP2452140_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 245 2021-2040 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')

#####SSP2454160#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP2454160")
# 
# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)
# 
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP2454160")
SSP2454160_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP2454160_envi_layers_stack <- stack(c(SSP2454160_envi_map_layers,other_data))
# 
# SSP2454160_invasive_future_prediction <- predict(SSP2454160_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP2454160_invasive_future_prediction, main='Invasive SSP 245 2041-2060 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP2454160_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 245 2041-2060 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')



SSP2454160_native_future_prediction <- predict(SSP2454160_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP2454160_native_future_prediction, main='Native SSP 245 2041-2060 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP1264160_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 245 2041-2060 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')


#####SSP2456180#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP2456180")
# 
# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)
# 

setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP2456180")
SSP2456180_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP2456180_envi_layers_stack <- stack(c(SSP2456180_envi_map_layers,other_data))

# SSP2456180_invasive_future_prediction <- predict(SSP2456180_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP2456180_invasive_future_prediction, main='Invasive SSP 245 2061-2080 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP2456180_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 245 2061-2080 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')
# 


SSP2456180_native_future_prediction <- predict(SSP2456180_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP2456180_native_future_prediction, main='Native SSP 245 2061-2080 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP2456180_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 245 2061-2080 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')



#####SSP2458100#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP2458100")

# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)


setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP2458100")
SSP2458100_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP2458100_envi_layers_stack <- stack(c(SSP2458100_envi_map_layers,other_data))

# SSP2458100_invasive_future_prediction <- predict(SSP2458100_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP2458100_invasive_future_prediction, main='Invasive SSP 245 2081-2100 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP2458100_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 245 2081-2100 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')



SSP2458100_native_future_prediction <- predict(SSP2458100_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP2458100_native_future_prediction, main='Native SSP 2245 2081-2100 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP2458100_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 245 2081-2100 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')


#####SSP3702140#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP3702140")
# 
# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)
# 

setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP3702140")
SSP3702140_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP3702140_envi_layers_stack <- stack(c(SSP3702140_envi_map_layers,other_data))

# SSP3702140_invasive_future_prediction <- predict(SSP3702140_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP3702140_invasive_future_prediction, main='Invasive SSP 370 2021-2040 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP3702140_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 370 2021-2040 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')



SSP3702140_native_future_prediction <- predict(SSP3702140_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP3702140_native_future_prediction, main='Native SSP 370 2021-2040 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP3702140_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 370 2021-2040 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')


#####SSP3704160#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP3704160")

# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)

setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP3704160")
SSP3704160_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP3704160_envi_layers_stack <- stack(c(SSP3704160_envi_map_layers,other_data))

# SSP3704160_invasive_future_prediction <- predict(SSP3704160_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP3704160_invasive_future_prediction, main='Invasive SSP 370 2041-2060 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP3704160_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 370 2041-2060 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')



SSP3704160_native_future_prediction <- predict(SSP3704160_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP3704160_native_future_prediction, main='Native SSP 370 2041-2060 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP3704160_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 370 2041-2060 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')



#####SSP3706180#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP3706180")
# 
# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)
# 

setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP3706180")
SSP3706180_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP3706180_envi_layers_stack <- stack(c(SSP3706180_envi_map_layers,other_data))

# SSP3706180_invasive_future_prediction <- predict(SSP3706180_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP3706180_invasive_future_prediction, main='Invasive SSP 370 2061-2080 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP3706180_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 370 2061-2080 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')



SSP3706180_native_future_prediction <- predict(SSP3706180_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP3706180_native_future_prediction, main='Native SSP 370 2061-2080 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP3706180_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 370 2061-2080 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')

#####SSP3708100#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP3708100")

# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)

setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP3708100")
SSP3708100_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP3708100_envi_layers_stack <- stack(c(SSP3708100_envi_map_layers,other_data))

# SSP3708100_invasive_future_prediction <- predict(SSP3708100_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP3708100_invasive_future_prediction, main='Invasive SSP 370 2081-2100 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP3708100_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 370 2081-2100 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')



SSP3708100_native_future_prediction <- predict(SSP3708100_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP3708100_native_future_prediction, main='Native SSP 370 2081-2100 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP3708100_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 370 2081-2100 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')


#####SSP5852140#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP5852140")
# 
# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)


setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP5852140")
SSP5852140_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP5852140_envi_layers_stack <- stack(c(SSP5852140_envi_map_layers,other_data))

# SSP5852140_invasive_future_prediction <- predict(SSP5852140_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP5852140_invasive_future_prediction, main='Invasive SSP 585 2021-2040 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP5852140_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 585 2021-2040 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')



SSP5852140_native_future_prediction <- predict(SSP5852140_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP5852140_native_future_prediction, main='Native SSP 585 2021-2040 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP5852140_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 585 2021-2040 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')

#####SSP5854160#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP5854160")
# 
# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)

setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP5854160")
SSP5854160_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP5854160_envi_layers_stack <- stack(c(SSP5854160_envi_map_layers,other_data))
# 
# SSP5854160_invasive_future_prediction <- predict(SSP5854160_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP5854160_invasive_future_prediction, main='Invasive SSP 585 2041-2060 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP5854160_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 585 2041-2060 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')



SSP5854160_native_future_prediction <- predict(SSP5854160_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP5854160_native_future_prediction, main='Native SSP 585 2041-2060 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP5854160_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 585 2041-2060 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')


#####SSP5856180#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP5856180")
# 
# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)


setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP5856180")
SSP5856180_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP5856180_envi_layers_stack <- stack(c(SSP5856180_envi_map_layers,other_data))

# SSP5856180_invasive_future_prediction <- predict(SSP5856180_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP5856180_invasive_future_prediction, main='Invasive SSP 585 2061-2080 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP5856180_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 585 2061-2080 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')



SSP5856180_native_future_prediction <- predict(SSP5856180_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP5856180_native_future_prediction, main='Native SSP 585 2061-2080 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP5856180_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 585 2061-2080 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')



#####SSP5858100#####
setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP5858100")
# 
# amt <- raster("amt.tif")
# test_LA_amt <- projectRaster(amt, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_amt, "amt.tif", overwrite=TRUE)
# 
# mdr <- raster("mdr.tif")
# test_LA_mdr <- projectRaster(mdr, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mdr, "mdr.tif", overwrite=TRUE)
# 
# isoth <- raster("isoth.tif")
# test_LA_isoth <- projectRaster(isoth, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_isoth, "isoth.tif", overwrite=TRUE)
# 
# ts <- raster("ts.tif")
# test_LA_ts <- projectRaster(ts, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ts, "ts.tif", overwrite=TRUE)
# 
# mtwm <- raster("mtwm.tif")
# test_LA_mtwm <- projectRaster(mtwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwm, "mtwm.tif", overwrite=TRUE)
# 
# mtcm <- raster("mtcm.tif")
# test_LA_mtcm <- projectRaster(mtcm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcm, "mtcm.tif", overwrite=TRUE)
# 
# tar <- raster("tar.tif")
# test_LA_tar <- projectRaster(tar, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_tar, "tar.tif", overwrite=TRUE)
# 
# mtwetq <- raster("mtwetq.tif")
# test_LA_mtwetq <- projectRaster(mtwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwetq, "mtwetq.tif", overwrite=TRUE)
# 
# mtdq <- raster("mtdq.tif")
# test_LA_mtdq <- projectRaster(mtdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtdq, "mtdq.tif", overwrite=TRUE)
# 
# mtwarmq <- raster("mtwarmq.tif")
# test_LA_mtwarmq <- projectRaster(mtwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtwarmq, "mtwarmq.tif", overwrite=TRUE)
# 
# mtcq <- raster("mtcq.tif")
# test_LA_mtcq <- projectRaster(mtcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_mtcq, "mtcq.tif", overwrite=TRUE)
# 
# ap <- raster("ap.tif")
# test_LA_ap <- projectRaster(ap, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ap, "ap.tif", overwrite=TRUE)
# 
# pwm <- raster("pwm.tif")
# test_LA_pwm <- projectRaster(pwm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwm, "pwm.tif", overwrite=TRUE)
# 
# pdm <- raster("pdm.tif")
# test_LA_pdm <- projectRaster(pdm, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdm, "pdm.tif", overwrite=TRUE)
# 
# ps <- raster("ps.tif")
# test_LA_ps <- projectRaster(ps, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_ps, "ps.tif", overwrite=TRUE)
# 
# pwetq <- raster("pwetq.tif")
# test_LA_pwetq <- projectRaster(pwetq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwetq, "pwetq.tif", overwrite=TRUE)
# 
# pdq <- raster("pdq.tif")
# test_LA_pdq <- projectRaster(pdq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pdq, "pdq.tif", overwrite=TRUE)
# 
# pwarmq <- raster("pwarmq.tif")
# test_LA_pwarmq <- projectRaster(pwarmq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pwarmq, "pwarmq.tif", overwrite=TRUE)
# 
# pcq <- raster("pcq.tif")
# test_LA_pcq <- projectRaster(pcq, LA_hfp,method = 'bilinear')
# writeRaster(test_LA_pcq, "pcq.tif", overwrite=TRUE)


setwd("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/SSP5858100")
SSP5858100_envi_map_layers <- list.files(".", pattern='.tif',full.names = TRUE)
other_data <- list.files("C:/Users/user/Desktop/The Spread of Invasvie Plant Species/otherdata", pattern=".tif", full.names = TRUE)
SSP5858100_envi_layers_stack <- stack(c(SSP5858100_envi_map_layers,other_data))

# SSP5858100_invasive_future_prediction <- predict(SSP5858100_envi_layers_stack,for_prediction_rf_regress)
# 
# plot(SSP5858100_invasive_future_prediction, main='Invasive SSP 585 2081-2100 Prediction')
# plot(LA_spdf, add=TRUE,border='dark grey')
# pr <- SSP5858100_invasive_future_prediction
# tr <- threshold(inv_predict_erf,"spec_sens")
# 
# plot(pr > tr, main='Invasive SSP 585 2081-2100 P/A')
# plot(LA_spdf, add=TRUE, border='dark grey')
# points(pres_train, pch='+')
# points(backgr_train, pch='-')



SSP5858100_native_future_prediction <- predict(SSP5858100_envi_layers_stack,nat_for_prediction_rf_regress)

plot(SSP5858100_native_future_prediction, main='Native SSP 585 2081-2100 Prediction')
plot(LA_spdf, add=TRUE,border='dark grey')
pr <- SSP5858100_native_future_prediction
tr <- threshold(nat_erf,"spec_sens")

plot(pr > tr, main='Native SSP 585 2081-2100 Presence/Absence')
plot(LA_spdf, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backgr_train, pch='-')



#####END#####
setwd("D:/Science Research/2020-2021 Research Project/Scripts")
 