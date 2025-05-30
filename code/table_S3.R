# table s5- table displaying at line modeling results

# load packages

library(tidyverse)
library(caret)

# load data frames

tA_full_train <- readRDS("../data/processed/dataframes/laboratoryatline_full_training.RDS")

tA_full_test <- readRDS("../data/processed/dataframes/laboratoryatline_full_testing.RDS")
  
tA_full_indval <- readRDS("../data/processed/dataframes/laboratoryatline_full_indval.RDS")
  
tA_nironefeasibility_train <- readRDS("../data/processed/dataframes/laboratoryatline_lowcostfeasibility_training.RDS")
  
tA_nironefeasibility_test <- readRDS("../data/processed/dataframes/laboratoryatline_lowcostfeasibility_testing.RDS")
  
tA_nironefeasibility_indval <- readRDS("../data/processed/dataframes/laboratoryatline_lowcostfeasibility_indval.RDS")
  
nirone_train <- readRDS("../data/processed/dataframes/lowcostatline_training.RDS")

nirone_test <- readRDS("../data/processed/dataframes/lowcostatline_testing.RDS")

nirone_indval <- readRDS("../data/processed/dataframes/lowcostatline_indval.RDS")

TA_full_PC <- readRDS("../data/processed/dataframes/laboratoryatline_full_PCs.RDS")

TA_nironefeasibility_PCs <-  readRDS("../data/processed/dataframes/laboratoryatline_lowcostfeasibility_PCs.RDS")

nirone_PCs <- readRDS("../data/processed/dataframes/lowcostatline_PCs.RDS")

# collect model performance statistics for each constituent for each model, normalizing each performance 
# metric by the range of the constituent observed in the calibration dataset
# save results as csv file

data.frame(parameters = c("Constituent","RMSEC","RMSECV","RMSEP","RMSEP-INDEXP",
                          "R2C","R2CV","R2P","R2P-INDEXP", "ncalsamples", "ncomp"),
           TA_full = c("Glucose",
                    caret::RMSE(tA_full_train$glucose_cal, tA_full_train$glucose_gperL),
                    caret::RMSE(tA_full_train$glucose_cv, tA_full_train$glucose_gperL),
                    caret::RMSE(tA_full_test$glucose_pred, tA_full_test$glucose_gperL),
                    caret::RMSE(tA_full_indval$glucose_pred, tA_full_indval$glucose_gperL),
                    caret::R2(tA_full_train$glucose_cal, tA_full_train$glucose_gperL),
                    caret::R2(tA_full_train$glucose_cv, tA_full_train$glucose_gperL),
                    caret::R2(tA_full_test$glucose_pred, tA_full_test$glucose_gperL),
                    caret::R2(tA_full_indval$glucose_pred, tA_full_indval$glucose_gperL),
                    nCalSamples = dim(tA_full_train)[1],
                    ncomp = TA_full_PC$Glucose ),
           TA_nironefeasibility = c("Glucose",
                    caret::RMSE(tA_nironefeasibility_train$glucose_cal, tA_nironefeasibility_train$glucose_gperL),
                    caret::RMSE(tA_nironefeasibility_train$glucose_cv, tA_nironefeasibility_train$glucose_gperL),
                    caret::RMSE(tA_nironefeasibility_test$glucose_pred, tA_nironefeasibility_test$glucose_gperL),
                    caret::RMSE(tA_nironefeasibility_indval$glucose_pred, tA_nironefeasibility_indval$glucose_gperL),
                    caret::R2(tA_nironefeasibility_train$glucose_cal, tA_nironefeasibility_train$glucose_gperL),
                    caret::R2(tA_nironefeasibility_train$glucose_cv, tA_nironefeasibility_train$glucose_gperL),
                    caret::R2(tA_nironefeasibility_test$glucose_pred, tA_nironefeasibility_test$glucose_gperL),
                    caret::R2(tA_nironefeasibility_indval$glucose_pred, tA_nironefeasibility_indval$glucose_gperL),
                    nCalSamples = dim(tA_nironefeasibility_train)[1],
                    ncomp = TA_nironefeasibility_PCs$Glucose),
           nirone = c("Glucose",
                      caret::RMSE(nirone_train$glucose_cal, nirone_train$glucose_gperL),
                      caret::RMSE(nirone_train$glucose_cv, nirone_train$glucose_gperL),
                      caret::RMSE(nirone_test$glucose_pred, nirone_test$glucose_gperL),
                      caret::RMSE(nirone_indval$glucose_pred, nirone_indval$glucose_gperL),
                      caret::R2(nirone_train$glucose_cal, nirone_train$glucose_gperL),
                      caret::R2(nirone_train$glucose_cv, nirone_train$glucose_gperL),
                      caret::R2(nirone_test$glucose_pred, nirone_test$glucose_gperL),
                      caret::R2(nirone_indval$glucose_pred, nirone_indval$glucose_gperL),
                      nCalSamples = dim(nirone_train)[1],
                      ncomp = nirone_PCs$Glucose),
           TA_full = c("xylose",
                        caret::RMSE(tA_full_train$xylose_cal, tA_full_train$xylose_gperL),
                        caret::RMSE(tA_full_train$xylose_cv, tA_full_train$xylose_gperL),
                        caret::RMSE(tA_full_test$xylose_pred, tA_full_test$xylose_gperL),
                        caret::RMSE(tA_full_indval$xylose_pred, tA_full_indval$xylose_gperL),
                        caret::R2(tA_full_train$xylose_cal, tA_full_train$xylose_gperL),
                        caret::R2(tA_full_train$xylose_cv, tA_full_train$xylose_gperL),
                        caret::R2(tA_full_test$xylose_pred, tA_full_test$xylose_gperL),
                        caret::R2(tA_full_indval$xylose_pred, tA_full_indval$xylose_gperL),
                       nCalSamples = dim(tA_full_train)[1],
                       ncomp = TA_full_PC$Xylose),
           TA_nironefeasibility = c("xylose",
                                     caret::RMSE(tA_nironefeasibility_train$xylose_cal, tA_nironefeasibility_train$xylose_gperL),
                                     caret::RMSE(tA_nironefeasibility_train$xylose_cv, tA_nironefeasibility_train$xylose_gperL),
                                     caret::RMSE(tA_nironefeasibility_test$xylose_pred, tA_nironefeasibility_test$xylose_gperL),
                                     caret::RMSE(tA_nironefeasibility_indval$xylose_pred, tA_nironefeasibility_indval$xylose_gperL),
                                     caret::R2(tA_nironefeasibility_train$xylose_cal, tA_nironefeasibility_train$xylose_gperL),
                                     caret::R2(tA_nironefeasibility_train$xylose_cv, tA_nironefeasibility_train$xylose_gperL),
                                     caret::R2(tA_nironefeasibility_test$xylose_pred, tA_nironefeasibility_test$xylose_gperL),
                                     caret::R2(tA_nironefeasibility_indval$xylose_pred, tA_nironefeasibility_indval$xylose_gperL),
                                    nCalSamples = dim(tA_nironefeasibility_train)[1],
                                    ncomp = TA_nironefeasibility_PCs$Xylose),
           nirone = c("xylose",
                      caret::RMSE(nirone_train$xylose_cal, nirone_train$xylose_gperL),
                      caret::RMSE(nirone_train$xylose_cv, nirone_train$xylose_gperL),
                      caret::RMSE(nirone_test$xylose_pred, nirone_test$xylose_gperL),
                      caret::RMSE(nirone_indval$xylose_pred, nirone_indval$xylose_gperL),
                      caret::R2(nirone_train$xylose_cal, nirone_train$xylose_gperL),
                      caret::R2(nirone_train$xylose_cv, nirone_train$xylose_gperL),
                      caret::R2(nirone_test$xylose_pred, nirone_test$xylose_gperL),
                      caret::R2(nirone_indval$xylose_pred, nirone_indval$xylose_gperL),
                      nCalSamples = dim(nirone_train)[1],
                      ncomp = nirone_PCs$Xylose),
TA_full = c("bdo",
             caret::RMSE(tA_full_train$bdo_cal, tA_full_train$totalBDO_gperL),
             caret::RMSE(tA_full_train$bdo_cv, tA_full_train$totalBDO_gperL),
             caret::RMSE(tA_full_test$bdo_pred, tA_full_test$totalBDO_gperL),
             caret::RMSE(tA_full_indval$bdo_pred, tA_full_indval$totalBDO_gperL),
             caret::R2(tA_full_train$bdo_cal, tA_full_train$totalBDO_gperL),
             caret::R2(tA_full_train$bdo_cv, tA_full_train$totalBDO_gperL),
             caret::R2(tA_full_test$bdo_pred, tA_full_test$totalBDO_gperL),
             caret::R2(tA_full_indval$bdo_pred, tA_full_indval$totalBDO_gperL),
            nCalSamples = dim(tA_full_train)[1],
            ncomp = TA_full_PC$BDO),
TA_nironefeasibility = c("bdo",
                          caret::RMSE(tA_nironefeasibility_train$bdo_cal, tA_nironefeasibility_train$totalBDO_gperL),
                          caret::RMSE(tA_nironefeasibility_train$bdo_cv, tA_nironefeasibility_train$totalBDO_gperL),
                          caret::RMSE(tA_nironefeasibility_test$bdo_pred, tA_nironefeasibility_test$totalBDO_gperL),
                          caret::RMSE(tA_nironefeasibility_indval$bdo_pred, tA_nironefeasibility_indval$totalBDO_gperL),
                          caret::R2(tA_nironefeasibility_train$bdo_cal, tA_nironefeasibility_train$totalBDO_gperL),
                          caret::R2(tA_nironefeasibility_train$bdo_cv, tA_nironefeasibility_train$totalBDO_gperL),
                          caret::R2(tA_nironefeasibility_test$bdo_pred, tA_nironefeasibility_test$totalBDO_gperL),
                          caret::R2(tA_nironefeasibility_indval$bdo_pred, tA_nironefeasibility_indval$totalBDO_gperL),
                         nCalSamples = dim(tA_nironefeasibility_train)[1],
                         ncomp = TA_nironefeasibility_PCs$BDO),
nirone = c("bdo",
           caret::RMSE(nirone_train$bdo_cal, nirone_train$totalBDO_gperL),
           caret::RMSE(nirone_train$bdo_cv, nirone_train$totalBDO_gperL),
           caret::RMSE(nirone_test$bdo_pred, nirone_test$totalBDO_gperL),
           caret::RMSE(nirone_indval$bdo_pred, nirone_indval$totalBDO_gperL),
           caret::R2(nirone_train$bdo_cal, nirone_train$totalBDO_gperL),
           caret::R2(nirone_train$bdo_cv, nirone_train$totalBDO_gperL),
           caret::R2(nirone_test$bdo_pred, nirone_test$totalBDO_gperL),
           caret::R2(nirone_indval$bdo_pred, nirone_indval$totalBDO_gperL),
           nCalSamples = dim(nirone_train)[1],
           ncomp = nirone_PCs$BDO),
TA_full = c("acetoin",
             caret::RMSE(tA_full_train$acetoin_cal, tA_full_train$acetoin_gperL),
             caret::RMSE(tA_full_train$acetoin_cv, tA_full_train$acetoin_gperL),
             caret::RMSE(tA_full_test$acetoin_pred, tA_full_test$acetoin_gperL),
             caret::RMSE(tA_full_indval$acetoin_pred, tA_full_indval$acetoin_gperL),
             caret::R2(tA_full_train$acetoin_cal, tA_full_train$acetoin_gperL),
             caret::R2(tA_full_train$acetoin_cv, tA_full_train$acetoin_gperL),
             caret::R2(tA_full_test$acetoin_pred, tA_full_test$acetoin_gperL),
             caret::R2(tA_full_indval$acetoin_pred, tA_full_indval$acetoin_gperL),
            nCalSamples = dim(tA_full_train)[1],
            ncomp = TA_full_PC$Acetoin),
TA_nironefeasibility = c("acetoin",
                          caret::RMSE(tA_nironefeasibility_train$acetoin_cal, tA_nironefeasibility_train$acetoin_gperL),
                          caret::RMSE(tA_nironefeasibility_train$acetoin_cv, tA_nironefeasibility_train$acetoin_gperL),
                          caret::RMSE(tA_nironefeasibility_test$acetoin_pred, tA_nironefeasibility_test$acetoin_gperL),
                          caret::RMSE(tA_nironefeasibility_indval$acetoin_pred, tA_nironefeasibility_indval$acetoin_gperL),
                          caret::R2(tA_nironefeasibility_train$acetoin_cal, tA_nironefeasibility_train$acetoin_gperL),
                          caret::R2(tA_nironefeasibility_train$acetoin_cv, tA_nironefeasibility_train$acetoin_gperL),
                          caret::R2(tA_nironefeasibility_test$acetoin_pred, tA_nironefeasibility_test$acetoin_gperL),
                          caret::R2(tA_nironefeasibility_indval$acetoin_pred, tA_nironefeasibility_indval$acetoin_gperL),
                         nCalSamples = dim(tA_nironefeasibility_train)[1],
                         ncomp=TA_nironefeasibility_PCs$Acetoin),
nirone = c("acetoin",
           caret::RMSE(nirone_train$acetoin_cal, nirone_train$acetoin_gperL),
           caret::RMSE(nirone_train$acetoin_cv, nirone_train$acetoin_gperL),
           caret::RMSE(nirone_test$acetoin_pred, nirone_test$acetoin_gperL),
           caret::RMSE(nirone_indval$acetoin_pred, nirone_indval$acetoin_gperL),
           caret::R2(nirone_train$acetoin_cal, nirone_train$acetoin_gperL),
           caret::R2(nirone_train$acetoin_cv, nirone_train$acetoin_gperL),
           caret::R2(nirone_test$acetoin_pred, nirone_test$acetoin_gperL),
           caret::R2(nirone_indval$acetoin_pred, nirone_indval$acetoin_gperL),
           nCalSamples = dim(nirone_train)[1],
           ncomp = nirone_PCs$Acetoin),
TA_full = c("glycerol",
             caret::RMSE(tA_full_train$glycerol_cal, tA_full_train$glycerol_gperL),
             caret::RMSE(tA_full_train$glycerol_cv, tA_full_train$glycerol_gperL),
             caret::RMSE(tA_full_test$glycerol_pred, tA_full_test$glycerol_gperL),
             caret::RMSE(tA_full_indval$glycerol_pred, tA_full_indval$glycerol_gperL),
             caret::R2(tA_full_train$glycerol_cal, tA_full_train$glycerol_gperL),
             caret::R2(tA_full_train$glycerol_cv, tA_full_train$glycerol_gperL),
             caret::R2(tA_full_test$glycerol_pred, tA_full_test$glycerol_gperL),
             caret::R2(tA_full_indval$glycerol_pred, tA_full_indval$glycerol_gperL),
            nCalSamples = dim(tA_full_train)[1],
            ncomp = TA_full_PC$Glycerol),
TA_nironefeasibility = c("glycerol",
                          caret::RMSE(tA_nironefeasibility_train$glycerol_cal, tA_nironefeasibility_train$glycerol_gperL),
                          caret::RMSE(tA_nironefeasibility_train$glycerol_cv, tA_nironefeasibility_train$glycerol_gperL),
                          caret::RMSE(tA_nironefeasibility_test$glycerol_pred, tA_nironefeasibility_test$glycerol_gperL),
                          caret::RMSE(tA_nironefeasibility_indval$glycerol_pred, tA_nironefeasibility_indval$glycerol_gperL),
                          caret::R2(tA_nironefeasibility_train$glycerol_cal, tA_nironefeasibility_train$glycerol_gperL),
                          caret::R2(tA_nironefeasibility_train$glycerol_cv, tA_nironefeasibility_train$glycerol_gperL),
                          caret::R2(tA_nironefeasibility_test$glycerol_pred, tA_nironefeasibility_test$glycerol_gperL),
                          caret::R2(tA_nironefeasibility_indval$glycerol_pred, tA_nironefeasibility_indval$glycerol_gperL),
                          nCalSamples = dim(tA_nironefeasibility_train)[1],
                         ncomp = TA_nironefeasibility_PCs$Glycerol),
nirone = c("glycerol",
           caret::RMSE(nirone_train$glycerol_cal, nirone_train$glycerol_gperL),
           caret::RMSE(nirone_train$glycerol_cv, nirone_train$glycerol_gperL),
           caret::RMSE(nirone_test$glycerol_pred, nirone_test$glycerol_gperL),
           caret::RMSE(nirone_indval$glycerol_pred, nirone_indval$glycerol_gperL),
           caret::R2(nirone_train$glycerol_cal, nirone_train$glycerol_gperL),
           caret::R2(nirone_train$glycerol_cv, nirone_train$glycerol_gperL),
           caret::R2(nirone_test$glycerol_pred, nirone_test$glycerol_gperL),
           caret::R2(nirone_indval$glycerol_pred, nirone_indval$glycerol_gperL),
           nCalSamples = dim(nirone_train)[1],
           ncomp = nirone_PCs$Glycerol)
) %>% 
  write.csv("../tables/tableS3.csv")
     