# table s6- table displaying in line modeling results with normalization

# load packages

library(tidyverse)
library(caret)

# load data frames

tA_probe_train <- readRDS("../data/processed/dataframes/laboratoryatline_probefeasibility_training.RDS")
  
tA_probe_test <- readRDS("../data/processed/dataframes/laboratoryatline_probefeasibility_testing.RDS")
  
tA_probe_indval <- readRDS("../data/processed/dataframes/laboratoryatline_probefeasibility_indval.RDS")

tA_PCs <- readRDS("../data/processed/dataframes/laboratoryatline_probefeasibility_PCs.RDS")

tB_train <- readRDS("../data/processed/dataframes/laboratoryonline_probe_training.RDS")

tB_test <- readRDS("../data/processed/dataframes/laboratoryonline_probe_testing.RDS")

tB_indval <- readRDS("../data/processed/dataframes/laboratoryonline_probe_indval.RDS")

tB_PCs <- readRDS("../data/processed/dataframes/laboratoryonline_probe_PCs.RDS")

# collect laboratory grade on line and at line with on line range model performance statistics for each constituent, normalizing each performance 
# metric by the range of the constituent observed in the calibration dataset
# save results as csv file

data.frame(parameters = c("Constituent","RMSEC","RMSECV","RMSEP","RMSEP-INDEXP",
                          "R2C","R2CV","R2P","R2P-INDEXP", "ncalsamples", "Npcs"),
           TA_probe = c("Glucose",
                    caret::RMSE(tA_probe_train$glucose_cal, tA_probe_train$glucose_gperL)/
                      (range(tA_probe_train$glucose_gperL)[2]-range(tA_probe_train$glucose_gperL)[1]),
                    caret::RMSE(tA_probe_train$glucose_cv, tA_probe_train$glucose_gperL)/
                      (range(tA_probe_train$glucose_gperL)[2]-range(tA_probe_train$glucose_gperL)[1]),
                    caret::RMSE(tA_probe_test$glucose_pred, tA_probe_test$glucose_gperL)/
                      (range(tA_probe_train$glucose_gperL)[2]-range(tA_probe_train$glucose_gperL)[1]),
                    caret::RMSE(tA_probe_indval$glucose_pred, tA_probe_indval$glucose_gperL)/
                      (range(tA_probe_train$glucose_gperL)[2]-range(tA_probe_train$glucose_gperL)[1]),
                    caret::R2(tA_probe_train$glucose_cal, tA_probe_train$glucose_gperL),
                    caret::R2(tA_probe_train$glucose_cv, tA_probe_train$glucose_gperL),
                    caret::R2(tA_probe_test$glucose_pred, tA_probe_test$glucose_gperL),
                    caret::R2(tA_probe_indval$glucose_pred, tA_probe_indval$glucose_gperL),
                    nCalSamples = dim(tA_probe_train)[1],
                    ncomp = tA_PCs$Glucose ),
           TB_probe = c("Glucose",
                      caret::RMSE(tB_train$glucose_cal, tB_train$glucose_gperL)/
                        (range(tB_train$glucose_gperL)[2]-range(tB_train$glucose_gperL)[1]),
                      caret::RMSE(tB_train$glucose_cv, tB_train$glucose_gperL)/
                        (range(tB_train$glucose_gperL)[2]-range(tB_train$glucose_gperL)[1]),
                      caret::RMSE(tB_test$glucose_pred, tB_test$glucose_gperL)/
                        (range(tB_train$glucose_gperL)[2]-range(tB_train$glucose_gperL)[1]),
                      caret::RMSE(tB_indval$glucose_pred, tB_indval$glucose_gperL)/
                        (range(tB_train$glucose_gperL)[2]-range(tB_train$glucose_gperL)[1]),
                      caret::R2(tB_train$glucose_cal, tB_train$glucose_gperL),
                      caret::R2(tB_train$glucose_cv, tB_train$glucose_gperL),
                      caret::R2(tB_test$glucose_pred, tB_test$glucose_gperL),
                      caret::R2(tB_indval$glucose_pred, tB_indval$glucose_gperL),
                      nCalSamples = dim(tB_train)[1],
                      ncomp = tB_PCs$Glucose ),
           TA_probe = c("xylose",
                                     caret::RMSE(tA_probe_train$xylose_cal, tA_probe_train$xylose_gperL)/
                          (range(tA_probe_train$xylose_gperL)[2]-range(tA_probe_train$xylose_gperL)[1]),
                                     caret::RMSE(tA_probe_train$xylose_cv, tA_probe_train$xylose_gperL)/
                          (range(tA_probe_train$xylose_gperL)[2]-range(tA_probe_train$xylose_gperL)[1]),
                                     caret::RMSE(tA_probe_test$xylose_pred, tA_probe_test$xylose_gperL)/
                          (range(tA_probe_train$xylose_gperL)[2]-range(tA_probe_train$xylose_gperL)[1]),
                                     caret::RMSE(tA_probe_indval$xylose_pred, tA_probe_indval$xylose_gperL)/
                          (range(tA_probe_train$xylose_gperL)[2]-range(tA_probe_train$xylose_gperL)[1]),
                                     caret::R2(tA_probe_train$xylose_cal, tA_probe_train$xylose_gperL),
                                     caret::R2(tA_probe_train$xylose_cv, tA_probe_train$xylose_gperL),
                                     caret::R2(tA_probe_test$xylose_pred, tA_probe_test$xylose_gperL),
                                     caret::R2(tA_probe_indval$xylose_pred, tA_probe_indval$xylose_gperL),
                        nCalSamples = dim(tA_probe_train)[1],
                        ncomp = tA_PCs$Xylose ),
           TB_probe = c("xylose",
                      caret::RMSE(tB_train$xylose_cal, tB_train$xylose_gperL)/
                        (range(tB_train$xylose_gperL)[2]-range(tB_train$xylose_gperL)[1]),
                      caret::RMSE(tB_train$xylose_cv, tB_train$xylose_gperL)/
                        (range(tB_train$xylose_gperL)[2]-range(tB_train$xylose_gperL)[1]),
                      caret::RMSE(tB_test$xylose_pred, tB_test$xylose_gperL)/
                        (range(tB_train$xylose_gperL)[2]-range(tB_train$xylose_gperL)[1]),
                      caret::RMSE(tB_indval$xylose_pred, tB_indval$xylose_gperL)/
                        (range(tB_train$xylose_gperL)[2]-range(tB_train$xylose_gperL)[1]),
                      caret::R2(tB_train$xylose_cal, tB_train$xylose_gperL),
                      caret::R2(tB_train$xylose_cv, tB_train$xylose_gperL),
                      caret::R2(tB_test$xylose_pred, tB_test$xylose_gperL),
                      caret::R2(tB_indval$xylose_pred, tB_indval$xylose_gperL),
                      nCalSamples = dim(tB_train)[1],
                      ncomp = tB_PCs$Xylose ),
           TA_probe = c("bdo",
                          caret::RMSE(tA_probe_train$bdo_cal, tA_probe_train$totalBDO_gperL)/
                          (range(tA_probe_train$totalBDO_gperL)[2]-range(tA_probe_train$totalBDO_gperL)[1]),
                          caret::RMSE(tA_probe_train$bdo_cv, tA_probe_train$totalBDO_gperL)/
                          (range(tA_probe_train$totalBDO_gperL)[2]-range(tA_probe_train$totalBDO_gperL)[1]),
                          caret::RMSE(tA_probe_test$bdo_pred, tA_probe_test$totalBDO_gperL)/
                          (range(tA_probe_train$totalBDO_gperL)[2]-range(tA_probe_train$totalBDO_gperL)[1]),
                          caret::RMSE(tA_probe_indval$bdo_pred, tA_probe_indval$totalBDO_gperL)/
                          (range(tA_probe_train$totalBDO_gperL)[2]-range(tA_probe_train$totalBDO_gperL)[1]),
                          caret::R2(tA_probe_train$bdo_cal, tA_probe_train$totalBDO_gperL),
                          caret::R2(tA_probe_train$bdo_cv, tA_probe_train$totalBDO_gperL),
                          caret::R2(tA_probe_test$bdo_pred, tA_probe_test$totalBDO_gperL),
                          caret::R2(tA_probe_indval$bdo_pred, tA_probe_indval$totalBDO_gperL),
                        nCalSamples = dim(tA_probe_train)[1],
                        ncomp = tA_PCs$BDO ),
TB_probe = c("bdo",
           caret::RMSE(tB_train$bdo_cal, tB_train$totalBDO_gperL)/
             (range(tB_train$totalBDO_gperL)[2]-range(tB_train$totalBDO_gperL)[1]),
           caret::RMSE(tB_train$bdo_cv, tB_train$totalBDO_gperL)/
             (range(tB_train$totalBDO_gperL)[2]-range(tB_train$totalBDO_gperL)[1]),
           caret::RMSE(tB_test$bdo_pred, tB_test$totalBDO_gperL)/
             (range(tB_train$totalBDO_gperL)[2]-range(tB_train$totalBDO_gperL)[1]),
           caret::RMSE(tB_indval$bdo_pred, tB_indval$totalBDO_gperL)/
             (range(tB_train$totalBDO_gperL)[2]-range(tB_train$totalBDO_gperL)[1]),
           caret::R2(tB_train$bdo_cal, tB_train$totalBDO_gperL),
           caret::R2(tB_train$bdo_cv, tB_train$totalBDO_gperL),
           caret::R2(tB_test$bdo_pred, tB_test$totalBDO_gperL),
           caret::R2(tB_indval$bdo_pred, tB_indval$totalBDO_gperL),
           nCalSamples = dim(tB_train)[1],
           ncomp = tB_PCs$BDO ),
TA_probe = c("acetoin",
                          caret::RMSE(tA_probe_train$acetoin_cal, tA_probe_train$acetoin_gperL)/
               (range(tA_probe_train$acetoin_gperL)[2]-range(tA_probe_train$acetoin_gperL)[1]),
                          caret::RMSE(tA_probe_train$acetoin_cv, tA_probe_train$acetoin_gperL)/
               (range(tA_probe_train$acetoin_gperL)[2]-range(tA_probe_train$acetoin_gperL)[1]),
                          caret::RMSE(tA_probe_test$acetoin_pred, tA_probe_test$acetoin_gperL)/
               (range(tA_probe_train$acetoin_gperL)[2]-range(tA_probe_train$acetoin_gperL)[1]),
                          caret::RMSE(tA_probe_indval$acetoin_pred, tA_probe_indval$acetoin_gperL)/
               (range(tA_probe_train$acetoin_gperL)[2]-range(tA_probe_train$acetoin_gperL)[1]),
                          caret::R2(tA_probe_train$acetoin_cal, tA_probe_train$acetoin_gperL),
                          caret::R2(tA_probe_train$acetoin_cv, tA_probe_train$acetoin_gperL),
                          caret::R2(tA_probe_test$acetoin_pred, tA_probe_test$acetoin_gperL),
                          caret::R2(tA_probe_indval$acetoin_pred, tA_probe_indval$acetoin_gperL),
             nCalSamples = dim(tA_probe_train)[1],
             ncomp = tA_PCs$Acetoin ),
TB_probe = c("acetoin",
           caret::RMSE(tB_train$acetoin_cal, tB_train$acetoin_gperL)/
             (range(tB_train$acetoin_gperL)[2]-range(tB_train$acetoin_gperL)[1]),
           caret::RMSE(tB_train$acetoin_cv, tB_train$acetoin_gperL)/
             (range(tB_train$acetoin_gperL)[2]-range(tB_train$acetoin_gperL)[1]),
           caret::RMSE(tB_test$acetoin_pred, tB_test$acetoin_gperL)/
             (range(tB_train$acetoin_gperL)[2]-range(tB_train$acetoin_gperL)[1]),
           caret::RMSE(tB_indval$acetoin_pred, tB_indval$acetoin_gperL)/
             (range(tB_train$acetoin_gperL)[2]-range(tB_train$acetoin_gperL)[1]),
           caret::R2(tB_train$acetoin_cal, tB_train$acetoin_gperL),
           caret::R2(tB_train$acetoin_cv, tB_train$acetoin_gperL),
           caret::R2(tB_test$acetoin_pred, tB_test$acetoin_gperL),
           caret::R2(tB_indval$acetoin_pred, tB_indval$acetoin_gperL),
           nCalSamples = dim(tB_train)[1],
           ncomp = tB_PCs$Acetoin ),
TA_probe = c("glycerol",
                          caret::RMSE(tA_probe_train$glycerol_cal, tA_probe_train$glycerol_gperL)/
               (range(tA_probe_train$glycerol_gperL)[2]-range(tA_probe_train$glycerol_gperL)[1]),
                          caret::RMSE(tA_probe_train$glycerol_cv, tA_probe_train$glycerol_gperL)/
               (range(tA_probe_train$glycerol_gperL)[2]-range(tA_probe_train$glycerol_gperL)[1]),
                          caret::RMSE(tA_probe_test$glycerol_pred, tA_probe_test$glycerol_gperL)/
               (range(tA_probe_train$glycerol_gperL)[2]-range(tA_probe_train$glycerol_gperL)[1]),
                          caret::RMSE(tA_probe_indval$glycerol_pred, tA_probe_indval$glycerol_gperL)/
               (range(tA_probe_train$glycerol_gperL)[2]-range(tA_probe_train$glycerol_gperL)[1]),
                          caret::R2(tA_probe_train$glycerol_cal, tA_probe_train$glycerol_gperL),
                          caret::R2(tA_probe_train$glycerol_cv, tA_probe_train$glycerol_gperL),
                          caret::R2(tA_probe_test$glycerol_pred, tA_probe_test$glycerol_gperL),
                          caret::R2(tA_probe_indval$glycerol_pred, tA_probe_indval$glycerol_gperL),
             nCalSamples = dim(tA_probe_train)[1],
             ncomp = tA_PCs$Glycerol ),
TB_probe = c("glycerol",
           caret::RMSE(tB_train$glycerol_cal, tB_train$glycerol_gperL)/
             (range(tB_train$glycerol_gperL)[2]-range(tB_train$glycerol_gperL)[1]),
           caret::RMSE(tB_train$glycerol_cv, tB_train$glycerol_gperL)/
             (range(tB_train$glycerol_gperL)[2]-range(tB_train$glycerol_gperL)[1]),
           caret::RMSE(tB_test$glycerol_pred, tB_test$glycerol_gperL)/
             (range(tB_train$glycerol_gperL)[2]-range(tB_train$glycerol_gperL)[1]),
           caret::RMSE(tB_indval$glycerol_pred, tB_indval$glycerol_gperL)/
             (range(tB_train$glycerol_gperL)[2]-range(tB_train$glycerol_gperL)[1]),
           caret::R2(tB_train$glycerol_cal, tB_train$glycerol_gperL),
           caret::R2(tB_train$glycerol_cv, tB_train$glycerol_gperL),
           caret::R2(tB_test$glycerol_pred, tB_test$glycerol_gperL),
           caret::R2(tB_indval$glycerol_pred, tB_indval$glycerol_gperL),
           nCalSamples = dim(tB_train)[1],
           ncomp = tB_PCs$Glycerol )
) %>% 
  write.csv("../tables/tablesS4_normalized.csv")
                    
                    

