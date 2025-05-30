# table S6- table displaying on line modeling results

#load packages

library(tidyverse)
library(caret)

# load data frames

tA_probe_train <- readRDS("../data/processed/dataframes/laboratoryatline_probefeasibility_training.RDS")
  
tA_probe_test <- readRDS("../data/processed/dataframes/laboratoryatline_probefeasibility_testing.RDS")
  
tA_probe_indval <- readRDS("../data/processed/dataframes/laboratoryatline_probefeasibility_indval.RDS")
  

tB__train <- readRDS("../data/processed/dataframes/laboratoryonline_probe_training.RDS")

tB__test <- readRDS("../data/processed/dataframes/laboratoryonline_probe_testing.RDS")

tB__indval <- readRDS("../data/processed/dataframes/laboratoryonline_probe_indval.RDS")

# collect and calculate performance statistics for each dataset for each constituent, saving results as a csv

data.frame(parameters = c("Constituent","RMSEC","RMSECV","RMSEP","RMSEP-INDEXP",
                          "R2C","R2CV","R2P","R2P-INDEXP"),
           TA_probe = c("Glucose",
                    caret::RMSE(tA_probe_train$glucose_cal, tA_probe_train$glucose_gperL),
                    caret::RMSE(tA_probe_train$glucose_cv, tA_probe_train$glucose_gperL),
                    caret::RMSE(tA_probe_test$glucose_pred, tA_probe_test$glucose_gperL),
                    caret::RMSE(tA_probe_indval$glucose_pred, tA_probe_indval$glucose_gperL),
                    caret::R2(tA_probe_train$glucose_cal, tA_probe_train$glucose_gperL),
                    caret::R2(tA_probe_train$glucose_cv, tA_probe_train$glucose_gperL),
                    caret::R2(tA_probe_test$glucose_pred, tA_probe_test$glucose_gperL),
                    caret::R2(tA_probe_indval$glucose_pred, tA_probe_indval$glucose_gperL)),
           TB_probe = c("Glucose",
                      caret::RMSE(tB__train$glucose_cal, tB__train$glucose_gperL),
                      caret::RMSE(tB__train$glucose_cv, tB__train$glucose_gperL),
                      caret::RMSE(tB__test$glucose_pred, tB__test$glucose_gperL),
                      caret::RMSE(tB__indval$glucose_pred, tB__indval$glucose_gperL),
                      caret::R2(tB__train$glucose_cal, tB__train$glucose_gperL),
                      caret::R2(tB__train$glucose_cv, tB__train$glucose_gperL),
                      caret::R2(tB__test$glucose_pred, tB__test$glucose_gperL),
                      caret::R2(tB__indval$glucose_pred, tB__indval$glucose_gperL)),
           TA_probe = c("xylose",
                                     caret::RMSE(tA_probe_train$xylose_cal, tA_probe_train$xylose_gperL),
                                     caret::RMSE(tA_probe_train$xylose_cv, tA_probe_train$xylose_gperL),
                                     caret::RMSE(tA_probe_test$xylose_pred, tA_probe_test$xylose_gperL),
                                     caret::RMSE(tA_probe_indval$xylose_pred, tA_probe_indval$xylose_gperL),
                                     caret::R2(tA_probe_train$xylose_cal, tA_probe_train$xylose_gperL),
                                     caret::R2(tA_probe_train$xylose_cv, tA_probe_train$xylose_gperL),
                                     caret::R2(tA_probe_test$xylose_pred, tA_probe_test$xylose_gperL),
                                     caret::R2(tA_probe_indval$xylose_pred, tA_probe_indval$xylose_gperL)),
           TB_probe = c("xylose",
                      caret::RMSE(tB__train$xylose_cal, tB__train$xylose_gperL),
                      caret::RMSE(tB__train$xylose_cv, tB__train$xylose_gperL),
                      caret::RMSE(tB__test$xylose_pred, tB__test$xylose_gperL),
                      caret::RMSE(tB__indval$xylose_pred, tB__indval$xylose_gperL),
                      caret::R2(tB__train$xylose_cal, tB__train$xylose_gperL),
                      caret::R2(tB__train$xylose_cv, tB__train$xylose_gperL),
                      caret::R2(tB__test$xylose_pred, tB__test$xylose_gperL),
                      caret::R2(tB__indval$xylose_pred, tB__indval$xylose_gperL)),
           TA_probe = c("bdo",
                          caret::RMSE(tA_probe_train$bdo_cal, tA_probe_train$totalBDO_gperL),
                          caret::RMSE(tA_probe_train$bdo_cv, tA_probe_train$totalBDO_gperL),
                          caret::RMSE(tA_probe_test$bdo_pred, tA_probe_test$totalBDO_gperL),
                          caret::RMSE(tA_probe_indval$bdo_pred, tA_probe_indval$totalBDO_gperL),
                          caret::R2(tA_probe_train$bdo_cal, tA_probe_train$totalBDO_gperL),
                          caret::R2(tA_probe_train$bdo_cv, tA_probe_train$totalBDO_gperL),
                          caret::R2(tA_probe_test$bdo_pred, tA_probe_test$totalBDO_gperL),
                          caret::R2(tA_probe_indval$bdo_pred, tA_probe_indval$totalBDO_gperL)),
TB_probe = c("bdo",
           caret::RMSE(tB__train$bdo_cal, tB__train$totalBDO_gperL),
           caret::RMSE(tB__train$bdo_cv, tB__train$totalBDO_gperL),
           caret::RMSE(tB__test$bdo_pred, tB__test$totalBDO_gperL),
           caret::RMSE(tB__indval$bdo_pred, tB__indval$totalBDO_gperL),
           caret::R2(tB__train$bdo_cal, tB__train$totalBDO_gperL),
           caret::R2(tB__train$bdo_cv, tB__train$totalBDO_gperL),
           caret::R2(tB__test$bdo_pred, tB__test$totalBDO_gperL),
           caret::R2(tB__indval$bdo_pred, tB__indval$totalBDO_gperL)),
TA_probe = c("acetoin",
                          caret::RMSE(tA_probe_train$acetoin_cal, tA_probe_train$acetoin_gperL),
                          caret::RMSE(tA_probe_train$acetoin_cv, tA_probe_train$acetoin_gperL),
                          caret::RMSE(tA_probe_test$acetoin_pred, tA_probe_test$acetoin_gperL),
                          caret::RMSE(tA_probe_indval$acetoin_pred, tA_probe_indval$acetoin_gperL),
                          caret::R2(tA_probe_train$acetoin_cal, tA_probe_train$acetoin_gperL),
                          caret::R2(tA_probe_train$acetoin_cv, tA_probe_train$acetoin_gperL),
                          caret::R2(tA_probe_test$acetoin_pred, tA_probe_test$acetoin_gperL),
                          caret::R2(tA_probe_indval$acetoin_pred, tA_probe_indval$acetoin_gperL)),
TB_probe = c("acetoin",
           caret::RMSE(tB__train$acetoin_cal, tB__train$acetoin_gperL),
           caret::RMSE(tB__train$acetoin_cv, tB__train$acetoin_gperL),
           caret::RMSE(tB__test$acetoin_pred, tB__test$acetoin_gperL),
           caret::RMSE(tB__indval$acetoin_pred, tB__indval$acetoin_gperL),
           caret::R2(tB__train$acetoin_cal, tB__train$acetoin_gperL),
           caret::R2(tB__train$acetoin_cv, tB__train$acetoin_gperL),
           caret::R2(tB__test$acetoin_pred, tB__test$acetoin_gperL),
           caret::R2(tB__indval$acetoin_pred, tB__indval$acetoin_gperL)),
TA_probe = c("glycerol",
                          caret::RMSE(tA_probe_train$glycerol_cal, tA_probe_train$glycerol_gperL),
                          caret::RMSE(tA_probe_train$glycerol_cv, tA_probe_train$glycerol_gperL),
                          caret::RMSE(tA_probe_test$glycerol_pred, tA_probe_test$glycerol_gperL),
                          caret::RMSE(tA_probe_indval$glycerol_pred, tA_probe_indval$glycerol_gperL),
                          caret::R2(tA_probe_train$glycerol_cal, tA_probe_train$glycerol_gperL),
                          caret::R2(tA_probe_train$glycerol_cv, tA_probe_train$glycerol_gperL),
                          caret::R2(tA_probe_test$glycerol_pred, tA_probe_test$glycerol_gperL),
                          caret::R2(tA_probe_indval$glycerol_pred, tA_probe_indval$glycerol_gperL)),
TB_probe = c("glycerol",
           caret::RMSE(tB__train$glycerol_cal, tB__train$glycerol_gperL),
           caret::RMSE(tB__train$glycerol_cv, tB__train$glycerol_gperL),
           caret::RMSE(tB__test$glycerol_pred, tB__test$glycerol_gperL),
           caret::RMSE(tB__indval$glycerol_pred, tB__indval$glycerol_gperL),
           caret::R2(tB__train$glycerol_cal, tB__train$glycerol_gperL),
           caret::R2(tB__train$glycerol_cv, tB__train$glycerol_gperL),
           caret::R2(tB__test$glycerol_pred, tB__test$glycerol_gperL),
           caret::R2(tB__indval$glycerol_pred, tB__indval$glycerol_gperL))
) %>% 
  write.csv("../tables/tablesS4.csv")
                    
                    

