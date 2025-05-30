# table s5- table displaying at line modeling results with normalization

#load packages

library(tidyverse)
library(caret)

# load data frames

laboratory_full_train <- readRDS("../data/processed/dataframes/laboratoryatline_full_training.RDS")

laboratory_full_test <- readRDS("../data/processed/dataframes/laboratoryatline_full_testing.RDS")
  
laboratory_full_indval <- readRDS("../data/processed/dataframes/laboratoryatline_full_indval.RDS")
  
laboratory_lowcostfeasibility_train <- readRDS("../data/processed/dataframes/laboratoryatline_lowcostfeasibility_training.RDS")
  
laboratory_lowcostfeasibility_test <- readRDS("../data/processed/dataframes/laboratoryatline_lowcostfeasibility_testing.RDS")
  
laboratory_lowcostfeasibility_indval <- readRDS("../data/processed/dataframes/laboratoryatline_lowcostfeasibility_indval.RDS")
  
lowcost_train <- readRDS("../data/processed/dataframes/lowcostatline_training.RDS")

lowcost_test <- readRDS("../data/processed/dataframes/lowcostatline_testing.RDS")

lowcost_indval <- readRDS("../data/processed/dataframes/lowcostatline_indval.RDS")

laboratory_full_PC <- readRDS("../data/processed/dataframes/laboratoryatline_full_PCs.RDS")

laboratory_lowcostfeasibility_PCs <-  readRDS("../data/processed/dataframes/laboratoryatline_lowcostfeasibility_PCs.RDS")

lowcost_PCs <- readRDS("../data/processed/dataframes/lowcostatline_PCs.RDS")

# collect laboratory grade and low cost grade at line model performance statistics for each constituent, normalizing each performance 
# metric by the range of the constituent observed in the calibration dataset
# save results as csv file

a <- data.frame(parameters = c("Constituent","RMSEC","RMSECV","RMSEP","RMSEP-INDEXP",
                          "R2C","R2CV","R2P","R2P-INDEXP", "ncalsamples", "ncomp"),
           laboratory_full = c("Glucose",
                    caret::RMSE(laboratory_full_train$glucose_cal, laboratory_full_train$glucose_gperL)/
                      (range(laboratory_full_train$glucose_gperL)[2]-range(laboratory_full_train$glucose_gperL)[1]),
                    caret::RMSE(laboratory_full_train$glucose_cv, laboratory_full_train$glucose_gperL)/
                      (range(laboratory_full_train$glucose_gperL)[2]-range(laboratory_full_train$glucose_gperL)[1]),
                    caret::RMSE(laboratory_full_test$glucose_pred, laboratory_full_test$glucose_gperL)/
                      (range(laboratory_full_train$glucose_gperL)[2]-range(laboratory_full_train$glucose_gperL)[1]),
                    caret::RMSE(laboratory_full_indval$glucose_pred, laboratory_full_indval$glucose_gperL)/
                      (range(laboratory_full_train$glucose_gperL)[2]-range(laboratory_full_train$glucose_gperL)[1]),
                    caret::R2(laboratory_full_train$glucose_cal, laboratory_full_train$glucose_gperL),
                    caret::R2(laboratory_full_train$glucose_cv, laboratory_full_train$glucose_gperL),
                    caret::R2(laboratory_full_test$glucose_pred, laboratory_full_test$glucose_gperL),
                    caret::R2(laboratory_full_indval$glucose_pred, laboratory_full_indval$glucose_gperL),
                    nCalSamples = dim(laboratory_full_train)[1],
                    ncomp = laboratory_full_PC$Glucose ),
           laboratory_lowcostfeasibility = c("Glucose",
                    caret::RMSE(laboratory_lowcostfeasibility_train$glucose_cal, laboratory_lowcostfeasibility_train$glucose_gperL)/
                      (range(laboratory_lowcostfeasibility_train$glucose_gperL)[2]-range(laboratory_lowcostfeasibility_train$glucose_gperL)[1]),
                    caret::RMSE(laboratory_lowcostfeasibility_train$glucose_cv, laboratory_lowcostfeasibility_train$glucose_gperL)/
                      (range(laboratory_lowcostfeasibility_train$glucose_gperL)[2]-range(laboratory_lowcostfeasibility_train$glucose_gperL)[1]),
                    caret::RMSE(laboratory_lowcostfeasibility_test$glucose_pred, laboratory_lowcostfeasibility_test$glucose_gperL)/
                      (range(laboratory_lowcostfeasibility_train$glucose_gperL)[2]-range(laboratory_lowcostfeasibility_train$glucose_gperL)[1]),
                    caret::RMSE(laboratory_lowcostfeasibility_indval$glucose_pred, laboratory_lowcostfeasibility_indval$glucose_gperL)/
                      (range(laboratory_lowcostfeasibility_train$glucose_gperL)[2]-range(laboratory_lowcostfeasibility_train$glucose_gperL)[1]),
                    caret::R2(laboratory_lowcostfeasibility_train$glucose_cal, laboratory_lowcostfeasibility_train$glucose_gperL),
                    caret::R2(laboratory_lowcostfeasibility_train$glucose_cv, laboratory_lowcostfeasibility_train$glucose_gperL),
                    caret::R2(laboratory_lowcostfeasibility_test$glucose_pred, laboratory_lowcostfeasibility_test$glucose_gperL),
                    caret::R2(laboratory_lowcostfeasibility_indval$glucose_pred, laboratory_lowcostfeasibility_indval$glucose_gperL),
                    nCalSamples = dim(laboratory_lowcostfeasibility_train)[1],
                    ncomp = laboratory_lowcostfeasibility_PCs$Glucose),
           lowcost = c("Glucose",
                      caret::RMSE(lowcost_train$glucose_cal, lowcost_train$glucose_gperL)/
                        (range(lowcost_train$glucose_gperL)[2]-range(lowcost_train$glucose_gperL)[1]),
                      caret::RMSE(lowcost_train$glucose_cv, lowcost_train$glucose_gperL)/
                        (range(lowcost_train$glucose_gperL)[2]-range(lowcost_train$glucose_gperL)[1]),
                      caret::RMSE(lowcost_test$glucose_pred, lowcost_test$glucose_gperL)/
                        (range(lowcost_train$glucose_gperL)[2]-range(lowcost_train$glucose_gperL)[1]),
                      caret::RMSE(lowcost_indval$glucose_pred, lowcost_indval$glucose_gperL)/
                        (range(lowcost_train$glucose_gperL)[2]-range(lowcost_train$glucose_gperL)[1]),
                      caret::R2(lowcost_train$glucose_cal, lowcost_train$glucose_gperL),
                      caret::R2(lowcost_train$glucose_cv, lowcost_train$glucose_gperL),
                      caret::R2(lowcost_test$glucose_pred, lowcost_test$glucose_gperL),
                      caret::R2(lowcost_indval$glucose_pred, lowcost_indval$glucose_gperL),
                      nCalSamples = dim(lowcost_train)[1],
                      ncomp = lowcost_PCs$Glucose),
           laboratory_full = c("xylose",
                        caret::RMSE(laboratory_full_train$xylose_cal, laboratory_full_train$xylose_gperL)/
                         (range(laboratory_full_train$xylose_gperL)[2]-range(laboratory_full_train$xylose_gperL)[1]),
                        caret::RMSE(laboratory_full_train$xylose_cv, laboratory_full_train$xylose_gperL)/
                         (range(laboratory_full_train$xylose_gperL)[2]-range(laboratory_full_train$xylose_gperL)[1]),
                        caret::RMSE(laboratory_full_test$xylose_pred, laboratory_full_test$xylose_gperL)/
                         (range(laboratory_full_train$xylose_gperL)[2]-range(laboratory_full_train$xylose_gperL)[1]),
                        caret::RMSE(laboratory_full_indval$xylose_pred, laboratory_full_indval$xylose_gperL)/
                         (range(laboratory_full_train$xylose_gperL)[2]-range(laboratory_full_train$xylose_gperL)[1]),
                        caret::R2(laboratory_full_train$xylose_cal, laboratory_full_train$xylose_gperL),
                        caret::R2(laboratory_full_train$xylose_cv, laboratory_full_train$xylose_gperL),
                        caret::R2(laboratory_full_test$xylose_pred, laboratory_full_test$xylose_gperL),
                        caret::R2(laboratory_full_indval$xylose_pred, laboratory_full_indval$xylose_gperL),
                       nCalSamples = dim(laboratory_full_train)[1],
                       ncomp = laboratory_full_PC$Xylose),
           laboratory_lowcostfeasibility = c("xylose",
                                     caret::RMSE(laboratory_lowcostfeasibility_train$xylose_cal, laboratory_lowcostfeasibility_train$xylose_gperL)/
                                      (range(laboratory_lowcostfeasibility_train$xylose_gperL)[2]-range(laboratory_lowcostfeasibility_train$xylose_gperL)[1]),
                                     caret::RMSE(laboratory_lowcostfeasibility_train$xylose_cv, laboratory_lowcostfeasibility_train$xylose_gperL)/
                                      (range(laboratory_lowcostfeasibility_train$xylose_gperL)[2]-range(laboratory_lowcostfeasibility_train$xylose_gperL)[1]),
                                     caret::RMSE(laboratory_lowcostfeasibility_test$xylose_pred, laboratory_lowcostfeasibility_test$xylose_gperL)/
                                      (range(laboratory_lowcostfeasibility_test$xylose_gperL)[2]-range(laboratory_lowcostfeasibility_test$xylose_gperL)[1]),
                                     caret::RMSE(laboratory_lowcostfeasibility_indval$xylose_pred, laboratory_lowcostfeasibility_indval$xylose_gperL)/
                                      (range(laboratory_lowcostfeasibility_train$xylose_gperL)[2]-range(laboratory_lowcostfeasibility_train$xylose_gperL)[1]),
                                     caret::R2(laboratory_lowcostfeasibility_train$xylose_cal, laboratory_lowcostfeasibility_train$xylose_gperL),
                                     caret::R2(laboratory_lowcostfeasibility_train$xylose_cv, laboratory_lowcostfeasibility_train$xylose_gperL),
                                     caret::R2(laboratory_lowcostfeasibility_test$xylose_pred, laboratory_lowcostfeasibility_test$xylose_gperL),
                                     caret::R2(laboratory_lowcostfeasibility_indval$xylose_pred, laboratory_lowcostfeasibility_indval$xylose_gperL),
                                    nCalSamples = dim(laboratory_lowcostfeasibility_train)[1],
                                    ncomp = laboratory_lowcostfeasibility_PCs$Xylose),
           lowcost = c("xylose",
                      caret::RMSE(lowcost_train$xylose_cal, lowcost_train$xylose_gperL)/
                        (range(lowcost_train$xylose_gperL)[2]-range(lowcost_train$xylose_gperL)[1]),
                      caret::RMSE(lowcost_train$xylose_cv, lowcost_train$xylose_gperL)/
                        (range(lowcost_train$xylose_gperL)[2]-range(lowcost_train$xylose_gperL)[1]),
                      caret::RMSE(lowcost_test$xylose_pred, lowcost_test$xylose_gperL)/
                        (range(lowcost_train$xylose_gperL)[2]-range(lowcost_train$xylose_gperL)[1]),
                      caret::RMSE(lowcost_indval$xylose_pred, lowcost_indval$xylose_gperL)/
                        (range(lowcost_train$xylose_gperL)[2]-range(lowcost_train$xylose_gperL)[1]),
                      caret::R2(lowcost_train$xylose_cal, lowcost_train$xylose_gperL),
                      caret::R2(lowcost_train$xylose_cv, lowcost_train$xylose_gperL),
                      caret::R2(lowcost_test$xylose_pred, lowcost_test$xylose_gperL),
                      caret::R2(lowcost_indval$xylose_pred, lowcost_indval$xylose_gperL),
                      nCalSamples = dim(lowcost_train)[1],
                      ncomp = lowcost_PCs$Xylose),
laboratory_full = c("bdo",
             caret::RMSE(laboratory_full_train$bdo_cal, laboratory_full_train$totalBDO_gperL)/
              (range(laboratory_full_train$totalBDO_gperL)[2]-range(laboratory_full_train$totalBDO_gperL)[1]),
             caret::RMSE(laboratory_full_train$bdo_cv, laboratory_full_train$totalBDO_gperL)/
              (range(laboratory_full_train$totalBDO_gperL)[2]-range(laboratory_full_train$totalBDO_gperL)[1]),
             caret::RMSE(laboratory_full_test$bdo_pred, laboratory_full_test$totalBDO_gperL)/
              (range(laboratory_full_train$totalBDO_gperL)[2]-range(laboratory_full_train$totalBDO_gperL)[1]),
             caret::RMSE(laboratory_full_indval$bdo_pred, laboratory_full_indval$totalBDO_gperL)/
              (range(laboratory_full_train$totalBDO_gperL)[2]-range(laboratory_full_train$totalBDO_gperL)[1]),
             caret::R2(laboratory_full_train$bdo_cal, laboratory_full_train$totalBDO_gperL),
             caret::R2(laboratory_full_train$bdo_cv, laboratory_full_train$totalBDO_gperL),
             caret::R2(laboratory_full_test$bdo_pred, laboratory_full_test$totalBDO_gperL),
             caret::R2(laboratory_full_indval$bdo_pred, laboratory_full_indval$totalBDO_gperL),
            nCalSamples = dim(laboratory_full_train)[1],
            ncomp = laboratory_full_PC$BDO),
laboratory_lowcostfeasibility = c("bdo",
                          caret::RMSE(laboratory_lowcostfeasibility_train$bdo_cal, laboratory_lowcostfeasibility_train$totalBDO_gperL)/
                           (range(laboratory_lowcostfeasibility_train$totalBDO_gperL)[2]-range(laboratory_lowcostfeasibility_train$totalBDO_gperL)[1]),
                          caret::RMSE(laboratory_lowcostfeasibility_train$bdo_cv, laboratory_lowcostfeasibility_train$totalBDO_gperL)/
                           (range(laboratory_lowcostfeasibility_train$totalBDO_gperL)[2]-range(laboratory_lowcostfeasibility_train$totalBDO_gperL)[1]),
                          caret::RMSE(laboratory_lowcostfeasibility_test$bdo_pred, laboratory_lowcostfeasibility_test$totalBDO_gperL)/
                           (range(laboratory_lowcostfeasibility_train$totalBDO_gperL)[2]-range(laboratory_lowcostfeasibility_train$totalBDO_gperL)[1]),
                          caret::RMSE(laboratory_lowcostfeasibility_indval$bdo_pred, laboratory_lowcostfeasibility_indval$totalBDO_gperL)/
                           (range(laboratory_lowcostfeasibility_train$totalBDO_gperL)[2]-range(laboratory_lowcostfeasibility_train$totalBDO_gperL)[1]),
                          caret::R2(laboratory_lowcostfeasibility_train$bdo_cal, laboratory_lowcostfeasibility_train$totalBDO_gperL),
                          caret::R2(laboratory_lowcostfeasibility_train$bdo_cv, laboratory_lowcostfeasibility_train$totalBDO_gperL),
                          caret::R2(laboratory_lowcostfeasibility_test$bdo_pred, laboratory_lowcostfeasibility_test$totalBDO_gperL),
                          caret::R2(laboratory_lowcostfeasibility_indval$bdo_pred, laboratory_lowcostfeasibility_indval$totalBDO_gperL),
                         nCalSamples = dim(laboratory_lowcostfeasibility_train)[1],
                         ncomp = laboratory_lowcostfeasibility_PCs$BDO),
lowcost = c("bdo",
           caret::RMSE(lowcost_train$bdo_cal, lowcost_train$totalBDO_gperL)/
             (range(lowcost_train$totalBDO_gperL)[2]-range(lowcost_train$totalBDO_gperL)[1]),
           caret::RMSE(lowcost_train$bdo_cv, lowcost_train$totalBDO_gperL)/
             (range(lowcost_train$totalBDO_gperL)[2]-range(lowcost_train$totalBDO_gperL)[1]),
           caret::RMSE(lowcost_test$bdo_pred, lowcost_test$totalBDO_gperL)/
             (range(lowcost_train$totalBDO_gperL)[2]-range(lowcost_train$totalBDO_gperL)[1]),
           caret::RMSE(lowcost_indval$bdo_pred, lowcost_indval$totalBDO_gperL)/
             (range(lowcost_train$totalBDO_gperL)[2]-range(lowcost_train$totalBDO_gperL)[1]),
           caret::R2(lowcost_train$bdo_cal, lowcost_train$totalBDO_gperL),
           caret::R2(lowcost_train$bdo_cv, lowcost_train$totalBDO_gperL),
           caret::R2(lowcost_test$bdo_pred, lowcost_test$totalBDO_gperL),
           caret::R2(lowcost_indval$bdo_pred, lowcost_indval$totalBDO_gperL),
           nCalSamples = dim(lowcost_train)[1],
           ncomp = lowcost_PCs$BDO),
laboratory_full = c("acetoin",
             caret::RMSE(laboratory_full_train$acetoin_cal, laboratory_full_train$acetoin_gperL)/
              (range(laboratory_full_train$acetoin_gperL)[2]-range(laboratory_full_train$acetoin_gperL)[1]),
             caret::RMSE(laboratory_full_train$acetoin_cv, laboratory_full_train$acetoin_gperL)/
              (range(laboratory_full_train$acetoin_gperL)[2]-range(laboratory_full_train$acetoin_gperL)[1]),
             caret::RMSE(laboratory_full_test$acetoin_pred, laboratory_full_test$acetoin_gperL)/
              (range(laboratory_full_train$acetoin_gperL)[2]-range(laboratory_full_train$acetoin_gperL)[1]),
             caret::RMSE(laboratory_full_indval$acetoin_pred, laboratory_full_indval$acetoin_gperL)/
              (range(laboratory_full_train$acetoin_gperL)[2]-range(laboratory_full_train$acetoin_gperL)[1]),
             caret::R2(laboratory_full_train$acetoin_cal, laboratory_full_train$acetoin_gperL),
             caret::R2(laboratory_full_train$acetoin_cv, laboratory_full_train$acetoin_gperL),
             caret::R2(laboratory_full_test$acetoin_pred, laboratory_full_test$acetoin_gperL),
             caret::R2(laboratory_full_indval$acetoin_pred, laboratory_full_indval$acetoin_gperL),
            nCalSamples = dim(laboratory_full_train)[1],
            ncomp = laboratory_full_PC$Acetoin),
laboratory_lowcostfeasibility = c("acetoin",
                          caret::RMSE(laboratory_lowcostfeasibility_train$acetoin_cal, laboratory_lowcostfeasibility_train$acetoin_gperL)/
                           (range(laboratory_lowcostfeasibility_train$acetoin_gperL)[2]-range(laboratory_lowcostfeasibility_train$acetoin_gperL)[1]),
                          caret::RMSE(laboratory_lowcostfeasibility_train$acetoin_cv, laboratory_lowcostfeasibility_train$acetoin_gperL)/
                           (range(laboratory_lowcostfeasibility_train$acetoin_gperL)[2]-range(laboratory_lowcostfeasibility_train$acetoin_gperL)[1]),
                          caret::RMSE(laboratory_lowcostfeasibility_test$acetoin_pred, laboratory_lowcostfeasibility_test$acetoin_gperL)/
                           (range(laboratory_lowcostfeasibility_train$acetoin_gperL)[2]-range(laboratory_lowcostfeasibility_train$acetoin_gperL)[1]),
                          caret::RMSE(laboratory_lowcostfeasibility_indval$acetoin_pred, laboratory_lowcostfeasibility_indval$acetoin_gperL)/
                           (range(laboratory_lowcostfeasibility_train$acetoin_gperL)[2]-range(laboratory_lowcostfeasibility_train$acetoin_gperL)[1]),
                          caret::R2(laboratory_lowcostfeasibility_train$acetoin_cal, laboratory_lowcostfeasibility_train$acetoin_gperL),
                          caret::R2(laboratory_lowcostfeasibility_train$acetoin_cv, laboratory_lowcostfeasibility_train$acetoin_gperL),
                          caret::R2(laboratory_lowcostfeasibility_test$acetoin_pred, laboratory_lowcostfeasibility_test$acetoin_gperL),
                          caret::R2(laboratory_lowcostfeasibility_indval$acetoin_pred, laboratory_lowcostfeasibility_indval$acetoin_gperL),
                         nCalSamples = dim(laboratory_lowcostfeasibility_train)[1],
                         ncomp=laboratory_lowcostfeasibility_PCs$Acetoin),
lowcost = c("acetoin",
           caret::RMSE(lowcost_train$acetoin_cal, lowcost_train$acetoin_gperL)/
             (range(lowcost_train$acetoin_gperL)[2]-range(lowcost_train$acetoin_gperL)[1]),
           caret::RMSE(lowcost_train$acetoin_cv, lowcost_train$acetoin_gperL)/
             (range(lowcost_train$acetoin_gperL)[2]-range(lowcost_train$acetoin_gperL)[1]),
           caret::RMSE(lowcost_test$acetoin_pred, lowcost_test$acetoin_gperL)/
             (range(lowcost_train$acetoin_gperL)[2]-range(lowcost_train$acetoin_gperL)[1]),
           caret::RMSE(lowcost_indval$acetoin_pred, lowcost_indval$acetoin_gperL)/
             (range(lowcost_train$acetoin_gperL)[2]-range(lowcost_train$acetoin_gperL)[1]),
           caret::R2(lowcost_train$acetoin_cal, lowcost_train$acetoin_gperL),
           caret::R2(lowcost_train$acetoin_cv, lowcost_train$acetoin_gperL),
           caret::R2(lowcost_test$acetoin_pred, lowcost_test$acetoin_gperL),
           caret::R2(lowcost_indval$acetoin_pred, lowcost_indval$acetoin_gperL),
           nCalSamples = dim(lowcost_train)[1],
           ncomp = lowcost_PCs$Acetoin),
laboratory_full = c("glycerol",
             caret::RMSE(laboratory_full_train$glycerol_cal, laboratory_full_train$glycerol_gperL)/
              (range(laboratory_full_train$glycerol_gperL)[2]-range(laboratory_full_train$glycerol_gperL)[1]),
             caret::RMSE(laboratory_full_train$glycerol_cv, laboratory_full_train$glycerol_gperL)/
              (range(laboratory_full_train$glycerol_gperL)[2]-range(laboratory_full_train$glycerol_gperL)[1]),
             caret::RMSE(laboratory_full_test$glycerol_pred, laboratory_full_test$glycerol_gperL)/
              (range(laboratory_full_train$glycerol_gperL)[2]-range(laboratory_full_train$glycerol_gperL)[1]),
             caret::RMSE(laboratory_full_indval$glycerol_pred, laboratory_full_indval$glycerol_gperL)/
              (range(laboratory_full_train$glycerol_gperL)[2]-range(laboratory_full_train$glycerol_gperL)[1]),
             caret::R2(laboratory_full_train$glycerol_cal, laboratory_full_train$glycerol_gperL),
             caret::R2(laboratory_full_train$glycerol_cv, laboratory_full_train$glycerol_gperL),
             caret::R2(laboratory_full_test$glycerol_pred, laboratory_full_test$glycerol_gperL),
             caret::R2(laboratory_full_indval$glycerol_pred, laboratory_full_indval$glycerol_gperL),
            nCalSamples = dim(laboratory_full_train)[1],
            ncomp = laboratory_full_PC$Glycerol),
laboratory_lowcostfeasibility = c("glycerol",
                          caret::RMSE(laboratory_lowcostfeasibility_train$glycerol_cal, laboratory_lowcostfeasibility_train$glycerol_gperL)/
                           (range(laboratory_lowcostfeasibility_train$glycerol_gperL)[2]-range(laboratory_lowcostfeasibility_train$glycerol_gperL)[1]),
                          caret::RMSE(laboratory_lowcostfeasibility_train$glycerol_cv, laboratory_lowcostfeasibility_train$glycerol_gperL)/
                           (range(laboratory_lowcostfeasibility_train$glycerol_gperL)[2]-range(laboratory_lowcostfeasibility_train$glycerol_gperL)[1]),
                          caret::RMSE(laboratory_lowcostfeasibility_test$glycerol_pred, laboratory_lowcostfeasibility_test$glycerol_gperL)/
                           (range(laboratory_lowcostfeasibility_train$glycerol_gperL)[2]-range(laboratory_lowcostfeasibility_train$glycerol_gperL)[1]),
                          caret::RMSE(laboratory_lowcostfeasibility_indval$glycerol_pred, laboratory_lowcostfeasibility_indval$glycerol_gperL)/
                           (range(laboratory_lowcostfeasibility_train$glycerol_gperL)[2]-range(laboratory_lowcostfeasibility_train$glycerol_gperL)[1]),
                          caret::R2(laboratory_lowcostfeasibility_train$glycerol_cal, laboratory_lowcostfeasibility_train$glycerol_gperL),
                          caret::R2(laboratory_lowcostfeasibility_train$glycerol_cv, laboratory_lowcostfeasibility_train$glycerol_gperL),
                          caret::R2(laboratory_lowcostfeasibility_test$glycerol_pred, laboratory_lowcostfeasibility_test$glycerol_gperL),
                          caret::R2(laboratory_lowcostfeasibility_indval$glycerol_pred, laboratory_lowcostfeasibility_indval$glycerol_gperL),
                          nCalSamples = dim(laboratory_lowcostfeasibility_train)[1],
                         ncomp = laboratory_lowcostfeasibility_PCs$Glycerol),
lowcost = c("glycerol",
           caret::RMSE(lowcost_train$glycerol_cal, lowcost_train$glycerol_gperL)/
             (range(lowcost_train$glycerol_gperL)[2]-range(lowcost_train$glycerol_gperL)[1]),
           caret::RMSE(lowcost_train$glycerol_cv, lowcost_train$glycerol_gperL)/
             (range(lowcost_train$glycerol_gperL)[2]-range(lowcost_train$glycerol_gperL)[1]),
           caret::RMSE(lowcost_test$glycerol_pred, lowcost_test$glycerol_gperL)/
             (range(lowcost_train$glycerol_gperL)[2]-range(lowcost_train$glycerol_gperL)[1]),
           caret::RMSE(lowcost_indval$glycerol_pred, lowcost_indval$glycerol_gperL)/
             (range(lowcost_train$glycerol_gperL)[2]-range(lowcost_train$glycerol_gperL)[1]),
           caret::R2(lowcost_train$glycerol_cal, lowcost_train$glycerol_gperL),
           caret::R2(lowcost_train$glycerol_cv, lowcost_train$glycerol_gperL),
           caret::R2(lowcost_test$glycerol_pred, lowcost_test$glycerol_gperL),
           caret::R2(lowcost_indval$glycerol_pred, lowcost_indval$glycerol_gperL),
           nCalSamples = dim(lowcost_train)[1],
           ncomp = lowcost_PCs$Glycerol)
) 


a %>% 
  write.csv("../tables/tableS3_normalized.csv")
                    
                    

