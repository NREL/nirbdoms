# table s5: comparing unique experiment results for lab at line lab at-line online feasibility, and lab on line

# load packages 

library(tidyverse)
library(patchwork)
library(caret)

# load datasets 

thermoA_full_indval <- readRDS("../data/processed/dataframes/laboratoryatline_full_indval.RDS")
thermoA_probefeasibility_indval <- readRDS("../data/processed/dataframes/laboratoryatline_probefeasibility_indval.RDS")
thermoB_probe_indval <- readRDS("../data/processed/dataframes/laboratoryonline_probe_indval.RDS")
thermoB_probe_indval_scaleup <- readRDS("../data/processed/dataframes/laboratoryonline_probe_scaleupval.RDS")

# calculate RMSEs across constituents and groups

aa<-  data.frame(
  model = "Laboratory Grade At-Line",
  constituent = c("Glucose","Xylose","Total BDO","Acetoin","Glycerol"),
    RMSEPunq = c(
      caret::RMSE(thermoA_full_indval$glucose_pred, thermoA_full_indval$glucose_gperL),
      caret::RMSE(thermoA_full_indval$xylose_pred, thermoA_full_indval$xylose_gperL),
      caret::RMSE(thermoA_full_indval$bdo_pred, thermoA_full_indval$totalBDO_gperL),
      caret::RMSE(thermoA_full_indval$acetoin_pred, thermoA_full_indval$acetoin_gperL),
      caret::RMSE(thermoA_full_indval$glycerol_pred, thermoA_full_indval$glycerol_gperL)
      )) |> rbind(
      data.frame(
        model = "Laboratory Grade At-Line On-Line Feasibility",
        constituent = c("Glucose","Xylose","Total BDO","Acetoin","Glycerol"),
        RMSEPunq = c(
          caret::RMSE(thermoA_probefeasibility_indval$glucose_pred, thermoA_probefeasibility_indval$glucose_gperL),
          caret::RMSE(thermoA_probefeasibility_indval$xylose_pred, thermoA_probefeasibility_indval$xylose_gperL),
          caret::RMSE(thermoA_probefeasibility_indval$bdo_pred, thermoA_probefeasibility_indval$totalBDO_gperL),
          caret::RMSE(thermoA_probefeasibility_indval$acetoin_pred, thermoA_probefeasibility_indval$acetoin_gperL),
          caret::RMSE(thermoA_probefeasibility_indval$glycerol_pred, thermoA_probefeasibility_indval$glycerol_gperL))
    )
) |>  rbind(
    data.frame(
      model = "Laboratory Grade On-Line 1.5L",
      constituent = c("Glucose","Xylose","Total BDO","Acetoin","Glycerol"),
      RMSEPunq = c(
        caret::RMSE(thermoB_probe_indval$glucose_pred, thermoB_probe_indval$glucose_gperL),
        caret::RMSE(thermoB_probe_indval$xylose_pred, thermoB_probe_indval$xylose_gperL),
        caret::RMSE(thermoB_probe_indval$bdo_pred, thermoB_probe_indval$totalBDO_gperL),
        caret::RMSE(thermoB_probe_indval$acetoin_pred, thermoB_probe_indval$acetoin_gperL),
        caret::RMSE(thermoB_probe_indval$glycerol_pred, thermoB_probe_indval$glycerol_gperL))
    )
  ) |> rbind(
  data.frame(
    model = "Laboratory Grade On-Line 10L",
    constituent = c("Glucose","Xylose","Total BDO","Acetoin","Glycerol"),
    RMSEPunq = c(
      caret::RMSE(thermoB_probe_indval_scaleup$glucose_pred, thermoB_probe_indval_scaleup$glucose_gperL),
      caret::RMSE(thermoB_probe_indval_scaleup$xylose_pred, thermoB_probe_indval_scaleup$xylose_gperL),
      caret::RMSE(thermoB_probe_indval_scaleup$bdo_pred, thermoB_probe_indval_scaleup$totalBDO_gperL),
      caret::RMSE(thermoB_probe_indval_scaleup$acetoin_pred, thermoB_probe_indval_scaleup$acetoin_gperL),
      caret::RMSE(thermoB_probe_indval_scaleup$glycerol_pred, thermoB_probe_indval_scaleup$glycerol_gperL))
  )
)

# save results as csv 

aa|> pivot_wider(
  names_from = constituent, values_from = RMSEPunq
) |> write.csv("../tables/tableS5.csv")
 

         