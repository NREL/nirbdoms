# figure 5: bar chart comparing results for thermo 203, thermo 203 nirone range, and nirone model data sets/models


# load packages 

library(tidyverse)
library(patchwork)
library(caret)
library(egg)
library(ggtext)

# load data sets

thermoA_full_testing <- readRDS("../data/processed/dataframes/laboratoryatline_full_testing.RDS")
thermoA_full_indval <- readRDS("../data/processed/dataframes/laboratoryatline_full_indval.RDS")
thermoA_full_cal <- readRDS("../data/processed/dataframes/laboratoryatline_full_training.RDS")


thermoA_nironefeasibility_testing <- readRDS("../data/processed/dataframes/laboratoryatline_probefeasibility_testing.RDS")
thermoA_nironefeasibility_indval <- readRDS("../data/processed/dataframes/laboratoryatline_probefeasibility_indval.RDS")
thermoA_nironefeasibiliity_cal <- readRDS("../data/processed/dataframes/laboratoryatline_probefeasibility_training.RDS")


nirone_testing <- readRDS("../data/processed/dataframes/lowcostatline_testing.RDS")
nirone_indval <- readRDS("../data/processed/dataframes/lowcostatline_indval.RDS")
nirone_cal <- readRDS("../data/processed/dataframes/lowcostatline_training.RDS")


#calculate RMSEs across constituents and groups

aa <- data.frame(
  constituent = c("Glucose","Xylose","Total BDO","Acetoin","Glycerol"),
  RMSEP= c(
    caret::RMSE(thermoA_full_testing$glucose_pred, thermoA_full_testing$glucose_gperL)/
      (range(thermoA_full_cal$glucose_gperL)[2]-range(thermoA_full_cal$glucose_gperL)[1]),
    caret::RMSE(thermoA_full_testing$xylose_pred, thermoA_full_testing$xylose_gperL)/
      (range(thermoA_full_cal$xylose_gperL)[2]-range(thermoA_full_cal$xylose_gperL)[1]),
    caret::RMSE(thermoA_full_testing$bdo_pred, thermoA_full_testing$totalBDO_gperL)/
      (range(thermoA_full_cal$totalBDO_gperL)[2]-range(thermoA_full_cal$totalBDO_gperL)[1]),
    caret::RMSE(thermoA_full_testing$acetoin_pred, thermoA_full_testing$acetoin_gperL)/
      (range(thermoA_full_cal$acetoin_gperL)[2]-range(thermoA_full_cal$acetoin_gperL)[1]),
    caret::RMSE(thermoA_full_testing$glycerol_pred, thermoA_full_testing$glycerol_gperL)/
      (range(thermoA_full_cal$glycerol_gperL)[2]-range(thermoA_full_cal$glycerol_gperL)[1])),
    RMSEPunq = c(
      caret::RMSE(thermoA_full_indval$glucose_pred, thermoA_full_indval$glucose_gperL)/
        (range(thermoA_full_cal$glucose_gperL)[2]-range(thermoA_full_cal$glucose_gperL)[1]),
      caret::RMSE(thermoA_full_indval$xylose_pred, thermoA_full_indval$xylose_gperL)/
        (range(thermoA_full_cal$xylose_gperL)[2]-range(thermoA_full_cal$xylose_gperL)[1]),
      caret::RMSE(thermoA_full_indval$bdo_pred, thermoA_full_indval$totalBDO_gperL)/
        (range(thermoA_full_cal$totalBDO_gperL)[2]-range(thermoA_full_cal$totalBDO_gperL)[1]),
      caret::RMSE(thermoA_full_indval$acetoin_pred, thermoA_full_indval$acetoin_gperL)/
        (range(thermoA_full_cal$acetoin_gperL)[2]-range(thermoA_full_cal$acetoin_gperL)[1]),
      caret::RMSE(thermoA_full_indval$glycerol_pred, thermoA_full_indval$glycerol_gperL)/
        (range(thermoA_full_cal$glycerol_gperL)[2]-range(thermoA_full_cal$glycerol_gperL)[1])
  )
) %>% 
  mutate(model = "Lab Grade NIR Spectra") %>% 
  rbind(data.frame(
    constituent = c("Glucose","Xylose","Total BDO","Acetoin","Glycerol"),
    RMSEP = c(
      caret::RMSE(thermoA_nironefeasibility_testing$glucose_pred, thermoA_nironefeasibility_testing$glucose_gperL)/
        (range(thermoA_full_cal$glucose_gperL)[2]-range(thermoA_full_cal$glucose_gperL)[1]),
      caret::RMSE(thermoA_nironefeasibility_testing$xylose_pred, thermoA_nironefeasibility_testing$xylose_gperL)/
        (range(thermoA_full_cal$xylose_gperL)[2]-range(thermoA_full_cal$xylose_gperL)[1]),
      caret::RMSE(thermoA_nironefeasibility_testing$bdo_pred, thermoA_nironefeasibility_testing$totalBDO_gperL)/
        (range(thermoA_full_cal$totalBDO_gperL)[2]-range(thermoA_full_cal$totalBDO_gperL)[1]),
      caret::RMSE(thermoA_nironefeasibility_testing$acetoin_pred, thermoA_nironefeasibility_testing$acetoin_gperL)/
        (range(thermoA_full_cal$acetoin_gperL)[2]-range(thermoA_full_cal$acetoin_gperL)[1]),
      caret::RMSE(thermoA_nironefeasibility_testing$glycerol_pred, thermoA_nironefeasibility_testing$glycerol_gperL)/
        (range(thermoA_full_cal$glycerol_gperL)[2]-range(thermoA_full_cal$glycerol_gperL)[1])),
    RMSEPunq= c(
      caret::RMSE(thermoA_nironefeasibility_indval$glucose_pred, thermoA_nironefeasibility_indval$glucose_gperL)/
        (range(thermoA_full_cal$glucose_gperL)[2]-range(thermoA_full_cal$glucose_gperL)[1]),
      caret::RMSE(thermoA_nironefeasibility_indval$xylose_pred, thermoA_nironefeasibility_indval$xylose_gperL)/
        (range(thermoA_full_cal$xylose_gperL)[2]-range(thermoA_full_cal$xylose_gperL)[1]),
      caret::RMSE(thermoA_nironefeasibility_testing$bdo_pred, thermoA_nironefeasibility_testing$totalBDO_gperL)/
        (range(thermoA_full_cal$totalBDO_gperL)[2]-range(thermoA_full_cal$totalBDO_gperL)[1]),
      caret::RMSE(thermoA_nironefeasibility_indval$acetoin_pred, thermoA_nironefeasibility_indval$acetoin_gperL)/
        (range(thermoA_full_cal$acetoin_gperL)[2]-range(thermoA_full_cal$acetoin_gperL)[1]),
      caret::RMSE(thermoA_nironefeasibility_indval$glycerol_pred, thermoA_nironefeasibility_indval$glycerol_gperL)/
        (range(thermoA_full_cal$glycerol_gperL)[2]-range(thermoA_full_cal$glycerol_gperL)[1])
  )
  ) %>% 
    mutate(model = "Lab Grade NIR Spectra, Limited to Low Cost Range"))%>% 
  rbind(data.frame(
    constituent = c("Glucose","Xylose","Total BDO","Acetoin","Glycerol"),
    RMSEP = c(
      caret::RMSE(nirone_testing$glucose_pred, nirone_testing$glucose_gperL)/
        (range(nirone_cal$glucose_gperL)[2]-range(nirone_cal$glucose_gperL)[1]),
      caret::RMSE(nirone_testing$xylose_pred, nirone_testing$xylose_gperL)/
        (range(nirone_cal$xylose_gperL)[2]-range(nirone_cal$xylose_gperL)[1]),
      caret::RMSE(nirone_testing$bdo_pred, nirone_testing$totalBDO_gperL)/
        (range(nirone_cal$totalBDO_gperL)[2]-range(nirone_cal$totalBDO_gperL)[1]),
      caret::RMSE(nirone_testing$acetoin_pred, nirone_testing$acetoin_gperL)/
        (range(nirone_cal$acetoin_gperL)[2]-range(nirone_cal$acetoin_gperL)[1]),
      caret::RMSE(nirone_testing$glycerol_pred, nirone_testing$glycerol_gperL)/
        (range(nirone_cal$glycerol_gperL)[2]-range(nirone_cal$glycerol_gperL)[1])),
      RMSEPunq = c(
      caret::RMSE(nirone_indval$glucose_pred, nirone_indval$glucose_gperL)/
        (range(nirone_cal$glucose_gperL)[2]-range(nirone_cal$glucose_gperL)[1]),
      caret::RMSE(nirone_indval$xylose_pred, nirone_indval$xylose_gperL)/
        (range(nirone_cal$xylose_gperL)[2]-range(nirone_cal$xylose_gperL)[1]),
      caret::RMSE(nirone_indval$bdo_pred, nirone_indval$totalBDO_gperL)/
        (range(nirone_cal$totalBDO_gperL)[2]-range(nirone_cal$totalBDO_gperL)[1]),
      caret::RMSE(nirone_indval$acetoin_pred, nirone_indval$acetoin_gperL)/
        (range(nirone_cal$acetoin_gperL)[2]-range(nirone_cal$acetoin_gperL)[1]),
      caret::RMSE(nirone_indval$glycerol_pred, nirone_indval$glycerol_gperL)/
        (range(nirone_cal$glycerol_gperL)[2]-range(nirone_cal$glycerol_gperL)[1])
    )
  ) %>% 
    mutate(model = "Low Cost NIR Spectra")) %>%
  pivot_longer(cols = c(RMSEP, RMSEPunq), names_to = "parameter", values_to = "value") %>% 
  mutate(parameter = factor(parameter, labels = c("RMSEP", bquote(RMSE[unq])))) %>% 
  mutate(constituent = factor(constituent, levels = c("Glucose","Xylose","Total BDO","Acetoin","Glycerol"),
                              labels = c("Glucose","Xylose","2,3-BDO","Acetoin","Glycerol") )) %>% 
  mutate(model = factor(model, levels = c("Lab Grade NIR Spectra", "Lab Grade NIR Spectra, Limited to Low Cost Range", "Low Cost NIR Spectra")))

labels <- c("RMSEP",bquote(RMSEP[unq]) )

# produce bar plot to visualize dataframe produced above

aaa <- aa %>% 
  ggplot(aes(fill=model, y = value, x = parameter))+
           geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_markdown(color = "black", size = 11))+
  scale_x_discrete(name = NULL,labels = labels
  ) +
  facet_grid(cols = vars(constituent))+
  labs(x="Performance Parameter", y = "RMSE Normalized to Constituent Calibration Range")+theme_bw()+theme(legend.position = "bottom")+
  scale_fill_manual(values = c("#a6cee3","#1f78b4","#b2df8a"), name = "")+
  theme(strip.background = element_rect(fill = "white", colour = "white"),
  panel.background = element_rect(fill = "white",
  colour = "black",size = 0.5, linetype = "solid"),
   panel.grid.major = element_line(size = 0.5, linetype = 'solid',
    colour = "lightgrey"))+theme(legend.position = "bottom")

tag_facet(aaa, tag_pool = c("A",
                           "B","C","D", "E"))+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        strip.text= element_text(colour = "black", size =9, face =2),
        strip.background = element_blank(),
        legend.key.height = unit(1,"mm"),
        axis.title.y = element_text(size = 8),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))

ggsave("../figures/fig5.tiff", width  = 185,height =85 ,units = "mm", compression = "lzw")

         