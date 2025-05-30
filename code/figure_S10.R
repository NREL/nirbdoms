# Figure S10- code to produce more time course HPLC vs NIR prediction plots for on-line model experiments 

# load packages

library(tidyverse)
library(patchwork)
library(egg)


#load data

speconly <- readRDS("../data/processed/dataframes/laboratoryonline_probe_spectraonly.RDS")

indval <- readRDS("../data/processed/dataframes/laboratoryonline_probe_indval.RDS")

test <- readRDS("../data/processed/dataframes/laboratoryonline_probe_testing.RDS")

training <- readRDS("../data/processed/dataframes/laboratoryonline_probe_training.RDS")

# pull and combine all HPLC data into one dataframe 

lc1 <- indval %>% select(glucose_gperL,xylose_gperL, totalBDO_gperL, acetoin_gperL,glycerol_gperL, sample, time_hours, experiment) %>% 
  rbind(test %>% 
          select(glucose_gperL,xylose_gperL, totalBDO_gperL, acetoin_gperL,glycerol_gperL, sample, time_hours, experiment)) %>% 
  rbind(training %>% 
          select(glucose_gperL,xylose_gperL, totalBDO_gperL, acetoin_gperL,glycerol_gperL, sample, time_hours, experiment))

# create a vector containing the names of the experiments 

experiments <- lc$experiment %>% unique()

# filter spectral data to contain only spectral data that is related to experiments with HPLC data

speconly_e <- speconly %>% filter(experiment %in% experiments)

# select out the prediction data from the spectral dataframe , reshaping for tidy plotting where constituent and prediction are set columns
# instead of each constituent having its own prediction column 
# add a variable that groups constituent as a product or reactant for faceting of plotting 

preds <- speconly_e%>% 
  pivot_longer(cols = c("glucose_pred","xylose_pred","bdo_pred","acetoin_pred","glycerol_pred"), 
               names_to = "constituent", values_to = "predicted") %>% 
  mutate(constituent= str_replace(constituent, "_pred","")) %>% 
   mutate(cons_groups = ifelse(constituent == "glucose", "Reactants", 
                               ifelse(constituent == "xylose", "Reactants", "Products"))) %>% 
   mutate(cons_groups = factor(cons_groups, levels = c("Reactants", "Products")))

# select out the HPLC data from the HPLC dataframe, reshaping for tidy plotting where constituent and measurements are set columns
# instead of each constituent having its own measurement column 
# add a variable that groups constituent as a product or reactant for faceting of plotting 
# plot measurements and predictions over time together

lc1 %>% 
  pivot_longer(cols = c(glucose_gperL, xylose_gperL, totalBDO_gperL, acetoin_gperL, glycerol_gperL), 
               names_to = "constituent", values_to = "measured") %>% 
  mutate(constituent = str_replace(constituent, "_gperL","")) %>%   
   mutate(constituent = ifelse(constituent == "totalBDO", "bdo", constituent))%>% 
  mutate(constituent = factor(constituent, levels = c("glucose","xylose","bdo","acetoin","glycerol"))) %>% 
   mutate(cons_groups = ifelse(constituent == "glucose", "Reactants", 
                               ifelse(constituent == "xylose", "Reactants", 
                                      "Products"))) %>% 
   mutate(cons_groups = factor(cons_groups, levels = c("Reactants", "Products"))) %>% 
  ggplot(aes(x=time_hours, y = measured, col = constituent)) +
   geom_point(aes(shape = "HPLC"), size =3) +
   theme_bw() +
   theme_bw() +
   geom_line(data = preds, aes(y=predicted, linetype = "NIR"), size = 2, alpha = .6) +
   scale_shape_manual(name = "Measurement Source", values = c( 16,1)) +
   scale_color_manual(name = "Constituent",values = c("#feb24c",
                                                      "#f03b20",
                                                      "#a6cee3",
                                                      "#1f78b4",
                                                      "#b2df8a")) +
  labs(x="Time (hours)", y = "g/L") +
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.width = unit(0.5,"mm"), 
        strip.text.y = element_text(colour = "black", size =9, face =2),
        strip.text.x= element_text(colour = "black", size =9, face =2),
        strip.background = element_blank(),
        legend.key.height = unit(1,"mm"),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey")) +
  theme(legend.position = "bottom") +
   facet_grid(cols = vars(cons_groups),
              rows = vars(experiment)) +
   ylim(0,160) + 
   guides(colour = guide_legend(override.aes = list(shape = 15))) + 
   scale_linetype_manual(name = "", values = 1) + 
   geom_point(shape =1,aes( y = measured,shape = "HPLC"), size =4,col = "black")


# save resulting figure

ggsave("../figures/S10_alltogether.tiff", width  = 140,height =300 ,units = "mm", compression = "lzw")
