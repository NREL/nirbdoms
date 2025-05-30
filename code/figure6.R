# figure6 - time course probe independent validation plot

#load packages

library(tidyverse)
library(patchwork)
library(egg)

#load data

speconly <- readRDS("../data/processed/dataframes/laboratoryonline_probe_spectraonly.RDS")
indval <- readRDS("../data/processed/dataframes/laboratoryonline_probe_indval.RDS")

# select out unique experiment spectra

speconly_e8 <- speconly %>% filter(experiment == "RN E8")

# group and reshape predictions by constituent for plotting, classify products vs substrates

preds <-speconly_e8 %>% 
  pivot_longer(cols = c("glucose_pred","xylose_pred","bdo_pred","acetoin_pred","glycerol_pred"), 
               names_to = "constituent", values_to = "predicted") %>% 
  mutate(constituent= str_replace(constituent, "_pred","")) %>% 
  mutate(cons_groups = ifelse(constituent == "glucose", "Substrates", 
                              ifelse(constituent == "xylose", "Substrates", 
                                     "Products")))%>%
  mutate(cons_groups = factor(cons_groups, levels = c("Substrates", "Products"))) 

# group and reshape HPLC measurements by constituent for plotting, classify products vs substrates

indval <- indval %>% 
  pivot_longer(cols = c(glucose_gperL, xylose_gperL, totalBDO_gperL, acetoin_gperL, glycerol_gperL), 
               names_to = "constituent", values_to = "measured") %>% 
  mutate(constituent = str_replace(constituent, "_gperL","")) %>% 
  mutate(constituent = ifelse(constituent == "totalBDO", "bdo", constituent)) %>% 
  pivot_longer(cols = c("glucose_pred","xylose_pred","bdo_pred","acetoin_pred","glycerol_pred"), 
               names_to = "constituent_predicted", values_to = "predicted") %>% 
  mutate(constituent_predicted = str_replace(constituent_predicted, "_pred","")) %>% 
  filter(constituent== constituent_predicted) %>% 
  mutate(constituent = factor(constituent, levels = c("glucose","xylose","bdo","acetoin","glycerol"))) %>% 
    mutate(cons_groups = ifelse(constituent == "glucose", "Substrates", 
                                ifelse(constituent == "xylose", "Substrates", 
                                       "Products"))) %>% 
  mutate(cons_groups = factor(cons_groups, levels = c("Substrates", "Products"))) 

# plot NIR and HPLC predictions over time together

 a <-  indval %>% ggplot(aes(x=time_hours, col = constituent))+geom_point( aes( y = measured,shape = "HPLC"), size =4)+
  theme_bw()+
  geom_line(data = preds, aes(y=predicted, linetype = "NIR"), size = 2, alpha = .5)+
  scale_shape_manual(name = "", values = c( 16,1))+
  scale_color_manual(name = "Constituent", values = c("#feb24c","#f03b20","#7570b3","#99d594","#3288bd") )+
  scale_linetype_manual(name = "", values = 1)+
  labs(x="Time (hours)", y = "Concentration (g/L)")+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.width = unit(0.5,"mm"), 
        strip.text.y = element_text(colour = "black", size =9, face =2),
        strip.text.x= element_text(colour = "black", size =9, face =2),
        strip.background = element_blank(),
        legend.key.height = unit(2,"mm"),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))+
  theme(legend.position = "bottom")+
    facet_grid(row = vars(cons_groups))+ 
  guides(colour = guide_legend(override.aes = list(shape = 15)),
         shape = "none",
         line = "none",
         linetype = "none")+ 
  geom_point(shape =1,aes( y = measured,shape = "HPLC"), size =4,col = "black")+ylim(-5, 175)
  
  
  tag_facet(a, tag_pool = c("A", "B"))

# save the plot
  
ggsave("../figures/fig6.tiff", width  = 185,height =185 ,units = "mm", compression = "lzw")




