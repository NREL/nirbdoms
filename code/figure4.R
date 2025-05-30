#figure 4- pred vs measured plots showing promise of at line monitoring using lab grade at line dataset 

#load packages

library(tidyverse)
library(patchwork)
library(egg)

#load data

testing <- readRDS("../data/processed/dataframes/laboratoryatline_full_testing.RDS")
indval <- readRDS("../data/processed/dataframes/laboratoryatline_full_indval.RDS")


# goal is to create one pred vs measured plot for each constituent, coloring by sample group (independent validation vs unique experiment)

# reshape data so that I can facet by constituent and plot independent validation (test) and unique experiment (ind) together

test <- testing %>% 
  pivot_longer(cols = c(glucose_gperL, xylose_gperL, totalBDO_gperL, acetoin_gperL, glycerol_gperL),
               names_to = "constituent", values_to = "measured") %>% 
  mutate(constituent = str_replace(constituent, "_gperL","")) %>% 
  mutate(constituent = ifelse(constituent == "totalBDO", "bdo", constituent)) %>% 
  pivot_longer(cols = c(glucose_pred, xylose_pred, bdo_pred, acetoin_pred, glycerol_pred),
               names_to = "constituent_predicted", values_to = "predicted") %>% 
  mutate(constituent_predicted = str_replace(constituent_predicted, "_pred","")) %>% 
  filter(constituent == constituent_predicted)%>% 
  mutate(constituent = factor(constituent, levels =c("glucose", "xylose", "bdo", "acetoin", "glycerol"),labels = c("Glucose", "Xylose","2,3-BDO","Acetoin", "Glycerol")))


ind <- indval%>% 
  pivot_longer(cols = c(glucose_gperL, xylose_gperL, totalBDO_gperL, acetoin_gperL, glycerol_gperL),
               names_to = "constituent", values_to = "measured") %>% 
  mutate(constituent = str_replace(constituent, "_gperL","")) %>% 
  mutate(constituent = ifelse(constituent == "totalBDO", "bdo", constituent)) %>% 
  pivot_longer(cols = c(glucose_pred, xylose_pred, bdo_pred, acetoin_pred, glycerol_pred),
               names_to = "constituent_predicted", values_to = "predicted") %>% 
  mutate(constituent_predicted = str_replace(constituent_predicted, "_pred","")) %>% 
  filter(constituent == constituent_predicted)%>% 
  mutate(constituent = factor(constituent, levels =c("glucose", "xylose", "bdo", "acetoin", "glycerol"),labels = c("Glucose", "Xylose","2,3-BDO","Acetoin", "Glycerol"))) 


# group substrates and products together so that I can plot in two rows

ind_subs <- ind %>% 
  filter(constituent == "Glucose"|
           constituent == "Xylose")
ind_prods <- ind %>% 
  filter(constituent != "Glucose"&
           constituent != "Xylose")

# Plots for the substrates (glucose and xylose)

aa <- test %>% 
  filter(constituent == "Glucose"|
  constituent == "Xylose") %>% 
 ggplot(aes(x=predicted, y = measured, col = "Independent Validation"))+geom_point(alpha = .3, size = 1)+
  facet_grid(col = vars(constituent))+
  geom_point(data = ind_subs, aes(col = "Unique Experiment Validation"),alpha = 1, size = 1)+coord_equal()+theme_bw()+geom_abline(linewidth = .2)+
  scale_color_manual(values = c("#f1a340", "#998ec3"),name = "")+labs(x="Predicted (g/L)", y = "Measured (g/L)")+
  theme(strip.background = element_rect(fill = "white",
                                        colour = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        axis.text = element_text(size = 12))+theme(legend.position = "bottom")+ylim(-10,175)+xlim(-10,175)

# add formatting 

subs <- tag_facet(aa, tag_pool = c("A",
                          "B"))+
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text= element_text(colour = "black", size = 12, face =2),
        strip.background = element_blank(),
        legend.key.height = unit(1,"mm"),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "inches"))
subs


# create predicted vs measured plots for the products (bdo, acetoin, glycerol)

aa <- test %>% 
  filter(constituent != "Glucose"&
           constituent != "Xylose") %>% 
  ggplot(aes(x=predicted, y = measured, col = "Independent Validation"))+geom_point(alpha = .3, size = 1)+
  facet_grid(col = vars(constituent))+
  geom_point(data = ind_prods, aes(col = "Unique Experiment Validation"),alpha = 1, size = 1)+coord_equal()+theme_bw()+geom_abline(linewidth = .2)+
  scale_color_manual(values = c("#f1a340", "#998ec3"),name = "")+labs(x="Predicted (g/L)", y = "Measured (g/L)")+
  theme(strip.background = element_rect(fill = "white",
                                        colour = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        axis.text = element_text(size = 12))+theme(legend.position = "bottom")+ylim(-10,175)+xlim(-10,175)

# add formatting 

prods <- tag_facet(aa, tag_pool = c("C","D", "E"))+
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text= element_text(colour = "black", size = 12, face =2),
        strip.background = element_blank(),
        legend.key.height = unit(1,"mm"),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"))+ 
  theme(plot.margin = unit(c(0, 0, 0, -1), "inches"))

# format plots together and adjust figure sizes

total <- subs/prods
wrap_plots(subs, prods, heights = c(1,1))+
  plot_layout(guides = "collect")&theme(legend.position = "bottom")

# save plot 

ggsave("../figures/fig4.tiff", width  = 150,height =125 ,units = "mm", compression = "lzw")

