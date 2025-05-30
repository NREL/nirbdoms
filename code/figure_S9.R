# supplementary S13- pred vs measured for laboratory grade on line probe models

# load packages

library(ggpmisc)
library(tidyverse)
library(patchwork)

# load data frames

tB_train <- readRDS("../data/processed/dataframes/laboratoryonline_probe_training.RDS")

tB_test <- readRDS("../data/processed/dataframes/laboratoryonline_probe_testing.RDS")

tB_indval <- readRDS("../data/processed/dataframes/laboratoryonline_probe_indval.RDS")

# set ranges for plots by constituent

rangeg <- max(tB_train$glucose_cv,tB_test$glucose_pred,
              tB_indval$glucose_pred,tB_train$glucose_gperL,tB_test$glucose_gperL,
              tB_indval$glucose_gperL
              )

rangex <- max(tB_train$xylose_cv,tB_test$xylose_pred,
              tB_indval$xylose_pred,tB_train$xylose_gperL,tB_test$xylose_gperL,
              tB_indval$xylose_gperL
)

rangeb <- max(tB_train$bdo_cv,tB_test$bdo_pred,
              tB_indval$bdo_pred,tB_train$totalBDO_gperL,tB_test$totalBDO_gperL,
              tB_indval$totalBDO_gperL
)

rangea <- max(tB_train$acetoin_cv,tB_test$acetoin_pred,
              tB_indval$acetoin_pred,tB_train$acetoin_gperL,tB_test$acetoin_gperL,
              tB_indval$acetoin_gperL
)

rangegy <- max(tB_train$glycerol_cv,tB_test$glycerol_pred,
              tB_indval$glycerol_pred,tB_train$glycerol_gperL,tB_test$glycerol_gperL,
              tB_indval$glycerol_gperL
)


# create a variable called Feedstock that delineates sugar source into a few categories for simpler plotting by sugar source by color
# apply mapping to each dataset 

tB_train <- tB_train |> 
  mutate(Feedstock = ifelse(feedstock == "Feed", "other",
                            ifelse(feedstock == "Seed", "other",
                                   ifelse(feedstock == "Revive", "other", 
                                          ifelse(feedstock == "Corn Stover", "Feedstock Hydrolyzate",
                                                 ifelse(feedstock == "Switchgrass", "Feedstock Hydrolyzate",
                                                        ifelse(feedstock == "Poplar", "Feedstock Hydrolyzate", 
                                                               ifelse(feedstock == "pure sugar", "Pure Sugar",
                                                                      ifelse(feedstock == "seed", "other", 
                                                                             ifelse(feedstock ==  "Corn Stover+Pure Sugar", "other", feedstock))))))))))|> 
  mutate(Feedstock = factor(Feedstock, levels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown")))


tB_test <- tB_test |>
  mutate(Feedstock = ifelse(feedstock == "Feed", "other",
                            ifelse(feedstock == "Seed", "other",
                                   ifelse(feedstock == "Revive", "other", 
                                          ifelse(feedstock == "Corn Stover", "Feedstock Hydrolyzate",
                                                 ifelse(feedstock == "Switchgrass", "Feedstock Hydrolyzate",
                                                        ifelse(feedstock == "Poplar", "Feedstock Hydrolyzate", 
                                                               ifelse(feedstock == "pure sugar", "Pure Sugar",
                                                                      ifelse(feedstock == "seed", "other", 
                                                                             ifelse(feedstock ==  "Corn Stover+Pure Sugar", "other", feedstock))))))))))|> 
  mutate(Feedstock = factor(Feedstock, levels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown")))

tB_indval <- tB_indval |>
  mutate(Feedstock = ifelse(feedstock == "Feed", "other",
                            ifelse(feedstock == "Seed", "other",
                                   ifelse(feedstock == "Revive", "other", 
                                          ifelse(feedstock == "Corn Stover", "Feedstock Hydrolyzate",
                                                 ifelse(feedstock == "Switchgrass", "Feedstock Hydrolyzate",
                                                        ifelse(feedstock == "Poplar", "Feedstock Hydrolyzate", 
                                                               ifelse(feedstock == "pure sugar", "Pure Sugar",
                                                                      ifelse(feedstock == "seed", "other", 
                                                                             ifelse(feedstock ==  "Corn Stover+Pure Sugar", "other", feedstock))))))))))|> 
  mutate(Feedstock = factor(Feedstock, levels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown")))

# plot predicted vs measured by constituent, coloring by sugar source type

# glucose

a <- tB_train %>% 
  ggplot(aes(x=glucose_cv, y = glucose_gperL, col = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Glucose CV")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangeg+5)+
  xlim(-10,rangeg+5)

b <- tB_test %>% 
  ggplot(aes(x=glucose_pred, y = glucose_gperL, color = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Glucose Ind Val")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangeg+5)+
  xlim(-10,rangeg+5)

c <-  tB_indval %>% 
  ggplot(aes(x=glucose_pred, y = glucose_gperL, col = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Glucose Ind Exp Val")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangeg+5)+
  xlim(-10,rangeg+5)

# xylose


d <- tB_train %>% 
  ggplot(aes(x=xylose_cv, y = xylose_gperL, col = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Xylose CV")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangex+5)+
  xlim(-10,rangex+5)


e <- tB_test %>% 
  ggplot(aes(x=xylose_pred, y = xylose_gperL, col = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Xylose Ind Val")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangex+5)+
  xlim(-10,rangex+5)


f <-  tB_indval %>% 
  ggplot(aes(x=xylose_pred, y = xylose_gperL, col = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Xylose Ind Exp Val")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangex+5)+
  xlim(-10,rangex+5)

# total bdo

g <- tB_train %>% 
  ggplot(aes(x=bdo_cv, y = totalBDO_gperL, col = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Total BDO CV")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangeb+5)+
  xlim(-10,rangeb+5)

h <- tB_test %>% 
  ggplot(aes(x=bdo_pred, y = totalBDO_gperL, col = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Total BDO Ind Val")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangeb+5)+
  xlim(-10,rangeb+5)

i <-  tB_indval %>% 
  ggplot(aes(x=bdo_pred, y = totalBDO_gperL, col = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Total BDO Ind Exp Val")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangeb+5)+
  xlim(-10,rangeb+5)

#acetoin

j <- tB_train %>% 
  ggplot(aes(x=acetoin_cv, y = acetoin_gperL, col = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Acetoin CV")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangea+5)+
  xlim(-10,rangea+5)

k <- tB_test %>% 
  ggplot(aes(x=acetoin_pred, y = acetoin_gperL, col = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Acetoin Ind Val")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangea+5)+
  xlim(-10,rangea+5)

l <-  tB_indval %>% 
  ggplot(aes(x=acetoin_pred, y = acetoin_gperL, col = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Acetoin Ind Exp Val")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangea+5)+
  xlim(-10,rangea+5)

#glycerol

m <- tB_train %>% 
  ggplot(aes(x=glycerol_cv, y = glycerol_gperL, col = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Glycerol CV")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangegy+5)+
  xlim(-10,rangegy+5)

n <- tB_test %>% 
  ggplot(aes(x=glycerol_pred, y = glycerol_gperL, col = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Glycerol Ind Val")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangegy+5)+
  xlim(-10,rangegy+5)

o <-  tB_indval %>% 
  ggplot(aes(x=glycerol_pred, y = glycerol_gperL, col = Feedstock))+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Glycerol Ind Exp Val")+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(title = element_text(size = 5))+
  ylim(-10,rangegy+5)+
  xlim(-10,rangegy+5)

# format all plots together into one master plot

p <-(a+b+c)/(d+e+f)/(g+h+i)/(j+k+l)/(m+n+o)

p+plot_layout(guides = "collect")

# save combined plot

ggsave("../figures/S9_onlinepredvsmeasured_withcols.tiff", width  = 240,height =340 ,units = "mm", compression = "lzw")

