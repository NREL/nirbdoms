# supplementary S12- pred vs measured for thermo a probe feasibility models

# load packages

library(ggpmisc)
library(tidyverse)
library(patchwork) 

# load data frames

tA_probefeasibility_train <- readRDS("../data/processed/dataframes/laboratoryatline_probefeasibility_training.RDS")

tA_probefeasibility_test <- readRDS("../data/processed/dataframes/laboratoryatline_probefeasibility_testing.RDS")

tA_probefeasibility_indval <- readRDS("../data/processed/dataframes/laboratoryatline_probefeasibility_indval.RDS")

# set ranges for plotting 

rangeg <- max(tA_probefeasibility_train$glucose_cv,tA_probefeasibility_test$glucose_pred,
              tA_probefeasibility_indval$glucose_pred,tA_probefeasibility_train$glucose_gperL,tA_probefeasibility_test$glucose_gperL,
              tA_probefeasibility_indval$glucose_gperL
              )

rangex <- max(tA_probefeasibility_train$xylose_cv,tA_probefeasibility_test$xylose_pred,
              tA_probefeasibility_indval$xylose_pred,tA_probefeasibility_train$xylose_gperL,tA_probefeasibility_test$xylose_gperL,
              tA_probefeasibility_indval$xylose_gperL
)

rangeb <- max(tA_probefeasibility_train$bdo_cv,tA_probefeasibility_test$bdo_pred,
              tA_probefeasibility_indval$bdo_pred,tA_probefeasibility_train$totalBDO_gperL,tA_probefeasibility_test$totalBDO_gperL,
              tA_probefeasibility_indval$totalBDO_gperL
)

rangea <- max(tA_probefeasibility_train$acetoin_cv,tA_probefeasibility_test$acetoin_pred,
              tA_probefeasibility_indval$acetoin_pred,tA_probefeasibility_train$acetoin_gperL,tA_probefeasibility_test$acetoin_gperL,
              tA_probefeasibility_indval$acetoin_gperL
)

rangegy <- max(tA_probefeasibility_train$glycerol_cv,tA_probefeasibility_test$glycerol_pred,
              tA_probefeasibility_indval$glycerol_pred,tA_probefeasibility_train$glycerol_gperL,tA_probefeasibility_test$glycerol_gperL,
              tA_probefeasibility_indval$glycerol_gperL
)

# produce predicted vs measured plots for each constituent

# create a variable called Feedstock that will be used to color observations by feedstock type to show how predictions on pure sugar and hydrolyzates compare
# apply to each dataset

tA_probefeasibility_train <- tA_probefeasibility_train |> 
  mutate(Feedstock = ifelse(feedstock == "Feed", "other",
                            ifelse(feedstock == "Seed", "other",
                                   ifelse(feedstock == "Revive", "other", 
                                          ifelse(feedstock == "Corn Stover", "Feedstock Hydrolyzate",
                                                 ifelse(feedstock == "Switchgrass", "Feedstock Hydrolyzate",
                                                        ifelse(feedstock == "Poplar", "Feedstock Hydrolyzate", 
                                                               ifelse(feedstock == "pure sugar", "Pure Sugar",
                                                                      ifelse(feedstock == "seed", "other", 
                                                                             ifelse(feedstock == "Corn Stover+Pure Sugar", "other", feedstock))))))))))|> 
  mutate(Feedstock = factor(Feedstock, levels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown")))


tA_probefeasibility_test <- tA_probefeasibility_test |>
  mutate(Feedstock = ifelse(feedstock == "Feed", "other",
                            ifelse(feedstock == "Seed", "other",
                                   ifelse(feedstock == "Revive", "other", 
                                          ifelse(feedstock == "Corn Stover", "Feedstock Hydrolyzate",
                                                 ifelse(feedstock == "Switchgrass", "Feedstock Hydrolyzate",
                                                        ifelse(feedstock == "Poplar", "Feedstock Hydrolyzate", 
                                                               ifelse(feedstock == "pure sugar", "Pure Sugar",
                                                                      ifelse(feedstock == "seed", "other", 
                                                                             ifelse(feedstock == "Corn Stover+Pure Sugar", "other", feedstock))))))))))|> 
  mutate(Feedstock = factor(Feedstock, levels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown")))

tA_probefeasibility_indval <- tA_probefeasibility_indval |>
  mutate(Feedstock = ifelse(feedstock == "Feed", "other",
                            ifelse(feedstock == "Seed", "other",
                                   ifelse(feedstock == "Revive", "other", 
                                          ifelse(feedstock == "Corn Stover", "Feedstock Hydrolyzate",
                                                 ifelse(feedstock == "Switchgrass", "Feedstock Hydrolyzate",
                                                        ifelse(feedstock == "Poplar", "Feedstock Hydrolyzate", 
                                                               ifelse(feedstock == "pure sugar", "Pure Sugar",
                                                                      ifelse(feedstock == "seed", "other", 
                                                                             ifelse(feedstock == "Corn Stover+Pure Sugar", "other", feedstock))))))))))|> 
  mutate(Feedstock = factor(Feedstock, levels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown")))

# plot predicted vs measured, colored by feedstock type, across all prediction types and constituents

a <- tA_probefeasibility_train %>% 
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


a

b <- tA_probefeasibility_test %>% 
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



c <-  tA_probefeasibility_indval %>% 
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

c
#xylose


d <- tA_probefeasibility_train %>% 
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


e <- tA_probefeasibility_test %>% 
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

e

f <-  tA_probefeasibility_indval %>% 
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

f
#total bdo


g <- tA_probefeasibility_train %>% 
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



g

h <- tA_probefeasibility_test %>% 
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

h


i <-  tA_probefeasibility_indval %>% 
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

i
#acetoin


j <- tA_probefeasibility_train %>% 
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



j

k <- tA_probefeasibility_test %>% 
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

k

l <-  tA_probefeasibility_indval %>% 
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


l
#glycerol


m <- tA_probefeasibility_train %>% 
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



m
n <- tA_probefeasibility_test %>% 
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



o <-  tA_probefeasibility_indval %>% 
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


# format all plots together

p <-(a+b+c)/(d+e+f)/(g+h+i)/(j+k+l)/(m+n+o)

p+plot_layout(guides = "collect")

# save plots

ggsave("../figures/S8_atlineonelinefeasibilitypredvsmeasured_withcols.tiff", width  = 240,height =340 ,units = "mm", compression = "lzw")
