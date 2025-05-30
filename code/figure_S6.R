# supplementary S10- predicted vs measured for acetoin in at line models

# load packages

library(ggpmisc)
library(tidyverse)

# load data frames

tA_full_train <- readRDS("../data/processed/dataframes/laboratoryatline_full_training.RDS")

tA_full_test <- readRDS("../data/processed/dataframes/laboratoryatline_full_testing.RDS")

tA_full_indval <- readRDS("../data/processed/dataframes/laboratoryatline_full_indval.RDS")


tA_nironefeasibility_train <- readRDS("../data/processed/dataframes/laboratoryatline_lowcostfeasibility_training.RDS")

tA_nironefeasibility_test <- readRDS("../data/processed/dataframes/laboratoryatline_lowcostfeasibility_testing.RDS")

tA_nironefeasibility_indval <- readRDS("../data/processed/dataframes/laboratoryatline_lowcostfeasibility_indval.RDS")


nirone_train <- readRDS("../data/processed/dataframes/lowcostatline_training.RDS")

nirone_test <- readRDS("../data/processed/dataframes/lowcostatline_testing.RDS")

nirone_indval <- readRDS("../data/processed/dataframes/lowcostatline_indval.RDS")

# create a variable called Feedstock that delineates sugar source into a few categories for simpler plotting by sugar source by color
# apply mapping to each dataset 

tA_full_train  <- tA_full_train |> 
  mutate(Feedstock = ifelse(feedstock == "Feed", "other",
                            ifelse(feedstock == "Seed", "other",
                                   ifelse(feedstock == "Revive", "other", 
                                          ifelse(feedstock == "Corn Stover", "Feedstock Hydrolyzate",
                                                 ifelse(feedstock == "Switchgrass", "Feedstock Hydrolyzate",
                                                        ifelse(feedstock == "Poplar", "Feedstock Hydrolyzate", feedstock)))))))|> 
  mutate(Feedstock = factor(Feedstock, levels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown")))

tA_full_test <- tA_full_test |> 
  mutate(Feedstock = ifelse(feedstock == "Feed", "other",
                            ifelse(feedstock == "Seed", "other",
                                   ifelse(feedstock == "Revive", "other", 
                                          ifelse(feedstock == "Corn Stover", "Feedstock Hydrolyzate",
                                                 ifelse(feedstock == "Switchgrass", "Feedstock Hydrolyzate",
                                                        ifelse(feedstock == "Poplar", "Feedstock Hydrolyzate", feedstock)))))))|> 
  mutate(Feedstock = factor(Feedstock, levels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown")))

tA_full_indval <- tA_full_indval |>  
  mutate(Feedstock = ifelse(feedstock == "Feed", "other",
                            ifelse(feedstock == "Seed", "other",
                                   ifelse(feedstock == "Revive", "other", 
                                          ifelse(feedstock == "Corn Stover", "Feedstock Hydrolyzate",
                                                 ifelse(feedstock == "Switchgrass", "Feedstock Hydrolyzate",
                                                        ifelse(feedstock == "Poplar", "Feedstock Hydrolyzate", feedstock))))))) |> 
  mutate(Feedstock = factor(Feedstock, levels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown")))

tA_nironefeasibility_train <- tA_nironefeasibility_train |>  
  mutate(Feedstock = ifelse(feedstock == "Feed", "other",
                            ifelse(feedstock == "Seed", "other",
                                   ifelse(feedstock == "Revive", "other", 
                                          ifelse(feedstock == "Corn Stover", "Feedstock Hydrolyzate",
                                                 ifelse(feedstock == "Switchgrass", "Feedstock Hydrolyzate",
                                                        ifelse(feedstock == "Poplar", "Feedstock Hydrolyzate", feedstock)))))))|> 
  mutate(Feedstock = factor(Feedstock, levels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown")))

tA_nironefeasibility_test <- tA_nironefeasibility_test |>  
  mutate(Feedstock = ifelse(feedstock == "Feed", "other",
                            ifelse(feedstock == "Seed", "other",
                                   ifelse(feedstock == "Revive", "other", 
                                          ifelse(feedstock == "Corn Stover", "Feedstock Hydrolyzate",
                                                 ifelse(feedstock == "Switchgrass", "Feedstock Hydrolyzate",
                                                        ifelse(feedstock == "Poplar", "Feedstock Hydrolyzate", feedstock)))))))|> 
  mutate(Feedstock = factor(Feedstock, levels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown")))

tA_nironefeasibility_indval <- tA_nironefeasibility_indval |>  
  mutate(Feedstock = ifelse(feedstock == "Feed", "other",
                            ifelse(feedstock == "Seed", "other",
                                   ifelse(feedstock == "Revive", "other", 
                                          ifelse(feedstock == "Corn Stover", "Feedstock Hydrolyzate",
                                                 ifelse(feedstock == "Switchgrass", "Feedstock Hydrolyzate",
                                                        ifelse(feedstock == "Poplar", "Feedstock Hydrolyzate", feedstock)))))))|> 
  mutate(Feedstock = factor(Feedstock, levels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown")))

nirone_train <- nirone_train |> 
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

nirone_test <-  nirone_test |> 
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

nirone_indval <- nirone_indval |> 
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

# Plot predicted vs measured, colored by feedstock, for each dataset 

a <- tA_full_train %>% 
  ggplot(aes(x=acetoin_cv, y = acetoin_gperL, col = Feedstock))+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  ylim(-5,55)+
  xlim(-5,55)+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Acetoin- Cross Validation", subtitle = "Laboratory at-line Full Model")+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))+
  theme(title = element_text(size = 5))

b <- tA_full_test %>% 
  ggplot(aes(x=acetoin_pred, y = acetoin_gperL, col = Feedstock))+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  ylim(-5,55)+
  xlim(-5,55)+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Acetoin- Independent Validation", subtitle = "Laboratory at-line Full Model")+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  theme(title = element_text(size = 5))

c <-  tA_full_indval %>% 
  ggplot(aes(x=acetoin_pred, y = acetoin_gperL, col = Feedstock))+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  ylim(-5,55)+
  xlim(-5,55)+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", 
       title = "Acetoin- Unq Exp Validation",
       subtitle = "Laboratory at-line Full Model")+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(strip.background = element_rect(fill = "white",
                                        colour = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "lightgrey"))+
  theme(title = element_text(size = 5))

d <- tA_nironefeasibility_train %>% 
  ggplot(aes(x=acetoin_cv, y = acetoin_gperL, col = Feedstock))+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  ylim(-5,55)+
  xlim(-5,55)+
  labs(x="Predicted (g/L)",
       y = "Measured (g/L)",
       title = "Acetoin- Cross Validation",
       subtitle = "Laboratory at line low cost feasibility range model")+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "lightgrey"))+
  theme(title = element_text(size = 5))

e <- tA_nironefeasibility_test %>% 
  ggplot(aes(x=acetoin_pred, y = acetoin_gperL, col = Feedstock))+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  ylim(-5,55)+
  xlim(-5,55)+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Acetoin- Independent Validation", subtitle = "Laboratory at line low cost feasibility range model")+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white",colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))+
  theme(title = element_text(size = 5)) 

f <-  tA_nironefeasibility_indval %>% 
  ggplot(aes(x=acetoin_pred, y = acetoin_gperL, col = Feedstock))+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  ylim(-5,55)+
  xlim(-5,55)+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Acetoin- Unq Exp Validation", subtitle = "Laboratory at line low cost feasibility range model")+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  theme(title = element_text(size = 5))

g <- nirone_train %>% 
  ggplot(aes(x=acetoin_cv, y = acetoin_gperL, col = Feedstock))+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  ylim(-5,55)+
  xlim(-5,55)+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Acetoin- Cross Validation", subtitle = "Low cost at line Model")+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(strip.background = element_rect(fill = "white", colour = "white"), 
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightgrey"))+
  theme(title = element_text(size = 5))

h <- nirone_test %>% 
  ggplot(aes(x=acetoin_pred, y = acetoin_gperL, col = Feedstock))+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  ylim(-5,55)+
  xlim(-5,55)+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Acetoin- Independent Validation", subtitle = "Low cost at line Model")+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))+
  theme(title = element_text(size = 5))

i <-  nirone_indval %>% 
  ggplot(aes(x=acetoin_pred, y = acetoin_gperL, col = Feedstock))+
  geom_point(show.legend=TRUE)+
  scale_color_manual(values = c("Pure Sugar" = "#33a02c", "Feedstock Hydrolyzate" = "#1f78b4", "other" = "#a6cee3","unknown" = "#b2df8a"),
                     labels = c("Pure Sugar", "Feedstock Hydrolyzate", "other", "unknown"),
                     drop = FALSE)+
  theme_bw()+
  coord_equal()+
  ylim(-5,55)+
  xlim(-5,55)+
  labs(x="Predicted (g/L)", y = "Measured (g/L)", title = "Acetoin- Unq Exp Validation", subtitle = "Low cost at line Model")+
  geom_abline()+
  geom_smooth(col = "black", method = "lm", se = FALSE)+
  stat_poly_eq( formula = y ~ x, aes(col = NULL, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), parse = TRUE, size = 2)+
  theme(strip.background = element_rect(fill = "white",colour = "white"),
        panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))+
  theme(title = element_text(size = 5))

# Combine plots into one master plot

j <- ((a+b+c)/(d+e+f)/(g+h+i))

j+plot_layout(guides ='collect')

# save combined plot

ggsave("../figures/S6_acetoinatlinepredvsmeasured_withfscolor.tiff", width  = 240,height =240 ,units = "mm", compression = "lzw")








  