# Supplemental Figure S2: pairs plot for each calibration set

# load packages

library(tidyverse)
library(patchwork)

# add a color scheme to refer to 

cols <- c("#7570b3","#1b9e77", "#d95f02")

# load datasets 

thermoA_full_cal <- readRDS("../data/processed/dataframes/laboratoryatline_full_training.RDS")
thermoB_full_cal <- readRDS("../data/processed/dataframes/laboratoryonline_probe_training.RDS")
nirone_full_cal <- readRDS("../data/processed/dataframes/lowcostatline_training.RDS")

# select out constituent variables from datasets and combine constituent data from each dataset

data <- thermoA_full_cal %>% 
  select( glucose_gperL, xylose_gperL, totalBDO_gperL, acetoin_gperL, glycerol_gperL) %>% 
  mutate(dataset = "laboratory at line")%>% 
  rbind(
    thermoB_full_cal%>% 
      select(glucose_gperL, xylose_gperL, totalBDO_gperL, acetoin_gperL, glycerol_gperL) %>% 
      mutate(dataset = "laboratory on line")
  ) %>% 
  rbind(
    nirone_full_cal%>% 
      select(glucose_gperL, xylose_gperL, totalBDO_gperL, acetoin_gperL, glycerol_gperL) %>% 
      mutate(dataset = "low cost at line")
  )

# plot each combination of constituents, coloring by the dataset that the observation belows to 

a <- data %>% ggplot(aes(x=glucose_gperL, y = xylose_gperL, col = dataset))+
  geom_point() +
  theme_bw() +
  scale_color_manual(values = cols) +
  facet_wrap(~dataset)+
  theme(legend.position = "none")

b <- data %>% ggplot(aes(x=glucose_gperL, y = totalBDO_gperL, col = dataset))+
  geom_point() +
  theme_bw() +
  scale_color_manual(values = cols) +
  facet_wrap(~dataset)+
  theme(legend.position = "none")

c <- data %>% ggplot(aes(x=glucose_gperL, y = acetoin_gperL, col = dataset))+
  geom_point() +
  theme_bw() +
  scale_color_manual(values = cols) +
  facet_wrap(~dataset)+
  theme(legend.position = "none")

d <- data %>% 
  ggplot(aes(x=glucose_gperL, y = glycerol_gperL, col = dataset)) +
  geom_point() +
  theme_bw() + 
  scale_color_manual(values = cols) +
  facet_wrap(~dataset)+
  theme(legend.position = "none")


e <- data %>% ggplot(aes(x=xylose_gperL, y = totalBDO_gperL, col = dataset)) +
  geom_point() +
  theme_bw() +
  scale_color_manual(values = cols) +
  facet_wrap(~dataset)+
  theme(legend.position = "none")

f <- data %>% ggplot(aes(x=xylose_gperL, y = acetoin_gperL, col = dataset)) +
  geom_point() +
  theme_bw() +
  scale_color_manual(values = cols) +
  facet_wrap(~dataset)+
  theme(legend.position = "none")

g <- data %>% ggplot(aes(x=xylose_gperL, y = glycerol_gperL, col = dataset))+ 
  geom_point() +
  theme_bw() +
  scale_color_manual(values = cols) +
  facet_wrap(~dataset)+
  theme(legend.position = "none")

h <- data %>% ggplot(aes(x=totalBDO_gperL, y = acetoin_gperL, col = dataset))+
  geom_point() +
  theme_bw() +
  scale_color_manual(values = cols) +
  facet_wrap(~dataset)+
  theme(legend.position = "none")

i<- data %>% ggplot(aes(x=totalBDO_gperL, y = glycerol_gperL, col = dataset))+
  geom_point() +
  theme_bw() +
  scale_color_manual(values = cols) +
  facet_wrap(~dataset)+
  theme(legend.position = "none")

j <- data %>% ggplot(aes(x=acetoin_gperL, y = glycerol_gperL, col = dataset))+
  geom_point() +
  theme_bw() +
  scale_color_manual(values = cols) +
  facet_wrap(~dataset)+
  theme(legend.position = "none")

# for sizing purposes, plot the first 5 generated plots together

(a/b/c/d/e) 

# save the combined figure

ggsave( "../figures/S2A_pairs.tiff", width  = 185,height = 250 ,units = "mm", compression = "lzw")

# for sizing purposes, plot the second 5 generated plots together

(f/g/h/i/j)

# save the combined figure 

ggsave( "../figures/S2B_pairs.tiff", width  = 185,height = 250, units = "mm", compression = "lzw")
