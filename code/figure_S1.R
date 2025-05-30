# Figure S1: Box plots of constituent distributions by dataset

# load packages

library(tidyverse)
library(openxlsx)

# load data, filtering out glucose only fermentations and contaminated fermentations

tB <- read.xlsx("../data/raw/laboratoryonline_fulldata.xlsx") %>% 
  filter(!is.na(glucose_gperL)) %>% 
  filter(lacticAcid_gperL < 2.5) %>% 
  filter(aceticAcid_gperL <2.5) %>% 
  filter(ethanol_gperL < 2.5) %>% 
  filter(xylose_gperL != 0)

tA <- read.xlsx("../data/raw/laboratoryatline_ringcup_fulldata.xlsx") %>% 
  filter(lacticAcid_gperL < 2.5) %>% 
  filter(aceticAcid_gperL <2.5) %>% 
  filter(ethanol_gperL < 2.5)%>% 
  filter(xylose_gperL != 0)

nirone <- read.xlsx("../data/raw/lowcostatline_ringcup_fulldata.xlsx") %>% 
  filter(lacticAcid_gperL < 2.5) %>% 
  filter(aceticAcid_gperL <2.5) %>% 
  filter(ethanol_gperL < 2.5)%>% 
  filter(xylose_gperL != 0)

# select columns associated with constituent and metatdata, combine this data from each dataset into one master dataset for plotting together

a <- tB %>% 
  select( glycerol_gperL, totalBDO_gperL, glucose_gperL, xylose_gperL, acetoin_gperL, instrument_NIR, experiment, fermenter) %>% 
  rbind( tA %>% 
           select( glycerol_gperL, totalBDO_gperL, glucose_gperL, xylose_gperL, acetoin_gperL, instrument_NIR, experiment, fermenter) )%>% 
  rbind( nirone %>% 
           select( glycerol_gperL, totalBDO_gperL, glucose_gperL, xylose_gperL, acetoin_gperL, instrument_NIR, experiment, fermenter) ) %>% 
  mutate(instrument_NIR = factor(instrument_NIR, levels = c("Laboratory Grade At-Line",
                                                            "Low Cost Grade At-Line",
                                                            "Laboratory Grade On-Line")))

# create boxplots to show the distribution of each constituent across each dataset

a %>% 
  pivot_longer(cols = c(glycerol_gperL, totalBDO_gperL, glucose_gperL, xylose_gperL, acetoin_gperL),
               names_to = "constituent",
               values_to = "concentration") %>%
  mutate(constituent = factor(constituent, levels = c("glucose_gperL", "xylose_gperL", "totalBDO_gperL", "acetoin_gperL", "glycerol_gperL"),
                              labels = c("Glucose", "Xylose", "2,3-BDO", "Acetoin", "Glycerol"))) %>% 
  ggplot(aes(col= instrument_NIR, y =concentration )) + 
  geom_boxplot() +
  scale_color_manual(values = c("#7570b3","#1b9e77", "#d95f02"),
                     name = "Dataset") +
  facet_wrap(~constituent, nrow = 1)+
  labs(y = "Concentration (g/L)")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              strip.text= element_text(colour = "black",
                                       size = 12,
                                       face =2),
              strip.background = element_blank(),
              legend.key.height = unit(1,"mm"),
              panel.background = element_rect(fill = "white",
                                              colour = "black", 
                                              size = 0.5, 
                                              linetype = "solid"),
        legend.position = "bottom")

# save resulting figure

ggsave("../figures/S1.tiff", width  = 200,height =75 ,units = "mm", compression = "lzw")
