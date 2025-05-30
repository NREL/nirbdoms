# Code to produce S1 - table of constituent distribution in HPLC data 

# build a table displaying parameters for constituent distribution across the three datasets

# load packages 
library(tidyverse)
library(openxlsx)

# load data

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

# build a dataframe containing constituent and metadata for all observations

a <- tB %>% 
  select( glycerol_gperL, totalBDO_gperL, glucose_gperL, xylose_gperL, acetoin_gperL, instrument_NIR, experiment, fermenter) %>% 
  rbind( tA %>% 
           select( glycerol_gperL, totalBDO_gperL, glucose_gperL, xylose_gperL, acetoin_gperL, instrument_NIR, experiment, fermenter) )%>% 
  rbind( nirone %>% 
           select( glycerol_gperL, totalBDO_gperL, glucose_gperL, xylose_gperL, acetoin_gperL, instrument_NIR, experiment, fermenter) ) %>% 
 mutate(instrument_NIR = factor(instrument_NIR, levels = c("Laboratory Grade At-Line",
                                                            "Low Cost Grade At-Line",
                                                            "Laboratory Grade On-Line")))


# calculate the number of timepoint observations for each instrument 

n <- a %>% select(instrument_NIR) %>% 
  group_by(instrument_NIR) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = instrument_NIR, values_from =n ) %>% 
  mutate(Constituent = "All",
         parameter = "n")

# calculate the number of unique experiments for each instrument

nexp <- a %>% select(instrument_NIR, experiment, fermenter)%>% 
  group_by(instrument_NIR, experiment, fermenter) %>% 
  summarize(uniqueexp = unique(experiment, fermenter)) %>% 
  group_by(instrument_NIR) %>% 
  count()

# calculate summary statistics for each constituent for each data collection configuration, save the resulting statistics as a .csv file

a %>% select(glucose_gperL, instrument_NIR) %>% 
  group_by(instrument_NIR) %>%
  summarize(
    mean = mean(glucose_gperL),
    median = median(glucose_gperL),
    sd = sd(glucose_gperL),
    IQR = IQR(glucose_gperL),
    min = min(glucose_gperL),
    max = max(glucose_gperL)
  ) %>% mutate(Constituent = "glucose_gperL") %>% 
  rbind(
    a %>% select(xylose_gperL, instrument_NIR) %>% 
      group_by(instrument_NIR) %>%
      summarize(
        mean = mean(xylose_gperL),
        median = median(xylose_gperL),
        sd = sd(xylose_gperL),
        IQR = IQR(xylose_gperL),
        min = min(xylose_gperL),
        max = max(xylose_gperL)
      ) %>% mutate(Constituent = "xylose_gperL"))%>% 
  rbind(
    a %>% select(totalBDO_gperL, instrument_NIR) %>% 
      group_by(instrument_NIR) %>%
      summarize(
        mean = mean(totalBDO_gperL),
        median = median(totalBDO_gperL),
        sd = sd(totalBDO_gperL),
        IQR = IQR(totalBDO_gperL),
        min = min(totalBDO_gperL),
        max = max(totalBDO_gperL)
      ) %>% mutate(Constituent = "totalBDO_gperL"))%>% 
  rbind(
    a %>% select(acetoin_gperL, instrument_NIR) %>% 
      group_by(instrument_NIR) %>%
      summarize(
        mean = mean(acetoin_gperL),
        median = median(acetoin_gperL),
        sd = sd(acetoin_gperL),
        IQR = IQR(acetoin_gperL),
        min = min(acetoin_gperL),
        max = max(acetoin_gperL)
      ) %>% mutate(Constituent = "acetoin_gperL"))%>% 
  rbind(
    a %>% select(glycerol_gperL, instrument_NIR) %>% 
      group_by(instrument_NIR) %>%
      summarize(
        mean = mean(glycerol_gperL),
        median = median(glycerol_gperL),
        sd = sd(glycerol_gperL),
        IQR = IQR(glycerol_gperL),
        min = min(glycerol_gperL),
        max = max(glycerol_gperL)
      ) %>% mutate(Constituent = "glycerol_gperL")) %>% 
  pivot_longer(cols = c(mean, median, sd, IQR, min, max), names_to = "parameter", values_to = "value") %>% 
  pivot_wider(names_from = instrument_NIR, values_from = value) %>% 
  rbind(n) %>% write.csv("../tables/tableS1.csv")



