# figure 3- representative spectra 

#load packages

library(tidyverse)
library(prospectr)
library(openxlsx)
library(ggsci)
library(patchwork)

# source spectra processing functions

source("./spectraprocessingfunctions.R")

# nirone data (low cost at line)


# load data

nirone <- read.xlsx("../data/raw/lowcostatline_ringcup_fulldata.xlsx")


# select spectarl columns 

speccols <- colnames(nirone)[which(colnames(nirone)=="2000"):which(colnames(nirone)=="2450")]

# average spectra 

a <- nirone %>% 
  group_by(instrument_NIR) %>% 
  summarise(across(all_of(speccols), mean)) %>% 
  mutate(instrument_NIR = paste0("(A) ", instrument_NIR))

# reshape data

aa <- a %>% pivot_longer(cols = all_of(speccols), names_to = "wavelength", values_to = "abs") %>% 
  mutate(wavenumber = 10000000/as.numeric(wavelength))

# process spectra

aa_p <- process_spectra_nirone(a) %>% as.data.frame() %>%  pivot_longer(cols = any_of(speccols), names_to = "wavelength", values_to = "tabs") %>% 
  mutate(wavenumber = 10000000/as.numeric(wavelength),
         instrument_NIR = aa$instrument_NIR[1])%>% 
  mutate(model = "NIRONE 2.5 Range")

rm(nirone)

# thermo 140 data (lab grade on line)
 
# load data

thermoB <- read.xlsx("../data/raw/laboratoryonline_fulldata.xlsx")

# select spectra columns

speccols <- colnames(thermoB)[which(colnames(thermoB)=="3999.639893"):which(colnames(thermoB)=="9503.484375")]

# average data by column 

b <- thermoB %>% 
  mutate(across(all_of(speccols), as.numeric)) %>% 
  group_by(instrument_NIR) %>% 
  summarise(across(all_of(speccols), mean)) %>% 
  mutate(instrument_NIR = paste0("(C) ", instrument_NIR))

# reshape data 

bb <- b %>% pivot_longer(cols = all_of(speccols), names_to = "wavenumber", values_to = "abs") %>% 
  mutate(wavenumber = as.numeric(wavenumber))

# process spectra

bb_p <- process_spectra_thermoB(b) %>% as.data.frame() %>%   pivot_longer(cols = everything(), names_to = "wavenumber", values_to = "tabs") %>% 
  mutate(wavenumber = as.numeric(wavenumber),
         instrument_NIR = bb$instrument_NIR[1])%>% 
  mutate(model = "Fiber Optic Probe Range")

rm(thermoB)

# thermo A (lab grade at line)

# load data

thermoA <- read.xlsx("../data/raw/laboratoryatline_ringcup_fulldata.xlsx")

# average spectra 

c <- thermoA %>% 
  group_by(instrument_NIR) %>% 
  summarise(across(all_of(speccols), mean)) %>% 
  mutate(instrument_NIR = paste0("(B) ", instrument_NIR))

# reshape data

cc <- c %>% pivot_longer(cols = all_of(speccols), names_to = "wavenumber", values_to = "abs")%>% 
  mutate(wavenumber = as.numeric(wavenumber)) 

# process spectra

cc_p <- process_spectra_A_full(c) %>% as.data.frame() %>%  pivot_longer(cols = everything(), names_to = "wavenumber", values_to = "tabs")%>% 
  mutate(wavenumber = as.numeric(wavenumber),
         instrument_NIR = cc$instrument_NIR[1]) %>% 
  mutate(model = "Full")

rm(thermoA)

# plot raw and transformed thermo A

# reshape data for plotting, putting all raw signal data together in the same dataframe
rawsig <- rbind(aa %>% 
                  select(!wavelength),bb,cc)

# reshape transformed data for plotting, adding all raw signal data into the same dataframe

t_sig <- rbind(aa_p %>%
                 select(!wavelength) , bb_p, cc_p)

# create transformed ranges that are the same for all graphs

max_firsttA <- max(rawsig$abs)
min_firsttA <- min(rawsig$abs)

max_secondtA <- max(t_sig$tabs)
min_secondtA <- min(t_sig$tabs)

# scale and shift variables calculated based on desired mins and maxes

scaletA = (max_secondtA-min_secondtA)/(max_firsttA-min_firsttA)
shifttA = (min_firsttA - min_secondtA)/-13


# Function to scale secondary axis

scale_function <- function(x, scale, shift){
  return ((x)*scale - shift)
}

# Function to scale secondary variable values

inv_scale_function <- function(x, scale, shift){
  return ((x + shift)/scale)
}

# add ranges for plotting

rect_ranges <- 
  data.frame(xmax = 4800, xmin = 4000, ymin = 1.79, ymax = 1.82, instrument_NIR = "(A) Low Cost Grade At-Line")

rect_ranges2 <- 
  data.frame(xmin = 5600, xmax = 6100, ymin =1.79 , ymax =1.82, instrument_NIR = "(A) Low Cost Grade At-Line")

# create dataframes designating where text should be added

text_add <- 
  data.frame(x =(5600+6100)/2, y = 1.9, label = "1st Overtone Region" ,instrument_NIR = "(A) Low Cost Grade At-Line")

text_add_2 <- 
  data.frame(x =(4000+4800)/2, y = 1.9, label = "Combinational Region" ,instrument_NIR = "(A) Low Cost Grade At-Line" )

# plot out average raw and transformed spectra by spectral collection configuration 

rawsig  %>% 
  left_join(t_sig, by = c("wavenumber"="wavenumber", "instrument_NIR" = "instrument_NIR")) %>% 
  mutate(facet = "(B)") %>% 
  ggplot(aes(x= wavenumber, y = abs, col = "Transflectance"))+geom_line()+
  geom_line(aes(y =  inv_scale_function(tabs, scaletA, shifttA),
                col = "Transformed Transflectance"))+
  theme_bw()+
  scale_color_manual(values = c("#1b9e77", "#d95f02"), guide="none")+
  labs(x="", y = "Transflectance")+
theme(strip.background = element_rect(fill = "white",
                                      colour = "white"),
      panel.background = element_rect(fill = "white",
                                      colour = "black",
                                      size = 0.5, linetype = "solid")
      )+
  scale_y_continuous(limits = c(.95*min_firsttA, 1.05*max_firsttA), 
                     sec.axis = sec_axis(~scale_function(., scaletA, shifttA),
                                         name = "Transformed Transflectance"))+
  theme(axis.title.y = element_text(color = "#1b9e77"),
         axis.title.y.right = element_text(color = "#d95f02"))+
  scale_x_reverse()+
  xlim(6100,4000)+
  theme(legend.position = "top")+
  facet_wrap(~instrument_NIR, nrow = 3)+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.width = unit(0.5,"mm"), 
        strip.text.y = element_text(colour = "black", size =9, face =2),
        strip.text.x= element_text(colour = "black", size =9, face =2, hjust = 0),
        strip.background = element_blank(),
        legend.key.height = unit(2,"mm"),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))+
geom_text(data = text_add, aes(x =x, y = y, label = label ),
          size = 3, col = "black",inherit.aes = FALSE)+#nirone
 geom_rect(data = rect_ranges, aes(xmin = xmin, xmax = xmax, ymin =ymin , ymax =ymax), fill = "#66a61e" , col = "#66a61e",
           inherit.aes = FALSE)+  
  geom_rect(data = rect_ranges2, aes(xmin = xmin, xmax = xmax, ymin =ymin , ymax =ymax), fill = "#66a61e" , col = "#66a61e",
                                            inherit.aes = FALSE)+
  geom_text(data = text_add_2, aes(x =x, y =y, label = label),
            size = 3, col = "black", inherit.aes = FALSE)


# save figure

ggsave("../figures/fig3_spectra.tiff", width  = 185,height = 185 ,units = "mm", compression = "lzw")
 
 