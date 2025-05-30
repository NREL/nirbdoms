# Spectral noise analysis  comparison- code to perform analysis and plot results 
# Analysis is performed on both nanopure water cells and an acetyl b reference standard
# To make the reference figure that compares constituent signal to noise signal,
# reference material spectra are also read in and plotted 
# Includes code to generate Supplemental Figure 11 Plot: Results of Spectral Noise Analysis 

# load necessary packages

library(tidyverse)
library(openxlsx)


# source spectral processing functions 

source("./spectraprocessingfunctions.R")

##
## Analysis 1: Comparing the variance in water controls between the laboratory grade and low cost at line 
## configurations
##

# load water control data for lab grade at line set up

labatline <- read.xlsx("../data/raw/laboratorygradeatline_ringcup_controlsforspectralnoiseanalysis.xlsx") |> 
  filter(sample == "water")

# turn spectral columns into a vector for selection (a required input for preprocess function)

speccols <- colnames(labatline)[21:2155]

# perform spectral processing

lab_spec_proces <- process_spectra_A_nironetest(labatline) |> as.data.frame()

# calculate variance at each wavenumber 

variance <- lab_spec_proces |>
  summarize(across(any_of(speccols), var))

# load water control data for low cost grade at line set up

lowcostatline <- read.xlsx("../data/raw/lowcostatline_ringcup_controlsforspectralnoiseanalysis.xlsx") |> 
  filter(sampleName == "water")

# there are more water control samples for the low cost than the lab grade, so I will 
# downsample the population so that n is the same for variance calculations

# sample 20
downsampled <- sample(30, 20)

# select the 20 

lowcostatline<- lowcostatline[downsampled,]

# select the spectral columns for low cost instrument

lc_speccols <- colnames(lowcostatline)[11:461]

# low cost data is being read in as characters- re-parse as numeric 

lowcostatline <- lowcostatline |> 
  mutate(across(all_of(lc_speccols), as.numeric))

# process the spectral data using low cost at line processing function

lc_spec_process <- process_spectra_nirone(lowcostatline) |> as.data.frame()

# calculate the variance associated with the low cost water measurements

variance_lc <- lc_spec_process |>
  summarize(across(any_of(lc_speccols), var))

# combine and plot water variance across spectral range together

watervariance <- variance |> 
  pivot_longer(cols = everything(), 
               names_to = "wn", 
               values_to = "abs") |>
  mutate(wn = round(as.numeric(wn), 1)) |> 
  mutate(instrument = "laboratory grade") |> 
  rbind(variance_lc |> 
          pivot_longer(
          cols = everything(),
        names_to = "wl",
        values_to = "abs") |> 
    mutate(wn = round(10000000/as.numeric(wl),1)) |> 
    select(!wl) |> 
    mutate(instrument = "low cost")) |> 
    ggplot(aes(x=wn, y = abs, col = instrument))+geom_line()+
  labs(y = "variance", x = "wavenumber (cm^-1)", title  = "variance vs wavenumber: water")+
  theme_bw()+
  scale_x_reverse()
  

watervariance

# create a summary table that shows the difference in mean standard deviation of water control measurements
# across both sampling configurations across the entire spectral range used in both configurations

water_sd_summary <- variance |> 
  pivot_longer(cols = everything(), 
               names_to = "wn", 
               values_to = "var") |>
  mutate(wn = round(as.numeric(wn), 1)) |>
  mutate(n = dim(labatline)[1]) |> 
  mutate(sample_sd = sqrt(var/n)) |> 
  mutate(instrument = "laboratory grade") |> 
  rbind(variance_lc |> 
          pivot_longer(
            cols = everything(),
            names_to = "wl",
            values_to = "var") |> 
          mutate(wn = round(10000000/as.numeric(wl),1)) |> 
          select(!wl) |> 
          mutate(n = dim(lowcostatline)[1]) |> 
          mutate(sample_sd = sqrt(var/n)) |> 
          mutate(instrument = "low cost")) |> 
  group_by(instrument) |> 
  summarize(mean_sample_sd = mean(sample_sd))


# control for differences in resolution by performing the average on a 
# selection of 10 evenly spaced values in the region of interest


# first select wavelengths for the lab grade system

targetvalues <- seq(4300, 4480, (4480-4300)/9)

# calculate average standard deviation in noise measurements across the entire dataset for the 
# lab grade dataset

lgsum <- variance |> 
  pivot_longer(cols = everything(), 
               names_to = "wn", 
               values_to = "var") |>
  mutate(wn = round(as.numeric(wn), 1)) |>
  mutate(n = dim(labatline)[1]) |> 
  mutate(sample_sd = sqrt(var/n)) |> 
  mutate(instrument = "laboratory grade")


# identify the wavelengths in the low cost dataset that most closely match those selected in the 
# lab grade dataset and select them from the dataset

closest_values <- sapply(targetvalues, function(tv){
  lgsum$wn[which.min(abs(lgsum$wn-tv))]
})

# select those 10 wavelengths from the noise summary dataframe

result <- lgsum |> 
  filter(wn %in% closest_values)

# calculate the average standard deviation in noise measurements across entire used spectral range for the 
# low cost dataset

lcsum <- variance_lc |> 
  pivot_longer(
    cols = everything(),
    names_to = "wl",
    values_to = "var") |> 
  mutate(wn = round(10000000/as.numeric(wl),1)) |> 
  select(!wl) |> 
  mutate(n = dim(lowcostatline)[1]) |> 
  mutate(sample_sd = sqrt(var/n)) |> 
  mutate(instrument = "low cost")

# identify the wavelengths in the low cost dataset that most closely match those selected in the 
# low grade dataset and select them from the dataset

closest_values <- sapply(targetvalues, function(tv){
  lcsum$wn[which.min(abs(lcsum$wn-tv))]
})

# select those 10 wavelengths from the noise summary dataframe

result_lc <- lcsum |> 
  filter(wn %in% closest_values)

# bind the 10 wavelength variance calculations together into one dataframe

compare_10wn <- rbind(result_lc, result)


# calculate the mean standard deviation across the 10 selected points for both instrument configurations

compare_10wn |> 
  group_by(instrument) |> 
  summarize(mean(sample_sd))

# save the calculations as a dataframe

comp <- compare_10wn |> 
  group_by(instrument) |> 
  summarize(meansd = mean(sample_sd))

# calculate the mean difference between the two measurements

ratio <- round(comp$meansd[2]/comp$meansd[1], 1)

text <- paste0("Low cost spectra has \n", ratio, "X greater noise \n than laboratory grade spectra \n in water controls \n across the 10 selected wavenumbers")

water_sd <- variance |> 
  pivot_longer(cols = everything(), 
               names_to = "wn", 
               values_to = "var") |>
  mutate(wn = round(as.numeric(wn), 1)) |>
  mutate(n = dim(labatline)[1]) |> 
  mutate(sample_sd = sqrt(var/n)) |> 
  mutate(instrument = "laboratory grade") |> 
  rbind(variance_lc |> 
          pivot_longer(
            cols = everything(),
            names_to = "wl",
            values_to = "var") |> 
          mutate(wn = round(10000000/as.numeric(wl),1)) |> 
          select(!wl) |> 
          mutate(n = dim(lowcostatline)[1]) |> 
          mutate(sample_sd = sqrt(var/n)) |> 
          mutate(instrument = "low cost")) |> 
  ggplot(aes(x=wn, y= sample_sd, col = instrument))+
  geom_line()+labs(title = "Water Control Spectral Noise Analysis",
                   y = "Control Sample Standard Deviation",
                   x=bquote('Wavenumber '(cm^-1)))+theme_bw()+
  ylim(0, .0003)+
geom_point(data = compare_10wn, aes(x = wn, y = sample_sd))+
  scale_x_reverse()+
  geom_text(aes(x = 4150, y = .00025, label= text), size = 3, col= "black", show.legend = FALSE)+
  xlim(4800, 4000)+
  geom_vline(xintercept = 4480, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4460, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4440, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4420, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4400, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4380, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4360, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4340, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4320, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4300, color = "grey", linetype = "dashed")+
  theme(legend.text = element_text(size = 8),
        legend.position = "inside",
        legend.position.inside = c(.15,.9),
                                                                            legend.title = element_text(size = 8),
                                                                            legend.key.width = unit(0.5,"mm"),
                                                                            strip.text.y = element_blank(),
                                                                            plot.margin = unit(c(1,1,1,0), "pt"),
                                                                            strip.text.x= element_blank(),
                                                                            strip.background = element_blank(),
                                                                            legend.key.height = unit(3,"mm"),
                                                                            panel.background = element_blank(),
                                                                            panel.grid.major = element_blank(),
                                                                            panel.grid.minor = element_blank())+theme(
                                                                              text = element_text(size = 8),
                                                                              plot.title = element_text(hjust = .5,size = 8,face = "bold"),
                                                                              plot.tag = element_text(face = "bold"))


water_sd



## STANDARD SPECTRA REGIONS OF INTEREST
## 
##

# read in standard material spectra collected on lab grade at-line system

data <- read.xlsx("../data/raw/laboratoryatline_ringcup_standardSpectra.xlsx") %>% 
  mutate(type = ifelse(constituent == "Glucose", "Reactant", 
                       ifelse(constituent == "Xylose", "Reactant", "Product"))) %>% 
  mutate(type = factor(type, levels = c("Reactant", "Product"))) %>% 
  mutate(constituent = factor(constituent, levels = c("Glucose", "Xylose","BDO" , "Acetoin","Glycerol")))

# select spectral columns 

cols <- colnames(data)[5:2139]

# process standard material spectra 

sg <- data %>% select(all_of(cols)) %>% standardNormalVariate() %>% savitzkyGolay(m=1, p = 2, w= 11) %>% as.data.frame()

# plot sandard material spectra

aa <- sg %>%
  cbind(data %>% select(!any_of(cols))) %>% 
  pivot_longer(cols = any_of(cols), names_to = "wn", values_to = "abs") %>% 
  mutate(wn = as.numeric(wn)) %>% 
  ggplot(aes(x=wn,y = abs, group = constituent, col = constituent))+
  geom_line(size = 1, alpha = .8)+
  theme_bw()+
  labs(x=bquote('Wavenumber '(cm^-1)), 
       y = "Transformed Transflectance",
       title = "Standard Material Spectral Signal Response")+
  scale_x_reverse()+
  xlim(4800, 4000)+
  ylim(-0.06,0.03)+
  theme(
        text = element_text(size = 8),
        plot.title = element_text(hjust = .5,size = 8,face = "bold"),
        plot.tag = element_text(face = "bold"))+
  scale_color_manual(name = "Constituent", 
                     values = c("#feb24c","#f03b20","#7570b3","#99d594","#3288bd") )+
  theme(legend.text = element_text(size = 8),
        legend.position = "inside",
        legend.position.inside = c(.1,.2),
        legend.title = element_text(size = 8),
        legend.key.width = unit(3,"mm"),
        strip.text.y = element_blank(),
        plot.margin = unit(c(1,1,1,0), "pt"),
        strip.text.x= element_blank(),
        strip.background = element_blank(),
        legend.key.height = unit(5,"mm"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_vline(xintercept = 4480, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4460, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4440, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4420, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4400, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4380, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4360, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4340, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4320, color = "grey", linetype = "dashed")+
  geom_vline(xintercept = 4300, color = "grey", linetype = "dashed")

aa

# plot both standard material and water control spectra together to show noise vs signal! 

(aa/water_sd)+plot_layout( axis_titles = "collect_x" )

# save plot 

ggsave("../figures/Supfigure_noiseanalysis.tiff", width  = 140,height =240 ,units = "mm", compression = "lzw")


# save differences in noise!

compare_10wn |> 
  group_by(instrument) |> 
  summarize(mean(sample_sd)) |> 
write.csv( "../tables/spectralnoisesummary.csv")



###
###
###
### ACETAL B Reference Noise Comparison

# the code below works through the sample spetral noise comparison process as that used with the water controls using an acetal b reference cell that was scanned with the water references

labatline <- read.xlsx("../data/raw/laboratorygradeatline_ringcup_controlsforspectralnoiseanalysis.xlsx") |> 
  filter(sample == "acetal b")
speccols <- colnames(labatline)[21:2155]


lab_spec_proces <- process_spectra_A_nironetest(labatline) |> as.data.frame()

variance <- lab_spec_proces |>
  summarize(across(any_of(speccols), var))



lowcostatline <- read.xlsx("../data/raw/lowcostatline_ringcup_controlsforspectralnoiseanalysis.xlsx") |> 
  filter(sampleName == "acetal b")

downsampled <- sample(30, 19)

lowcostatline <- lowcostatline[downsampled,]
lc_speccols <- colnames(lowcostatline)[11:461]

lowcostatline <- lowcostatline |> 
  mutate(across(all_of(lc_speccols), as.numeric))

lc_spec_process <- process_spectra_nirone(lowcostatline) |> as.data.frame()

variance_lc <- lc_spec_process |>
  summarize(across(any_of(lc_speccols), var))



acetalvariance <- variance |> 
  pivot_longer(cols = everything(), 
               names_to = "wn", 
               values_to = "abs") |>
  mutate(wn = round(as.numeric(wn), 1)) |> 
  mutate(instrument = "laboratory grade") |> 
  rbind(variance_lc |> 
          pivot_longer(
            cols = everything(),
            names_to = "wl",
            values_to = "abs") |> 
          mutate(wn = round(10000000/as.numeric(wl),1)) |> 
          select(!wl) |> 
          mutate(instrument = "low cost")) |> 
  ggplot(aes(x=wn, y = abs, col = instrument))+geom_line()+
  labs(y = "variance", x = "wavenumber (cm^-1)", title  = "variance vs wavenumber: acetal b")+
  theme_bw()+
  scale_x_reverse()+
  labs(title = "water")


acetal_sd <- variance |> 
  pivot_longer(cols = everything(), 
               names_to = "wn", 
               values_to = "var") |>
  mutate(wn = round(as.numeric(wn), 1)) |>
  mutate(n = dim(labatline)[1]) |> 
  mutate(sample_sd = sqrt(var/n)) |> 
  mutate(instrument = "laboratory grade") |> 
  rbind(variance_lc |> 
          pivot_longer(
            cols = everything(),
            names_to = "wl",
            values_to = "var") |> 
          mutate(wn = round(10000000/as.numeric(wl),1)) |> 
          select(!wl) |> 
          mutate(n = dim(lowcostatline)[1]) |> 
          mutate(sample_sd = sqrt(var/n)) |> 
          mutate(instrument = "low cost")) |> 
  ggplot(aes(x=wn, y= sample_sd, col = instrument))+theme_bw()+
  geom_line()+labs(title = "Acetal b Control",
                   y = "Sample Standard Deviation",
                   x = 'wavenumber (cm^-1)')+
  ylim(0, .0007)

library(patchwork)
water_sd/acetal_sd+plot_layout(guides = "collect",axis_titles = "collect")




acetalb_sd_summary <- variance |> 
  pivot_longer(cols = everything(), 
               names_to = "wn", 
               values_to = "var") |>
  mutate(wn = round(as.numeric(wn), 1)) |>
  mutate(n = dim(labatline)[1]) |> 
  mutate(sample_sd = sqrt(var/n)) |> 
  mutate(instrument = "laboratory grade") |> 
  rbind(variance_lc |> 
          pivot_longer(
            cols = everything(),
            names_to = "wl",
            values_to = "var") |> 
          mutate(wn = round(10000000/as.numeric(wl),1)) |> 
          select(!wl) |> 
          mutate(n = dim(lowcostatline)[1]) |> 
          mutate(sample_sd = sqrt(var/n)) |> 
          mutate(instrument = "low cost")) |> 
  group_by(instrument) |> 
  summarize(mean_sample_sd = mean(sample_sd))

summary <- acetalb_sd_summary |> 
  mutate(sampletype = "acetal b") |> 
  rbind(water_sd_summary |> 
          mutate(sampletype = "water"))  

  

# selecting only from 10 evenly spaced values in the region of interest
# first for lab grade

targetvalues <- seq(4300, 4500, (4500-4300)/10)
lgsum <- acetalb_sd_summary <- variance |> 
  pivot_longer(cols = everything(), 
               names_to = "wn", 
               values_to = "var") |>
  mutate(wn = round(as.numeric(wn), 1)) |>
  mutate(n = dim(labatline)[1]) |> 
  mutate(sample_sd = sqrt(var/n)) |> 
  mutate(instrument = "laboratory grade")

closest_values <- sapply(targetvalues, function(tv){
  lgsum$wn[which.min(abs(lgsum$wn-tv))]
})
result <- lgsum |> 
  filter(wn %in% closest_values)

# now for low cost
lcsum <- variance_lc |> 
  pivot_longer(
    cols = everything(),
    names_to = "wl",
    values_to = "var") |> 
  mutate(wn = round(10000000/as.numeric(wl),1)) |> 
  select(!wl) |> 
  mutate(n = dim(lowcostatline)[1]) |> 
  mutate(sample_sd = sqrt(var/n)) |> 
  mutate(instrument = "low cost")

closest_values <- sapply(targetvalues, function(tv){
  lcsum$wn[which.min(abs(lcsum$wn-tv))]
})
result_lc <- lcsum |> 
  filter(wn %in% closest_values)

