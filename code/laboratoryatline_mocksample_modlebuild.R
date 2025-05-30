# script for building mock BSRD Models for Manuscript

# load packages

library(tidyverse)
library(prospectr)
library(pls)
library(openxlsx)
library(patchwork)
library(GGally)

source("./spectraprocessingfunctions.R")

# load data

data <- read.xlsx("../data/raw/laboratoryatline_rinccup_mockSampleSpectra.xlsx")

#process spectra, save as a matrix 

data$spec1 <- process_spectra_A_full(data) 

#build wc validation matrix

wc <- data.frame(bdo = data$totalBDO_gperL, 
                 glucose = data$glucose_gperL, 
                 xylose = data$xylose_gperL,
                 acetoin = data$acetoin_gperL,
                 glycerol = data$glycerol_gperL) %>% as.matrix()

#build the model
fullmodel <- mvr(wc~spec1, data = data, 
                 method = "oscorespls",
                 validation = "LOO",
                 ncomp = 20, 
                 center = TRUE,
                 jackknife = TRUE,
                 model = TRUE)


#hand pick PCs to tune to based on validation plot
validationplot(fullmodel)

bdoPC <-7
gPC  <- 10
xPC <- 7
aPC <- 5
gyPC <- 7


# Visualize model details

# save coefficients as a separate data frame for plotting

coefficients <- data.frame(
  Constituent = "BDO",
  wavenumber = as.numeric(rownames(jack.test(fullmodel, ncomp = 1)$coefficients)),
  regcoef = jack.test(fullmodel, ncomp = bdoPC)$coefficients[,1,1],
  sd_regcoef =  jack.test(fullmodel, ncomp = bdoPC)$sd[,1,1]) %>% 
  rbind(
    data.frame(
      Constituent = "Glucose",
      wavenumber = as.numeric(rownames(jack.test(fullmodel, ncomp = 1)$coefficients)),
      regcoef = jack.test(fullmodel, ncomp = gPC)$coefficients[,2,1],
      sd_regcoef =  jack.test(fullmodel, ncomp = gPC)$sd[,2,1]) 
  ) %>% 
  rbind(
    data.frame(
      Constituent = "Xylose",
      wavenumber = as.numeric(rownames(jack.test(fullmodel, ncomp = 1)$coefficients)),
      regcoef = jack.test(fullmodel, ncomp = xPC)$coefficients[,3,1],
      sd_regcoef =  jack.test(fullmodel, ncomp = xPC)$sd[,3,1]) 
  ) %>% 
  rbind(
    data.frame(
      Constituent = "Acetoin",
      wavenumber = as.numeric(rownames(jack.test(fullmodel, ncomp = 1)$coefficients)),
      regcoef = jack.test(fullmodel, ncomp = aPC)$coefficients[,4,1],
      sd_regcoef =  jack.test(fullmodel, ncomp = aPC)$sd[,4,1]) 
  )%>% 
  rbind(
    data.frame(
      Constituent = "Glycerol",
      wavenumber = as.numeric(rownames(jack.test(fullmodel, ncomp = 1)$coefficients)),
      regcoef = jack.test(fullmodel, ncomp = gyPC)$coefficients[,5,1],
      sd_regcoef =  jack.test(fullmodel, ncomp = aPC)$sd[,5,1]) 
  )


# visualize model coefficients

coefficients %>% 
  mutate(Constituent = factor(Constituent, levels = c("Glucose","Xylose","BDO", "Acetoin","Glycerol"))) %>% 
  ggplot(aes(x=wavenumber, y = regcoef, col = Constituent))+geom_line(size = 1)+geom_hline(yintercept = 0, col = "grey")+
  theme_bw()+
  geom_errorbar(alpha = .8,
                aes(x=wavenumber, 
                    ymin= regcoef -sd_regcoef,
                    ymax =regcoef +sd_regcoef ,
                    col="mean +/- 1 SD"))+
  labs(y = "Regression Coefficients", x = "Wavenumber (cm^-1)")+
  theme(legend.position ="none")+
  scale_x_reverse()+
  facet_wrap(~Constituent, nrow = 5)+
  scale_color_manual(name = "Constituent", values = c("#feb24c","#f03b20","#7570b3","#99d594","#3288bd", "pink") )


# visualize model loadings

 a <- data.frame(
  PC1 = fullmodel$loadings[,1],
  PC2 = fullmodel$loadings[,2],
  wavenumber = as.numeric(rownames(fullmodel$loadings))) %>% 
  ggplot(aes(x=wavenumber, y = PC1,col = "PC1"))+geom_line(linewidth = 1, alpha = .8)+geom_line(linewidth = 1, alpha = .8,aes(y=PC2, col = "PC2"))+
   theme_bw()+
   scale_x_reverse()+
   geom_hline(yintercept = 0, col = "grey")+
   labs(x="wavenumber (cm^-1)", y = "Model Loadings")+
   scale_color_manual(values = c("darkblue","darkred"))


a

# plot transformed spectra combinational region to compare to model loadings

b<- data %>% 
   pivot_longer(all_of(speccols),
                names_to = "wl", values_to = "abs") %>% 
  group_by(wl) %>% 
  summarize(abs = mean(abs)) %>% 
   mutate(wl = as.numeric(wl)) %>%
   ggplot(aes(x=wl, y = abs))+geom_line(size = 1)+
  theme_bw()+scale_x_reverse()+
  labs(x="wavelength (cm^-1)",
       y= "Average Log(1/R)")+
  xlim(6600,4000)
 b/a
 

# Pred vs Measured Plot (LOOCV)
 
data<- data%>% 
   mutate(bdo_cv = fullmodel$validation$pred[,1,bdoPC],
          bdo_cal = predict(fullmodel, data)[,1,bdoPC],
          glucose_cv = fullmodel$validation$pred[,2,gPC],
          glucose_cal = predict(fullmodel, data)[,2,gPC],
          xylose_cv = fullmodel$validation$pred[,3,xPC],
          xylose_cal = predict(fullmodel,data)[,3,xPC],
          acetoin_cv = fullmodel$validation$pred[,4,aPC],
          acetoin_cal = predict(fullmodel, data)[,4,aPC],
          glycerol_cv = fullmodel$validation$pred[,5,gyPC],
          glycerol_cal = predict(fullmodel,data)[,5,gyPC]) 

# save resulting predictions

saveRDS(data, "../data/processed/laboratoryatline_mocksample_modelresults.RDS")

# visualize model scores of first 2 PCs

data %>% 
  mutate(
    PC1 = fullmodel$scores[,1],
    PC2 = fullmodel$scores[,2]
) %>% 
  ggplot(aes(x=PC1, y=PC2, col = totalBDO_gperL))+geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+theme_bw()


# visualize correlations between constituents 

data %>% select(glucose_gperL, xylose_gperL, totalBDO_gperL, acetoin_gperL, totalBDO_gperL) %>% 
ggpairs()+
  theme_bw()+
  theme(legend.text = element_text(size = 8),
                legend.title = element_text(size = 8),
                strip.text= element_text(colour = "black", size =9, face =2, hjust = 0),
                strip.background = element_blank(),
                legend.key.height = unit(1,"mm"),
                title = element_text(colour = "black", size =9.5, face =2, hjust = 0.5),
                panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"),
                panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))


