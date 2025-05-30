# building the lab grade online model 

# load packages

library(tidyverse)
library(pls)
library(prospectr)
library(openxlsx)

# source signal processing functions
source("./spectraprocessingfunctions.R")

# load data

data <- read.xlsx("../data/raw/laboratoryonline_fulldata.xlsx")

# create vector with names of spectra specific columns

speccols <- colnames(data)[31:1458]

# make sure that all spectra are being read in as numeric values

data <- data %>% 
  mutate(across(all_of(speccols), as.numeric))

# filter out data without HPLC 

spectraonly <- data %>% filter(is.na(glucose_gperL))
data <- data %>% filter(!is.na(glucose_gperL))

# separate out the independent validation experiments 

# unique experiment validation is RN E8

indval <- data %>% filter(experiment == "RN E8")
data <- data %>% filter(!(experiment == "RN E8"))

# process limits evaluation - BDO Record Experiment

limits <- data %>% filter(experiment == "max bdo")
data <- data %>% filter(!(experiment == "max bdo"))

# scale up validation are the 2 labeled 10L

scaleupval <- data %>%filter((experiment == "cr exp 2 (10 L)") | (experiment ==  "BSRD 10L PS HR"))
data <- data %>%filter(!(experiment == "cr exp 2 (10 L)") & !(experiment ==  "BSRD 10L PS HR"))


# remove contaminated samples

data <- data %>% filter(lacticAcid_gperL < 2.5) %>% 
  filter(aceticAcid_gperL <2.5) %>% 
  filter(ethanol_gperL < 2.5) 


# process spectra, save as a matrix 

data$spec1 <- process_spectra_thermoB(data) 
indval$spec1 <- process_spectra_thermoB(indval)
limits$spec1 <- process_spectra_thermoB(limits)
scaleupval$spec1 <- process_spectra_thermoB(scaleupval)
spectraonly$spec1 <- process_spectra_thermoB(spectraonly)

# build wc validation matrix

wc <- data.frame(bdo = data$totalBDO_gperL, 
                 glucose = data$glucose_gperL,
                 xylose = data$xylose_gperL,
                 acetoin = data$acetoin_gperL,
                 glycerol = data$glycerol_gperL) %>% as.matrix()
data$wc <- wc

# use ken stone algorithm to pick test and training groups, removing spectral redundancy

ks <- kenStone(data$spec1, k = round(.80*dim(data)[1],0), pc = .95)

training <- data[ks$model,]
testing <- data[ks$test,]

# build the model

fullmodel <- mvr(wc~spec1, data = training, 
                 method = "oscorespls",
                 validation = "LOO",
                 ncomp = 20, 
                 center = TRUE,
                 jackknife = TRUE,
                 x=TRUE,
                 y=TRUE)

# pick PCs to tune to based on validation plot

validationplot(fullmodel)

bdoPC <- 10
gPC  <- 8
xPC <-15
aPC <- 11
gyPC <- 13


PCs <- data.frame(
  BDO = bdoPC, Glucose= gPC, Xylose = xPC, Acetoin = aPC, Glycerol = gyPC)

saveRDS(PCs, "../data/processed/dataframes/laboratoryonline_probe_PCs.RDS")

# save resulting prediction, reclassifying negative predictions as zero 

# calibration and cross validation 

training <- training %>% 
  mutate(bdo_cv = fullmodel$validation$pred[,1,bdoPC],
         bdo_cal = predict(fullmodel, training)[,1,bdoPC],
         glucose_cv = fullmodel$validation$pred[,2,gPC],
         glucose_cal = predict(fullmodel, training)[,2,gPC],
         xylose_cv = fullmodel$validation$pred[,3,xPC],
         xylose_cal = predict(fullmodel, training)[,3,xPC],
         acetoin_cv = fullmodel$validation$pred[,4,aPC],
         acetoin_cal = predict(fullmodel, training)[,4,aPC],
         glycerol_cv = fullmodel$validation$pred[,5,gyPC],
         glycerol_cal = predict(fullmodel, training)[,5,gyPC]) %>% 
  mutate(glucose_cv= ifelse(glucose_cv <0, 0, glucose_cv)) %>% 
  mutate(xylose_cv = ifelse(xylose_cv <0, 0, xylose_cv))%>% 
  mutate(xylose_cal = ifelse(glucose_cal <0, xylose_cal + glucose_cal, xylose_cal)) %>% 
  mutate(glucose_cal= ifelse(glucose_cal <0, 0, glucose_cal)) %>% 
  mutate(xylose_cal = ifelse(xylose_cal <0, 0, xylose_cal))  %>%   
  mutate(bdo_cal = ifelse(bdo_cal <0, 0,bdo_cal)) %>% 
  mutate(acetoin_cal= ifelse(acetoin_cal <0, 0,acetoin_cal)) %>% 
  mutate(glycerol_cal= ifelse(glycerol_cal <0, 0,glycerol_cal)) %>%   
  mutate(bdo_cv = ifelse(bdo_cv <0, 0,bdo_cv)) %>% 
  mutate(acetoin_cv= ifelse(acetoin_cv <0, 0,acetoin_cv)) %>% 
  mutate(glycerol_cv= ifelse(glycerol_cv <0, 0,glycerol_cv)) %>% 
  mutate(model = "Laboratory on line probe")

# independent validation 

testing <- testing %>% cbind(data.frame(glucose_pred = predict(fullmodel, testing$spec1)[,2,gPC],
           xylose_pred = predict(fullmodel, testing$spec1)[,3,xPC],
           bdo_pred = predict(fullmodel, testing$spec1)[,1,bdoPC],
           acetoin_pred = predict(fullmodel, testing$spec1)[,4,aPC],
           glycerol_pred = predict(fullmodel, testing$spec1)[,5,gyPC]
           )) %>% 
  mutate(glucose_pred= ifelse(glucose_pred <0, 0, glucose_pred)) %>% 
  mutate(xylose_pred = ifelse(xylose_pred <0, 0, xylose_pred))  %>%   
  mutate(bdo_pred = ifelse(bdo_pred <0, 0,bdo_pred)) %>% 
  mutate(acetoin_pred= ifelse(acetoin_pred <0, 0,acetoin_pred)) %>% 
  mutate(glycerol_pred= ifelse(glycerol_pred <0, 0,glycerol_pred)) %>% 
  mutate(model = "Laboratory on line probe")

# traditional within spec unique experiment validation

indval <- indval%>% cbind(data.frame(glucose_pred = predict(fullmodel, indval$spec1)[,2,gPC],
                                       xylose_pred = predict(fullmodel, indval$spec1)[,3,xPC],
                                       bdo_pred = predict(fullmodel, indval$spec1)[,1,bdoPC],
                                       acetoin_pred = predict(fullmodel, indval$spec1)[,4,aPC],
                                       glycerol_pred = predict(fullmodel, indval$spec1)[,5,gyPC])) %>% 
  mutate(glucose_pred= ifelse(glucose_pred <0, 0, glucose_pred)) %>% 
  mutate(xylose_pred = ifelse(xylose_pred <0, 0, xylose_pred)) %>%   
  mutate(bdo_pred = ifelse(bdo_pred <0, 0,bdo_pred)) %>% 
  mutate(acetoin_pred= ifelse(acetoin_pred <0, 0,acetoin_pred)) %>% 
  mutate(glycerol_pred= ifelse(glycerol_pred <0, 0,glycerol_pred)) %>% 
  mutate(model = "Laboratory on line probe")

# process limits oos unique experiment validation 

limits <- limits%>% cbind(data.frame(glucose_pred = predict(fullmodel, limits$spec1)[,2,gPC],
                                     xylose_pred = predict(fullmodel, limits$spec1)[,3,xPC],
                                     bdo_pred = predict(fullmodel, limits$spec1)[,1,bdoPC],
                                     acetoin_pred = predict(fullmodel, limits$spec1)[,4,aPC],
                                     glycerol_pred = predict(fullmodel, limits$spec1)[,5,gyPC])) %>% 
  mutate(glucose_pred= ifelse(glucose_pred <0, 0, glucose_pred)) %>% 
  mutate(xylose_pred = ifelse(xylose_pred <0, 0, xylose_pred)) %>%   
  mutate(bdo_pred = ifelse(bdo_pred <0, 0,bdo_pred)) %>% 
  mutate(acetoin_pred= ifelse(acetoin_pred <0, 0,acetoin_pred)) %>% 
  mutate(glycerol_pred= ifelse(glycerol_pred <0, 0,glycerol_pred)) %>% 
  mutate(model = "Laboratory on line probe")

# process scale up oos unique experiment validation 

scaleupval <- scaleupval%>% cbind(data.frame(glucose_pred = predict(fullmodel, scaleupval$spec1)[,2,gPC],
                                     xylose_pred = predict(fullmodel, scaleupval$spec1)[,3,xPC],
                                     bdo_pred = predict(fullmodel, scaleupval$spec1)[,1,bdoPC],
                                     acetoin_pred = predict(fullmodel, scaleupval$spec1)[,4,aPC],
                                     glycerol_pred = predict(fullmodel, scaleupval$spec1)[,5,gyPC])) %>% 
  mutate(glucose_pred= ifelse(glucose_pred <0, 0, glucose_pred)) %>% 
  mutate(xylose_pred = ifelse(xylose_pred <0, 0, xylose_pred)) %>%   
  mutate(bdo_pred = ifelse(bdo_pred <0, 0,bdo_pred)) %>% 
  mutate(acetoin_pred= ifelse(acetoin_pred <0, 0,acetoin_pred)) %>% 
  mutate(glycerol_pred= ifelse(glycerol_pred <0, 0,glycerol_pred)) %>% 
  mutate(model = "Laboratory on line probe")


# predictions for on-line work without HPLC data

spectraonly <- spectraonly%>% cbind(data.frame(glucose_pred = predict(fullmodel, spectraonly$spec1)[,2,gPC],
                                                       xylose_pred = predict(fullmodel, spectraonly$spec1)[,3,xPC],
                                                       bdo_pred = predict(fullmodel, spectraonly$spec1)[,1,bdoPC],
                                                       acetoin_pred = predict(fullmodel, spectraonly$spec1)[,4,aPC],
                                                       glycerol_pred = predict(fullmodel, spectraonly$spec1)[,5,gyPC]
                                               ))  %>% 
  mutate(glucose_pred= ifelse(glucose_pred <0, 0, glucose_pred)) %>% 
  mutate(xylose_pred = ifelse(xylose_pred <0, 0, xylose_pred))  %>%   
  mutate(bdo_pred = ifelse(bdo_pred <0, 0,bdo_pred)) %>% 
  mutate(acetoin_pred= ifelse(acetoin_pred <0, 0,acetoin_pred)) %>% 
  mutate(glycerol_pred= ifelse(glycerol_pred <0, 0,glycerol_pred)) %>% 
  mutate(model = "Laboratory on line probe")



#save dataframes and models

saveRDS(fullmodel, "../data/processed/models/laboratoryonline_probe_fullmodel.RDS")
saveRDS(training, "../data/processed/dataframes/laboratoryonline_probe_training.RDS")
saveRDS(testing, "../data/processed/dataframes/laboratoryonline_probe_testing.RDS")
saveRDS(indval, "../data/processed/dataframes/laboratoryonline_probe_indval.RDS")
saveRDS(limits, "../data/processed/dataframes/laboratoryonline_probe_limits.RDS")
saveRDS(scaleupval, "../data/processed/dataframes/laboratoryonline_probe_scaleupval.RDS")
saveRDS(spectraonly, "../data/processed/dataframes/laboratoryonline_probe_spectraonly.RDS")


