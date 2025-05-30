# building lab grade at line low cost feasibility model 

# load packages

library(tidyverse)
library(pls)
library(prospectr)
library(openxlsx)

# source spectral processing script

source("./spectraprocessingfunctions.R")

# load data

data <- read.xlsx("../data/raw/laboratoryatline_ringcup_fulldata.xlsx")

# separate out the independent validation experiment

indval <- data %>% filter(experiment == "RN E8") %>% filter(fermenter == "F1")

# update data to remove ind val 
data <- data %>% filter(!(sample %in% indval$sample))

# remove contaminated samples
data <- data %>% filter(lacticAcid_gperL < 2.5) %>% 
                  filter(aceticAcid_gperL <2.5) %>% 
                  filter(ethanol_gperL < 2.5) 

# separate out glucose only samples

glucoseonly <- data %>% filter(xylose_gperL == 0)
data <- data %>% filter(!xylose_gperL == 0)

# process spectra, save as a matrix 

data$spec1 <- process_spectra_A_nironetest(data) 
indval$spec1 <- process_spectra_A_nironetest(indval)
glucoseonly$spec1 <- process_spectra_A_nironetest(glucoseonly)

# build wc validation matrix

wc <- data.frame(bdo = data$totalBDO_gperL, 
                 glucose = data$glucose_gperL, 
                 xylose = data$xylose_gperL,
                 acetoin = data$acetoin_gperL,
                 glycerol = data$glycerol_gperL) %>% as.matrix()
data$wc <- wc

# use ken stone algorithm to pick test and training groups, removing spectral redundancy

ks <- kenStone(data$spec1, k = round(.6*dim(data)[1],0), pc = .95)

training <- data[ks$model,]
testing <- data[ks$test,]

# build the model

fullmodel <- mvr(wc~spec1, data = training, 
                 method = "oscorespls",
                 validation = "LOO",
                 ncomp = 20, 
                 center = TRUE,
                 jackknife = TRUE,
                 model = TRUE)

# hand pick PCs to tune to based on validation plot (elbow method/lowest CV combination)
validationplot(fullmodel)

bdoPC <- 6
gPC  <- 7
xPC <- 12
aPC <- 10
gyPC <- 10
PCs <- data.frame(
  BDO = bdoPC, Glucose= gPC, Xylose = xPC, Acetoin = aPC, Glycerol = gyPC)

saveRDS(PCs, "../data/processed/dataframes/laboratoryatline_nironefeasibility_PCs.RDS")

# save resulting predictions, post processing negative predictions as zero

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
  mutate(bdo_cv = ifelse(bdo_cv <0, 0,bdo_cv)) %>% 
  mutate(glucose_cv = ifelse(glucose_cv <0, 0,glucose_cv)) %>% 
  mutate(xylose_cv = ifelse(xylose_cv <0, 0,xylose_cv)) %>% 
  mutate(acetoin_cv = ifelse(acetoin_cv <0, 0,acetoin_cv)) %>% 
  mutate(glycerol_cv = ifelse(glycerol_cv <0, 0,glycerol_cv) )%>% 
  mutate(bdo_cal = ifelse(bdo_cal <0, 0,bdo_cal)) %>% 
  mutate(glucose_cal = ifelse(glucose_cal <0, 0,glucose_cal)) %>% 
  mutate(xylose_cal = ifelse(xylose_cal <0, 0,xylose_cal)) %>% 
  mutate(acetoin_cal = ifelse(acetoin_cal <0, 0,acetoin_cal)) %>% 
  mutate(glycerol_cal = ifelse(glycerol_cal <0, 0,glycerol_cal) )%>% 
  mutate(model = "Laboratory at line Low Cost Range Test")

testing <- testing %>% 
  mutate(bdo_pred = predict(fullmodel, testing)[,1,bdoPC],
         glucose_pred  = predict(fullmodel, testing)[,2,gPC],
         xylose_pred = predict(fullmodel, testing)[,3,xPC],
         acetoin_pred = predict(fullmodel, testing)[,4,aPC],
         glycerol_pred  = predict(fullmodel, testing)[,5,gyPC]) %>% 
  mutate(bdo_pred = ifelse(bdo_pred <0, 0,bdo_pred)) %>% 
  mutate(glucose_pred = ifelse(glucose_pred <0, 0,glucose_pred)) %>% 
  mutate(xylose_pred = ifelse(xylose_pred <0, 0,xylose_pred)) %>% 
  mutate(acetoin_pred= ifelse(acetoin_pred <0, 0,acetoin_pred)) %>% 
  mutate(glycerol_pred= ifelse(glycerol_pred <0, 0,glycerol_pred)) %>% 
  mutate(model = "Laboratory at line Low Cost Range Test")


indval <- indval  %>% 
  mutate(bdo_pred = predict(fullmodel, indval )[,1,bdoPC],
         glucose_pred  = predict(fullmodel, indval )[,2,gPC],
         xylose_pred = predict(fullmodel, indval )[,3,xPC],
         acetoin_pred = predict(fullmodel, indval )[,4,aPC],
         glycerol_pred  = predict(fullmodel, indval )[,5,gyPC]) %>% 
  mutate(bdo_pred = ifelse(bdo_pred <0, 0,bdo_pred)) %>% 
  mutate(glucose_pred = ifelse(glucose_pred <0, 0,glucose_pred)) %>% 
  mutate(xylose_pred = ifelse(xylose_pred <0, 0,xylose_pred)) %>% 
  mutate(acetoin_pred= ifelse(acetoin_pred <0, 0,acetoin_pred)) %>% 
  mutate(glycerol_pred= ifelse(glycerol_pred <0, 0,glycerol_pred)) %>% 
  mutate(model = "Laboratory at line Low Cost Range Test")

glucoseonly <- glucoseonly %>% 
  mutate(bdo_pred = predict(fullmodel, glucoseonly )[,1,bdoPC],
         glucose_pred  = predict(fullmodel, glucoseonly )[,2,gPC],
         xylose_pred = predict(fullmodel, glucoseonly)[,3,xPC],
         acetoin_pred = predict(fullmodel, glucoseonly )[,4,aPC],
         glycerol_pred  = predict(fullmodel, glucoseonly )[,5,gyPC])%>% 
  mutate(bdo_pred = ifelse(bdo_pred <0, 0,bdo_pred)) %>% 
  mutate(acetoin_pred= ifelse(acetoin_pred <0, 0,acetoin_pred)) %>% 
  mutate(glycerol_pred= ifelse(glycerol_pred <0, 0,glycerol_pred)) %>% 
  mutate(model = "Laboratory At-Line Low Cost Range Test")

# save dataframes and models

saveRDS(fullmodel, "../data/processed/models/laboratoryatline_lowcostfeasibility_fullmodel.RDS")
saveRDS(training, "../data/processed/dataframes/laboratoryatline_lowcostfeasibility_training.RDS")
saveRDS(testing, "../data/processed/dataframes/laboratoryatline_lowcostfeasibility_testing.RDS")
saveRDS(indval, "../data/processed/dataframes/laboratoryatline_lowcostfeasibility_indval.RDS")
saveRDS(glucoseonly, "../data/processed/dataframes/laboratoryatline_lowcostfeasibility_glucoseonly.RDS")
