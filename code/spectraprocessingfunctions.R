# preprocessing functions for each dataset

# lab grate at line - full range

process_spectra_A_full <- function(dataframe, theDiff=1, thePoly =2, theWindow = 7, lowlimit = "3999.639893", hilimit = "9503.484375"){
  speccols <- colnames(dataframe)[which(colnames(dataframe)== lowlimit):which(colnames(dataframe)==hilimit)]
  spectra <- dataframe %>% select(all_of(speccols))
  speccols_t <- speccols[speccols < 9000]
  speccols_t <- speccols_t[speccols_t >4000]
  spectra <- spectra %>% select(any_of(speccols_t))
  spec_m <- spectra %>% as.matrix()
  spec_snv <- spec_m %>% prospectr::standardNormalVariate() %>% data.frame()
  colnames(spec_snv)<- substring(colnames(spec_snv), 2,)
  colnames(spec_snv) <- colnames(spec_snv)  %>% as.character()
  cols <- colnames(spec_snv) 
  spec_snvsg <- spec_m %>% prospectr::savitzkyGolay( p = thePoly, m = theDiff, w = theWindow) %>% as.data.frame()
  colnames(spec_snvsg)<- colnames(spec_snvsg) %>% as.character()
  no6100 <- cols[cols<(10000000/1520)] %>% as.character()
  
  spec_snvsg_no6100<- spec_snvsg %>% select(any_of(no6100)) 
  spec_snvsg_no6100 <- spec_snvsg_no6100 %>% as.matrix()
  
  return(spec_snvsg_no6100)
}

# lab grade at line low cost range

process_spectra_A_nironetest <- function(dataframe, theDiff=1, thePoly =2, theWindow = 7, lowlimit = "3999.639893", hilimit = "9503.484375"){
  speccols <- colnames(dataframe)[which(colnames(dataframe)== lowlimit):which(colnames(dataframe)==hilimit)]
  spectra <- dataframe %>% select(all_of(speccols))
  speccols_t <- speccols[speccols < 9000]
  speccols_t <- speccols_t[speccols_t >4000]
  spectra <- spectra %>% select(any_of(speccols_t))
  spec_m <- spectra %>% as.matrix()
  spec_snv <- spec_m %>% prospectr::standardNormalVariate() %>% data.frame()
  colnames(spec_snv)<- substring(colnames(spec_snv), 2,)
  spec_snvsg <- spec_m %>% prospectr::savitzkyGolay( p = thePoly, m = theDiff, w = theWindow) %>% as.data.frame()
  speccols <- colnames(spec_snvsg) %>% as.numeric()
  nirone <- speccols[speccols < 10000000/2000] 
  nirone <- nirone[nirone > 10000000/2450] %>%  as.character()
  spec_snvsg_nirone <- spec_snvsg %>% select(any_of(nirone)) 
  spec_snvsg_nirone  <- spec_snvsg_nirone  %>% as.matrix()
  return(spec_snvsg_nirone)
}

# low cost at line

process_spectra_nirone <- function(data, theDiff=1, thePoly =2, theWindow = 17, lowlimit = "2000", hilimit = "2450"){
  speccols <- colnames(data)[which(colnames(data)=="2000"):which(colnames(data)=="2450")]
  spectra <- data %>% select(any_of(speccols))
  spec_m <- spectra %>% as.matrix()
  spec_snv <- spec_m %>% prospectr::standardNormalVariate() %>% data.frame()
  colnames(spec_snv)<- substring(colnames(spec_snv), 2,)
  spec_sg <- spec_snv %>% prospectr::savitzkyGolay( p = thePoly, m = theDiff, w = theWindow)
  spec_snvsg <- spec_sg %>% as.matrix()
  return(spec_snvsg)
}

# lab grade on line

process_spectra_thermoB <- function(data, theDiff=1, thePoly =2, theWindow = 11, lowlimit = "3999.639893", hilimit = "9503.484375"){
  spec <- data 
  speccols <- colnames(spec)[which(colnames(spec)== lowlimit):which(colnames(spec)==hilimit)]
  spec <- spec %>% select(all_of(speccols)) 
  speccols_t <- speccols[speccols < 9000]
  speccols_t <- speccols_t[speccols_t >4000]
  spec <- spec %>% select(any_of(speccols_t))
  spec_SNV_SG <- spec %>% 
    prospectr::standardNormalVariate() %>%
    prospectr::savitzkyGolay(m = theDiff, p = thePoly, w = theWindow) %>% data.frame() 
  colnames(spec_SNV_SG) <- as.numeric(substring(colnames(spec_SNV_SG),2)) 
  cols <- as.numeric(colnames(spec_SNV_SG))
  cols_t <- cols[cols < as.numeric(10000000/1520)] 
  cols_keep <- cols_t[which(cols_t > as.numeric(10000000/1870))] %>% as.character()
  cols_keep2 <- cols_t[which(cols_t < as.numeric(4700))] 
  cols_keep2 <- cols_keep2[which(cols_keep2 > as.numeric(4377))] 
  collls <-c(cols_keep, cols_keep2) %>% as.numeric() 
  spec_SNV_SG_t <- spec_SNV_SG[,colnames(spec_SNV_SG)%in% collls] %>% as.matrix()
  return(spec_SNV_SG_t)
}

