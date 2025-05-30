# Code to generate Table S2- correlation matrices for each dataset 

# NOTE: The function to produce the correlation matrix was pulled from: https://www.r-bloggers.com/2020/07/create-a-publication-ready-correlation-matrix-with-significance-levels-in-r/

# load packages

library(openxlsx)
library(Hmisc)

# define correlation matrix function 
correlation_matrix <- function(df, 
                               type = "pearson",
                               digits = 3, 
                               decimal.mark = ".",
                               use = "all", 
                               show_significance = TRUE, 
                               replace_diagonal = FALSE, 
                               replacement = ""){
  
  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)
  
  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]
  
  # transform input data frame to matrix
  x <- as.matrix(df)
  
  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)
  
  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0) > 0) {
    Rformatted = ifelse(R > 0, paste0(' ', Rformatted), Rformatted)
  }
  
  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    Rformatted = paste0(Rformatted, stars)
  }
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep =" ")
  
  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }
  
  return(Rnew)
}

# load raw data and filter out contaminated samples as well as samples that were only run on glucose (as was done in model building)

thermoatline <-read.xlsx("../data/raw/laboratoryatline_ringcup_fulldata.xlsx") %>% 
  filter(lacticAcid_gperL < 2.5) %>% 
  filter(aceticAcid_gperL <2.5) %>% 
  filter(ethanol_gperL < 2.5)%>% 
  filter(xylose_gperL != 0) %>% 
  select(glucose_gperL, xylose_gperL, totalBDO_gperL, acetoin_gperL, glycerol_gperL)

thermoonline <-read.xlsx("../data/raw/laboratoryonline_fulldata.xlsx") %>% 
  filter(lacticAcid_gperL < 2.5) %>% 
  filter(aceticAcid_gperL <2.5) %>% 
  filter(ethanol_gperL < 2.5)%>% 
  filter(xylose_gperL != 0) %>% 
  select(glucose_gperL, xylose_gperL, totalBDO_gperL, acetoin_gperL, glycerol_gperL)

nirone <- read.xlsx("../data/raw/lowcostatline_ringcup_fulldata.xlsx") %>% 
  filter(lacticAcid_gperL < 2.5) %>% 
  filter(aceticAcid_gperL <2.5) %>% 
  filter(ethanol_gperL < 2.5)%>% 
  filter(xylose_gperL != 0) %>% 
  select(glucose_gperL, xylose_gperL, totalBDO_gperL, acetoin_gperL, glycerol_gperL)

# produce correlation matrix for each dataset

tatline <- correlation_matrix(thermoatline, use = "upper") %>% as.data.frame()

tonline <- correlation_matrix(thermoonline, use = "upper")%>% as.data.frame()

nironeatline <- correlation_matrix(nirone, use = "upper")%>% as.data.frame()

# save correlation matrices into an excel spreadsheet, first producing the sheets and then appending them with the correlation matrix

corrs <- createWorkbook()

addWorksheet(corrs, sheetName = "Laboratory Grade At-Line")

addWorksheet(corrs, sheetName = "Laboratory Grade On-Line")

addWorksheet(corrs, sheetName = "Low Cost Grade At-Line")

writeDataTable(corrs, sheet = 1, tatline)

writeDataTable(corrs, sheet = 2, tonline)

writeDataTable(corrs, sheet = 3, nironeatline)

# save the workbook

saveWorkbook(corrs, "../tables/TableS2_correlationmatrixes.xlsx")


