nirbdoms
================
Zofia Tillman
2025-05-28

Code used to run analysis and create visualizations/tables for
‘Rapid monitoring of fermentations – a feasibility study on biological
2,3-butanediol production’ 

Tillman, Z., Peterson, D.J., Dowe, N. et al. Rapid monitoring of fermentations: a feasibility study on biological 2,3-butanediol production. Biotechnol. Biofuels Bioprod. 18, 60 (2025).

## How to Reproduce Analysis

1.  Clone Repository

2.  Download the research data used to perform this analysis from
    data.nrel.gov - Data for “Rapid monitoring of fermentations- a
    feasibility study of biological 2,3-butanediol production” and save
    data to data/raw folder within repository

3.  Produce Models by working through the following scripts:

- laboratoryatline_full_modelbuild.R (lab grade full models)
- laboratoryatline_lowcostfeasibility_modelbuild.R (lab grade low cost
  feasibility models)
- laboratoryatline_onlinefeasibility_modelbuild.R (lab grade on line
  feasibility models)
- lowcostatline_modelbuild.R (low cost at line models)
- laboratoryonline_modelbuild.R (laboratory grade on line models)



4.  Produce Figures and Tables describing the models by working through
    the following scripts:

- figure\_(1-S11).R code used to generate each figure in the manuscript
- table\_(S1-5).R code used to generate each table in the manuscript

## Dependencies

- R version 4.4.2 (2024-10-31 ucrt)
  - `tidyverse` (v. 2.0.0)
  - `pls` (v. 2.8.5)
  - `prospectr` (v. 0.2.7)
  - `ggpmisc` (v. 0.6.1)
  - `openxlsx` (v. 4.2.8)
  - `egg` (v. 0.4.5)
  - `ggsci` (v. 3.2.0)
  - `caret` (v. 7.0.1)
  - `ggtext` (v. 0.1.2)
  - `GGally` (v. 2.2.1)
  - `patchwork` (v. 1.3.0)
  - `Hmisc` (v. 5.2.2)
  - `rmarkdown` (v. 2.29)

## My Computer

    ## R version 4.4.2 (2024-10-31 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 11 x64 (build 22631)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## time zone: America/Denver
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] Hmisc_5.2-2     patchwork_1.3.0 GGally_2.2.1    ggtext_0.1.2   
    ##  [5] caret_7.0-1     lattice_0.22-6  ggsci_3.2.0     egg_0.4.5      
    ##  [9] gridExtra_2.3   openxlsx_4.2.8  ggpmisc_0.6.1   ggpp_0.5.8-1   
    ## [13] prospectr_0.2.7 pls_2.8-5       lubridate_1.9.4 forcats_1.0.0  
    ## [17] stringr_1.5.1   dplyr_1.1.4     purrr_1.0.2     readr_2.1.5    
    ## [21] tidyr_1.3.1     tibble_3.2.1    ggplot2_3.5.1   tidyverse_2.0.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] polynom_1.4-1        pROC_1.18.5          rlang_1.1.5         
    ##  [4] magrittr_2.0.3       compiler_4.4.2       vctrs_0.6.5         
    ##  [7] reshape2_1.4.4       quantreg_6.00        pkgconfig_2.0.3     
    ## [10] fastmap_1.2.0        backports_1.5.0      rmarkdown_2.29      
    ## [13] prodlim_2024.06.25   tzdb_0.4.0           MatrixModels_0.5-3  
    ## [16] xfun_0.50            recipes_1.1.0        parallel_4.4.2      
    ## [19] cluster_2.1.6        R6_2.5.1             stringi_1.8.4       
    ## [22] RColorBrewer_1.1-3   parallelly_1.41.0    rpart_4.1.23        
    ## [25] Rcpp_1.0.14          iterators_1.0.14     knitr_1.49          
    ## [28] future.apply_1.11.3  base64enc_0.1-3      Matrix_1.7-1        
    ## [31] splines_4.4.2        nnet_7.3-19          timechange_0.3.0    
    ## [34] tidyselect_1.2.1     rstudioapi_0.17.1    yaml_2.3.10         
    ## [37] timeDate_4041.110    codetools_0.2-20     listenv_0.9.1       
    ## [40] plyr_1.8.9           withr_3.0.2          evaluate_1.0.3      
    ## [43] foreign_0.8-87       future_1.34.0        survival_3.7-0      
    ## [46] ggstats_0.8.0        zip_2.3.1            xml2_1.3.6          
    ## [49] pillar_1.10.1        checkmate_2.3.2      foreach_1.5.2       
    ## [52] stats4_4.4.2         generics_0.1.3       mathjaxr_1.6-0      
    ## [55] hms_1.1.3            munsell_0.5.1        scales_1.3.0        
    ## [58] globals_0.16.3       class_7.3-22         glue_1.8.0          
    ## [61] tools_4.4.2          data.table_1.16.4    SparseM_1.84-2      
    ## [64] ModelMetrics_1.2.2.2 gower_1.0.2          grid_4.4.2          
    ## [67] ipred_0.9-15         colorspace_2.1-1     nlme_3.1-166        
    ## [70] htmlTable_2.4.3      Formula_1.2-5        cli_3.6.3           
    ## [73] lava_1.8.1           gtable_0.3.6         digest_0.6.37       
    ## [76] htmlwidgets_1.6.4    farver_2.1.2         htmltools_0.5.8.1   
    ## [79] lifecycle_1.0.4      hardhat_1.4.0        gridtext_0.1.5      
    ## [82] MASS_7.3-61
