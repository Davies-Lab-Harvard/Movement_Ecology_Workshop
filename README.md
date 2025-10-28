# Movement_Ecology_Workshop

## Useful Links:
[Getting started in amt](https://cran.r-project.org/web/packages/amt/vignettes/p1_getting_started.html)

[iSSA Tutorial](https://www.youtube.com/watch?v=jiY9N-TNRjs)

## Step 1: Load data and prepare a data frame for iSSA

`Step 1 - Data Prep.R`: R script that shows how to make a "track" object, standardize the temporal sampling rate of the track, generate random steps, and extract covariates at the end of each used and random movement step.

`white-thighed_hornbill_data.csv`: CSV containing GPS locations of five white-thighed hornbills (3 female, 2 male) tracked in the Dja Faunal Reserve of Cameroon. Explanation of column names:
    - `tag.local.identifier`: Serial number of the GPS tag (e-obs 27 g solar-powered tag)
    - `study.local.timestamp`: Timestamp recorded for each GPS location in the format MM/DD/YYYY HH:MM:SS
    - `utm.easting`: UTM Easting
    - `utm.northing`: UTM Northing
    - `utm.zone: UTM zone
    - `study.timezone`: timezone of the timestamps
    - `Sex`: sex of the individual

- `ch.tif`: Canopy Height, 10 m resolution

- `vc.tif`: Vertical Complexity Index, 10 m resolution

- `d50.tif`: Distance to gap of size >= 50 m, 10 m resolution

- `d500.tif`: Distance to gap of size >=500 m, 10 m resolution

- `swamp.tif`: Landscape classified as either swamp (1) or other habitat (0)

Figshare link containing full set of scripts and data for Russo et al. 2024 _J. Anim. Ecol.:_ https://figshare.com/articles/dataset/3D_Structure_-_Hornbill_Seed_Dispersal/25857385

##E Session info for Step 1

```r
R version 4.4.2 (2024-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.utf8  
[2] LC_CTYPE=English_United States.utf8   
[3] LC_MONETARY=English_United States.utf8 
[4] LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: America/New_York
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] amt_0.2.2.0     lubridate_1.9.3 forcats_1.0.0   stringr_1.5.2  
 [5] dplyr_1.1.4     purrr_1.0.2     readr_2.1.5     tidyr_1.3.1    
 [9] tibble_3.3.0    ggplot2_4.0.0   tidyverse_2.0.0 terra_1.8-70   

loaded via a namespace (and not attached):
 [1] utf8_1.2.6         generics_0.1.4     circular_0.5-1     class_7.3-22      
 [5] KernSmooth_2.23-24 stringi_1.8.7      lattice_0.22-6     hms_1.1.3         
 [9] magrittr_2.0.3     grid_4.4.2         timechange_0.3.0   RColorBrewer_1.1-3
[13] mvtnorm_1.3-2      Matrix_1.7-1       backports_1.5.0    e1071_1.7-16      
[17] DBI_1.2.3          survival_3.7-0     scales_1.4.0       codetools_0.2-20  
[21] Rdpack_2.6.2       cli_3.6.3          rlang_1.1.4        units_0.8-5       
[25] rbibutils_2.3      splines_4.4.2      withr_3.0.2        tools_4.4.2       
[29] tzdb_0.5.0         checkmate_2.3.2    boot_1.3-31        fitdistrplus_1.2-1
[33] vctrs_0.6.5        R6_2.6.1           proxy_0.4-27       lifecycle_1.0.4   
[37] classInt_0.4-10    MASS_7.3-61        pkgconfig_2.0.3    pillar_1.11.1     
[41] gtable_0.3.6       data.table_1.16.4  glue_1.8.0         Rcpp_1.0.13-1     
[45] sf_1.0-19          tidyselect_1.2.1   rstudioapi_0.17.1  farver_2.1.2      
[49] compiler_4.4.2     S7_0.2.0
```

## Step 2: 
