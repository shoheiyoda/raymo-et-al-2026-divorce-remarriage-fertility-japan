#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# 00_SetParameters
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
library(tidyverse)
library(haven)

"%notin%" <- Negate("%in%")

# NOTE: Update this path to your local directory where the JNFS microdata (.dta files) are stored.
JnfsDir <- "/Volumes/Samsung_T5/JNFS/03_M/"

# output directory
OutDir <- "02_Output/"
  OutDir_p <- paste0(OutDir, "Period/")
  OutDir_c <- paste0(OutDir, "Cohort/")