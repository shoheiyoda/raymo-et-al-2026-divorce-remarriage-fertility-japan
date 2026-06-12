#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# 01_MakeTidyData.R
# Outline:
#  - Load raw JNFS microdata
#  - Construct retrospective person-month
#  - Calculate unweighted/weighted exposure and births.
#
# Produced tidydata;
#   - year (5-year interval)
#   - age (5-year interval)
#   - marital status
#   - exposure (weighted and unweighted)
#   - birth order
#   - number of births (weighted and unweighted)
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

rm(list=ls())

source("01_Script/00_SetParameters.R")

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Load the raw JNFS data ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
DF_org <- NULL

for(iSR in seq(14, 16)){
  DF_raw <- read_dta(paste0(JnfsDir, "Nfs2021V3M", iSR, ".dta"))
  
  DF_temp <- 
    DF_raw |> 
    filter(Sex %in% c(2,4)) |> 
    select(id, Sex, YearSvy, Weight4Grp,
           CAgeSvyW,
           MarStatW, StrMarStat,
           NofMarW,
           NofChildEvbMbf, NofChildEvbMbfHist, NofChildEvbMbfHistW,
           NofNatHist,
           DateSvy, 
           DateBth,  DateBthW,
           DateFMar, DateFMarW, DateFMarDisW,
           DateCMar,
           DateNatMbf01,  DateNatMbf02,  DateNatMbf03,
           DateNatMbf01W, DateNatMbf02W, DateNatMbf03W,
           RAgeMonthNat01W, RAgeMonthNat02W, RAgeMonthNat03W, RAgeMonthNat04W, RAgeMonthNat05W,
           YearBth,      ZMonthBth,
           YearBthW,     ZMonthBthW,
           YearCMar,     ZMonthCMar,
           YearFMarDisW, ZMonthFMarDisW)
  
  DF_org <-
    DF_org |> 
    bind_rows(DF_temp)
}

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Create variables & missing data handling----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
DF <-
  DF_org |> 
  mutate(
    DateBth      = if_else(Sex == 4, DateBthW, DateBth),
    DateAge15    = DateBth + 180,  # century month at 15th birthday - this will be starting point for exposure count
    DateFMar     = if_else(Sex == 4, DateFMarW, DateFMar),
    DateNatMbf01 = if_else(Sex == 4, DateNatMbf01W, DateNatMbf01),
    DateNatMbf02 = if_else(Sex == 4, DateNatMbf02W, DateNatMbf02),
    DateNatMbf03 = if_else(Sex == 4, DateNatMbf03W, DateNatMbf03)
  ) |> 
  # missing value indicators
  mutate(
    is_NA_DateFMar     = is.na(DateFMar) & MarStatW != 5,
    is_NA_DateCMar     = is.na(DateCMar) & Sex == 4,
    is_NA_DateFMarDisW = is.na(DateFMarDisW) & MarStatW != 1 & MarStatW != 5,
    
    is_NA_DateNatMbf01 = case_when(is.na(DateNatMbf01) & Sex == 2 & NofChildEvbMbfHist  > 0 ~ TRUE,
                                   is.na(DateNatMbf01) & Sex == 4 & NofChildEvbMbfHistW > 0 ~ TRUE,
                                   .default = FALSE),
    
    is_NA_DateNatMbf02 = case_when(is.na(DateNatMbf02) & Sex == 2 & NofChildEvbMbfHist  > 1 ~ TRUE,
                                   is.na(DateNatMbf02) & Sex == 4 & NofChildEvbMbfHistW > 1 ~ TRUE,
                                   .default = FALSE),

    is_NA_DateNatMbf03 = case_when(is.na(DateNatMbf03) & Sex == 2 & NofChildEvbMbfHist  > 2 ~ TRUE,
                                   is.na(DateNatMbf03) & Sex == 4 & NofChildEvbMbfHistW > 2 ~ TRUE,
                                   .default = FALSE)
  ) |> 
  mutate(
    any_NA_DateNatMbf = is_NA_DateNatMbf01 | is_NA_DateNatMbf02 | is_NA_DateNatMbf03
  ) |> 
  # century month of births in current marriage (married women)
  # use the number of births in birth history roster for consistency with above
  # note: births before current marriage are also asked elsewhere
  mutate(
    DateNat01W = if_else(Sex == 4 & NofNatHist > 0, DateBth + RAgeMonthNat01W, NA_real_),
    DateNat02W = if_else(Sex == 4 & NofNatHist > 1, DateBth + RAgeMonthNat02W, NA_real_),
    DateNat03W = if_else(Sex == 4 & NofNatHist > 2, DateBth + RAgeMonthNat03W, NA_real_),
    DateNat04W = if_else(Sex == 4 & NofNatHist > 3, DateBth + RAgeMonthNat04W, NA_real_),
    DateNat05W = if_else(Sex == 4 & NofNatHist > 4, DateBth + RAgeMonthNat05W, NA_real_)
  ) |> 
  # missing indicators of date of birth
  mutate(
    is_NA_DateNat01W = is.na(DateNat01W) & Sex == 4 & NofNatHist > 0,
    is_NA_DateNat02W = is.na(DateNat02W) & Sex == 4 & NofNatHist > 1,
    is_NA_DateNat03W = is.na(DateNat03W) & Sex == 4 & NofNatHist > 2,
    is_NA_DateNat04W = is.na(DateNat04W) & Sex == 4 & NofNatHist > 3,
    is_NA_DateNat05W = is.na(DateNat05W) & Sex == 4 & NofNatHist > 4
  ) |> 
  mutate(
    any_NA_DateNat0105W = is_NA_DateNat01W | is_NA_DateNat02W | is_NA_DateNat03W | is_NA_DateNat04W | is_NA_DateNat05W
  ) |> 
  mutate(
    any_NA_all = is_NA_DateFMar | is_NA_DateCMar | is_NA_DateFMarDisW | any_NA_DateNatMbf | any_NA_DateNat0105W
  ) |> 
  # number of births ever born
  #   Sex:2 -> unmarried women
  #   Sex:4 -> married women
  mutate(
   NofBth = if_else(Sex == 2, NofChildEvbMbfHist, NofChildEvbMbfHistW + NofNatHist)
  ) |> 
  mutate(
    DateBth      = ceiling(DateBth),
    DateAge15    = ceiling(DateAge15),
    DateFMar     = ceiling(DateFMar),
    DateFMarDisW = ceiling(DateFMarDisW),
    DateCMar     = ceiling(DateCMar),
    DateNatMbf01 = ceiling(DateNatMbf01),
    DateNatMbf02 = ceiling(DateNatMbf02),
    DateNatMbf03 = ceiling(DateNatMbf03),
    DateNat01W   = ceiling(DateNat01W),
    DateNat02W   = ceiling(DateNat02W),
    DateNat03W   = ceiling(DateNat03W),
    DateNat04W   = ceiling(DateNat04W),
    DateNat05W   = ceiling(DateNat05W)
  )

## drop missing cases and keep necessary variables ----
DF_wide <-
  DF |> 
  filter(any_NA_all == FALSE) |> 
  arrange(YearSvy, id) |> 
  mutate(SerialNumber = row_number()) |> 
  mutate(
    YearBth   = if_else(Sex == 2, YearBth,   YearBthW),
    ZMonthBth = if_else(Sex == 2, ZMonthBth, ZMonthBthW)
  ) |> 
  select(SerialNumber, 
         Weight4Grp,
         YearSvy, Sex, CAgeSvyW,
         MarStatW, NofBth,
         ZMonthBth, YearBth, StrMarStat,
         DateSvy, DateBth, DateAge15, DateFMar, DateCMar, DateFMarDisW,
         DateNatMbf01, DateNatMbf02, DateNatMbf03,
         DateNat01W, DateNat02W, DateNat03W, DateNat04W, DateNat05W
         )

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Create person-month data ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
DF_long <-
  DF_wide |> 
  mutate(NofPsnPrd = DateSvy - (DateAge15 - 1)) |> 
  uncount(NofPsnPrd) |> 
  group_by(SerialNumber) |> 
  mutate(RDurMonth = row_number()) |> 
  ungroup() |> 
  mutate(DateCounter = DateAge15 + RDurMonth - 1) |> 
  
  # create round numbers for age and year
  # small adjustment is to make sure that month 12 goes in year t rather than t+1
  mutate(
    CAge = trunc((DateCounter - DateBth) / 12),
    Year = trunc((DateCounter - .001)    / 12)
  ) |> 
  # create transition indicators
  mutate(
    Svy_flg     = if_else(DateCounter == DateSvy,      TRUE, FALSE, missing = FALSE),
    FMar_flg    = if_else(DateCounter == DateFMar,     TRUE, FALSE, missing = FALSE),
    FMarDis_flg = if_else(DateCounter == DateFMarDisW, TRUE, FALSE, missing = FALSE),
    CMar_flg    = if_else(DateCounter == DateCMar,     TRUE, FALSE, missing = FALSE),
    Bth1_flg    = if_else((DateCounter == DateNat01W) | (DateCounter == DateNatMbf01), TRUE, FALSE, missing = FALSE),
    Bth2_flg    = if_else((DateCounter == DateNat02W) | (DateCounter == DateNatMbf02), TRUE, FALSE, missing = FALSE),
    Bth3_flg    = if_else((DateCounter == DateNat03W) | (DateCounter == DateNatMbf03), TRUE, FALSE, missing = FALSE),
    Bth4_flg    = if_else( DateCounter == DateNat04W,  TRUE, FALSE, missing = FALSE),
    Bth5_flg    = if_else( DateCounter == DateNat05W,  TRUE, FALSE, missing = FALSE)
  ) |> 
  # create marital status measures
  #  _40 -> at age 40 (time invariant)
  #  _t  -> at age  t (time variant)
  mutate(
    MarStat_40 = "Never married",
    MarStat_40 = if_else(MarStatW == 1,                                        "First married(1st)",   MarStat_40),
    MarStat_40 = if_else(MarStatW == 1 & StrMarStat == 3 & !is.na(StrMarStat), "First married(Remar)", MarStat_40),
    MarStat_40 = if_else(MarStatW %in% c(2,3,4),                               "Formerly married",     MarStat_40),
    MarStat_40 = if_else(MarStatW %in% c(6,7,8),                               "Remarried",            MarStat_40),
    MarStat_40 = fct_relevel(MarStat_40,
                            "Never married",
                            "First married(1st)",
                            "First married(Remar)",
                            "Formerly married",
                            "Remarried"),
    
    MarStat_t = "Never married",
    MarStat_t = if_else(DateCounter >= DateFMar     & !is.na(DateFMar),                                                  "First married(1st)",   MarStat_t),
    MarStat_t = if_else(DateCounter >= DateFMar     & !is.na(DateFMar) & StrMarStat == 3 & !is.na(StrMarStat),           "First married(Remar)", MarStat_t),
    MarStat_t = if_else(DateCounter >= DateFMarDisW &                           !is.na(DateFMarDisW),                    "Formerly married",     MarStat_t),
    MarStat_t = if_else(DateCounter >= DateCMar     & DateCMar > DateFMarDisW & !is.na(DateCMar) & !is.na(DateFMarDisW), "Remarried",            MarStat_t),
    MarStat_t = fct_relevel(MarStat_t,
                          "Never married",
                          "First married(1st)",
                          "First married(Remar)",
                          "Formerly married",
                          "Remarried")
  ) |> 
  # construct exposure variables
  # assume a_x = 0.5
  mutate(
    FMar_exp    = case_when(Svy_flg == TRUE | FMar_flg    == TRUE ~ 0.5, .default = 1),
    CMar_exp    = case_when(Svy_flg == TRUE | CMar_flg    == TRUE ~ 0.5, .default = 1),
    FMarDis_exp = case_when(Svy_flg == TRUE | FMarDis_flg == TRUE ~ 0.5, .default = 1),
    Bth1_exp    = case_when(Svy_flg == TRUE | Bth1_flg    == TRUE ~ 0.5, .default = 1),
    Bth2_exp    = case_when(Svy_flg == TRUE | Bth2_flg    == TRUE ~ 0.5, .default = 1),
    Bth3_exp    = case_when(Svy_flg == TRUE | Bth3_flg    == TRUE ~ 0.5, .default = 1),
    Bth4_exp    = case_when(Svy_flg == TRUE | Bth4_flg    == TRUE ~ 0.5, .default = 1),
    Bth5_exp    = case_when(Svy_flg == TRUE | Bth5_flg    == TRUE ~ 0.5, .default = 1)
  ) |> 
  mutate(CAge5y = case_match(CAge,
                             c(15:19) ~ "15-19",
                             c(20:24) ~ "20-24",
                             c(25:29) ~ "25-29",
                             c(30:34) ~ "30-34",
                             c(35:39) ~ "35-39",
                             c(40:44) ~ "40-44",
                             c(45:49) ~ "45-49"),
         
         Year5y = case_match(Year,
                             c(1975:1989) ~ "1975-89",
                             c(1990:1994) ~ "1990-94",
                             c(1995:1999) ~ "1995-99",
                             c(2000:2004) ~ "2000-04",
                             c(2005:2009) ~ "2005-09",
                             c(2010:2014) ~ "2010-14",
                             c(2015:2021) ~ "2015-21"
                             ),
         
         Year5yCntCensus = case_match(Year,
                             c(1975:1997) ~ "1975-97",
                             c(1998:2002) ~ "1998-02",
                             c(2003:2007) ~ "2003-07",
                             c(2008:2012) ~ "2008-12",
                             c(2013:2017) ~ "2013-17",
                             c(2018:2021) ~ "2018-21"
                             ),
         
         YearBth5y = case_match(YearBth,
                                c(1960:1964) ~ "1960-64",
                                c(1965:1969) ~ "1965-69",
                                c(1970:1974) ~ "1970-74",
                                c(1975:1979) ~ "1975-79",
                                c(1980:1984) ~ "1980-84"
                                )
  ) |> 
  # exclude those age 50 and over
  filter(CAge < 50)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Calculate exposure and number of births by year and age group ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# period
tbl_exp_NofBth_p_temp <-
  DF_long |> 
  group_by(Year5y, CAge5y, MarStat_40, MarStat_t) |> 
  reframe(
    Exposure1_uwt = sum(Bth1_exp, na.rm = TRUE),
    Exposure2_uwt = sum(Bth2_exp, na.rm = TRUE),
    Exposure3_uwt = sum(Bth3_exp, na.rm = TRUE),
    Exposure4_uwt = sum(Bth4_exp, na.rm = TRUE),
    Exposure5_uwt = sum(Bth5_exp, na.rm = TRUE),
    
    NofBth1_uwt    = sum(Bth1_flg, na.rm = TRUE),
    NofBth2_uwt    = sum(Bth2_flg, na.rm = TRUE),
    NofBth3_uwt    = sum(Bth3_flg, na.rm = TRUE),
    NofBth4_uwt    = sum(Bth4_flg, na.rm = TRUE),
    NofBth5_uwt    = sum(Bth5_flg, na.rm = TRUE),
    
    Exposure1_wt = sum(Bth1_exp * Weight4Grp, na.rm = TRUE),
    Exposure2_wt = sum(Bth2_exp * Weight4Grp, na.rm = TRUE),
    Exposure3_wt = sum(Bth3_exp * Weight4Grp, na.rm = TRUE),
    Exposure4_wt = sum(Bth4_exp * Weight4Grp, na.rm = TRUE),
    Exposure5_wt = sum(Bth5_exp * Weight4Grp, na.rm = TRUE),
    
    NofBth1_wt    = sum(Bth1_flg * Weight4Grp, na.rm = TRUE),
    NofBth2_wt    = sum(Bth2_flg * Weight4Grp, na.rm = TRUE),
    NofBth3_wt    = sum(Bth3_flg * Weight4Grp, na.rm = TRUE),
    NofBth4_wt    = sum(Bth4_flg * Weight4Grp, na.rm = TRUE),
    NofBth5_wt    = sum(Bth5_flg * Weight4Grp, na.rm = TRUE)
  )

tbl_exp_NofBth_p <-
  expand_grid(
  Year5y = c("1975-89", "1990-94", "1995-99", "2000-04", "2005-09", "2010-14", "2015-21"),
  CAge5y = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
  MarStat_40 = c("Never married", "First married(1st)", "First married(Remar)", "Formerly married", "Remarried"),
  MarStat_t  = c("Never married", "First married(1st)", "First married(Remar)", "Formerly married", "Remarried")
) |> 
  left_join(tbl_exp_NofBth_p_temp,
            by = c("Year5y", "CAge5y", "MarStat_40", "MarStat_t")) |> 
  replace_na(list(
    Exposure1_uwt = 0,
    Exposure2_uwt = 0,
    Exposure3_uwt = 0,
    Exposure4_uwt = 0,
    Exposure5_uwt = 0,
    NofBth1_uwt   = 0,
    NofBth2_uwt   = 0,
    NofBth3_uwt   = 0,
    NofBth4_uwt   = 0,
    NofBth5_uwt   = 0,
    Exposure1_wt  = 0,
    Exposure2_wt  = 0,
    Exposure3_wt  = 0,
    Exposure4_wt  = 0,
    Exposure5_wt  = 0,
    NofBth1_wt    = 0,
    NofBth2_wt    = 0,
    NofBth3_wt    = 0,
    NofBth4_wt    = 0,
    NofBth5_wt    = 0
  )) |> 
  mutate(
    MarStat_40 = fct_relevel(MarStat_40, "Never married", "First married(1st)", "First married(Remar)", "Formerly married", "Remarried"),
    MarStat_t  = fct_relevel(MarStat_t,  "Never married", "First married(1st)", "First married(Remar)", "Formerly married", "Remarried")
  ) |> 
  pivot_longer(
    cols = -c(Year5y, CAge5y, MarStat_40, MarStat_t),
    names_to = c(".value", "BirthOrder", "Weight"),
    names_pattern = "(Exposure|NofBth)([1-5]+)_(uwt|wt)"
  ) |> 
  mutate(
    BirthOrder = as.integer(BirthOrder),
    Weight = factor(Weight, levels = c("uwt", "wt"))
  )

# cohort
tbl_exp_NofBth_c_temp <-
  DF_long |> 
  group_by(YearBth5y, CAge5y, MarStat_40, MarStat_t) |> 
  reframe(
    Exposure1_uwt = sum(Bth1_exp, na.rm = TRUE),
    Exposure2_uwt = sum(Bth2_exp, na.rm = TRUE),
    Exposure3_uwt = sum(Bth3_exp, na.rm = TRUE),
    Exposure4_uwt = sum(Bth4_exp, na.rm = TRUE),
    Exposure5_uwt = sum(Bth5_exp, na.rm = TRUE),
    
    NofBth1_uwt    = sum(Bth1_flg, na.rm = TRUE),
    NofBth2_uwt    = sum(Bth2_flg, na.rm = TRUE),
    NofBth3_uwt    = sum(Bth3_flg, na.rm = TRUE),
    NofBth4_uwt    = sum(Bth4_flg, na.rm = TRUE),
    NofBth5_uwt    = sum(Bth5_flg, na.rm = TRUE),
    
    Exposure1_wt = sum(Bth1_exp * Weight4Grp, na.rm = TRUE),
    Exposure2_wt = sum(Bth2_exp * Weight4Grp, na.rm = TRUE),
    Exposure3_wt = sum(Bth3_exp * Weight4Grp, na.rm = TRUE),
    Exposure4_wt = sum(Bth4_exp * Weight4Grp, na.rm = TRUE),
    Exposure5_wt = sum(Bth5_exp * Weight4Grp, na.rm = TRUE),
    
    NofBth1_wt    = sum(Bth1_flg * Weight4Grp, na.rm = TRUE),
    NofBth2_wt    = sum(Bth2_flg * Weight4Grp, na.rm = TRUE),
    NofBth3_wt    = sum(Bth3_flg * Weight4Grp, na.rm = TRUE),
    NofBth4_wt    = sum(Bth4_flg * Weight4Grp, na.rm = TRUE),
    NofBth5_wt    = sum(Bth5_flg * Weight4Grp, na.rm = TRUE)
  )

tbl_exp_NofBth_c <-
  expand_grid(
    YearBth5y = c("1960-64", "1965-69", "1970-74", "1975-79"),
    CAge5y = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
    MarStat_40 = c("Never married", "First married(1st)", "First married(Remar)", "Formerly married", "Remarried"),
    MarStat_t  = c("Never married", "First married(1st)", "First married(Remar)", "Formerly married", "Remarried")
  ) |> 
  left_join(tbl_exp_NofBth_c_temp,
            by = c("YearBth5y", "CAge5y", "MarStat_40", "MarStat_t")) |> 
  replace_na(list(
    Exposure1_uwt = 0,
    Exposure2_uwt = 0,
    Exposure3_uwt = 0,
    Exposure4_uwt = 0,
    Exposure5_uwt = 0,
    NofBth1_uwt   = 0,
    NofBth2_uwt   = 0,
    NofBth3_uwt   = 0,
    NofBth4_uwt   = 0,
    NofBth5_uwt   = 0,
    Exposure1_wt  = 0,
    Exposure2_wt  = 0,
    Exposure3_wt  = 0,
    Exposure4_wt  = 0,
    Exposure5_wt  = 0,
    NofBth1_wt    = 0,
    NofBth2_wt    = 0,
    NofBth3_wt    = 0,
    NofBth4_wt    = 0,
    NofBth5_wt    = 0
  )) |> 
  mutate(
    MarStat_40 = fct_relevel(MarStat_40, "Never married", "First married(1st)", "First married(Remar)", "Formerly married", "Remarried"),
    MarStat_t  = fct_relevel(MarStat_t,  "Never married", "First married(1st)", "First married(Remar)", "Formerly married", "Remarried")
  ) |> 
  pivot_longer(
    cols = -c(YearBth5y, CAge5y, MarStat_40, MarStat_t),
    names_to = c(".value", "BirthOrder", "Weight"),
    names_pattern = "(Exposure|NofBth)([1-5]+)_(uwt|wt)"
  ) |> 
  mutate(
    BirthOrder = as.integer(BirthOrder),
    Weight = factor(Weight, levels = c("uwt", "wt"))
  )

#=#=#=#=#=#=#=#=##=#=#=#=#=#=#=#=##=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Save Data ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
saveRDS(tbl_exp_NofBth_p, file = paste0("00_Data/", "tbl_exp_NofBth_p.rds")) # period
saveRDS(tbl_exp_NofBth_c, file = paste0("00_Data/", "tbl_exp_NofBth_c.rds")) # cohort
