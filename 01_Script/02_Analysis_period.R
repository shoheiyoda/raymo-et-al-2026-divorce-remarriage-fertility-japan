#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# 02_Analysis_period.R
#  Outline:
#  - Calculate observed/counterfactual period total fertility rates (PTFR).
#  - Generate period-based counterparts to the cohort-based figures: 
#    Figures 1 and 2, and Appendix Figures A1-A4 and A6.
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

rm(list=ls())

source("01_Script/00_SetParameters.R")

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
#  Calculate Marital-Status-Specific Fertility Rates (Pfx) ----
#  See Equation 1 components in the text
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
tbl_exp_NofBth <- 
  readRDS("00_Data/tbl_exp_NofBth_p.rds") |> 
  filter(Year5y %in% c("2000-04", "2005-09", "2010-14", "2015-21")) |> 
  filter(CAge5y %notin% c("40-44", "45-49")) 

tbl_fx_obs <-
  tbl_exp_NofBth |> 
  filter(CAge5y %notin% c("40-44", "45-49")) |> 
  group_by(Year5y, CAge5y, MarStat_t, BirthOrder, Weight) |> 
  reframe(
    Exposure = sum(Exposure),
    NofBth   = sum(NofBth)
  ) |> 
  mutate(Pfx = NofBth / Exposure * 12) |> 
  mutate(Pfx = ifelse(is.nan(Pfx), 0, Pfx)) |> 
  group_by(Year5y, CAge5y, BirthOrder, Weight) |> 
  mutate(PropExp = Exposure / sum(Exposure)) |> 
  ungroup() |> 
  mutate(Comp2TFR = Pfx * PropExp) |> 
  rename(MarStat = MarStat_t)

## Figure 1: marital-status-specific exposure ----
tbl_Fig1 <-
  tbl_fx_obs |> 
  filter(Year5y %in% c("2000-04", "2015-21")) |> 
  filter(MarStat != "First married(Remar)") |> 
  filter(Weight == "wt") |>
  group_by(Year5y, CAge5y, MarStat) |>
  reframe(PropExp = mean(PropExp))

Fig1 <-
  tbl_Fig1 |> 
  ggplot(aes(x = CAge5y,
             y = PropExp,
             group = interaction(MarStat, Year5y),
             color  = MarStat,
             linetype = Year5y,
             shape = MarStat)
  ) +
  geom_line() +
  geom_point(fill = "white", size = 3) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 18, 21, 23)) +
  labs(
    x = "Age",
    y = "Proportion",
    color = "Marital Status",
    linetype = "Year",
    shape = "Marital Status"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_text(size = rel(1.3)),
        legend.text  = element_text(size = rel(1.3)),
        strip.text   = element_text(size = rel(1.3)),
        axis.text.x  = element_text(size = rel(1.3)),
        axis.text.y  = element_text(size = rel(1.3)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))

print(Fig1)

ggsave(filename = paste0(OutDir_p, "Figure1.pdf"),
       device = "pdf",
       width  = 9,
       height = 6)

## Figure 2: marital-status-specific ASFR ----
# observed marital-status-specific ASFR
tbl_ASFR_obs <-
  tbl_exp_NofBth |> 
  filter(CAge5y %notin% c("40-44", "45-49")) |> 
  group_by(Year5y, CAge5y, MarStat_t, BirthOrder, Weight) |> 
  reframe(
    Exposure = sum(Exposure),
    NofBth   = sum(NofBth)
  ) |> 
  filter(Weight == "wt") |> 
  rename(MarStat = MarStat_t) |> 
  mutate(MarStat = if_else(MarStat %in% c("First married(1st)", "First married(Remar)"), "First married", MarStat) |> 
                   fct_relevel("Never married", "First married", "Formerly married", "Remarried")
         ) |> 
  group_by(Year5y, CAge5y, BirthOrder, MarStat) |> 
  reframe(
    Exposure    = sum(Exposure), 
    NofBth      = sum(NofBth),
  ) |>
  ungroup() |> 
  mutate(Pfx = NofBth / Exposure * 12) |> 
  mutate(Pfx = ifelse(is.nan(Pfx), 0, Pfx)) |> 
  group_by(Year5y, CAge5y, BirthOrder) |> 
  mutate(PropExp = Exposure / sum(Exposure)) |> 
  ungroup() |> 
  mutate(Comp2TFR = Pfx * PropExp)

# combine the two first-married categories
ASFR_temp <-
  tbl_fx_obs |> 
  filter(Year5y %in% c("2000-04", "2015-21")) |> 
  filter(Weight == "wt") |> 
  filter(MarStat %in% c("First married(1st)", "First married(Remar)")) |> 
  group_by(Year5y, CAge5y, BirthOrder) |> 
  mutate(weight = PropExp / sum(PropExp)) |> 
  reframe(Pfx = weighted.mean(Pfx, w = weight)) |> 
  mutate(MarStat = "First married") |> 
  relocate(Year5y, CAge5y, BirthOrder, MarStat, Pfx)
  
tbl_Fig2 <- 
  tbl_fx_obs |> 
  filter(Year5y %in% c("2000-04", "2015-21")) |> 
  filter(Weight == "wt") |> 
  filter(MarStat %notin% c("First married(1st)", "First married(Remar)")) |> 
  select(Year5y, CAge5y, BirthOrder, MarStat, Pfx) |> 
  bind_rows(ASFR_temp) |> 
  mutate(MarStat = fct_relevel(MarStat,
                               "Never married",
                               "First married",
                               "Formerly married",
                               "Remarried"))

Fig2 <-
  tbl_Fig2 |> 
  group_by(Year5y, CAge5y, MarStat) |> 
  reframe(Pfx = sum(Pfx)) |> 
  ggplot(aes(x = CAge5y,
             y = Pfx,
             group = interaction(MarStat, Year5y),
             color = MarStat,
             linetype = Year5y,
             shape = MarStat)
  ) +
    geom_line() +
    geom_point(fill = "white", size = 3) +
    scale_color_brewer(palette = "Set1") +
    scale_shape_manual(values = c(16, 18, 21, 23)) +
    labs(
      x = "Age Group",
      y = "ASFR",
      linetype = "Year",
      color = "Marital Status",
      shape = "Marital Status"  ) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          legend.title = element_text(size = rel(1.3)),
          legend.text  = element_text(size = rel(1.3)),
          axis.text.x  = element_text(size = rel(1.3)),
          axis.text.y  = element_text(size = rel(1.3)),
          axis.title.x = element_text(size = rel(1.3)),
          axis.title.y = element_text(size = rel(1.3)))

print(Fig2)

ggsave(filename = paste0(OutDir_p, "Figure2.pdf"),
       device = "pdf",
       width  = 9,
       height = 6)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Birth hazard ratio  ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# set ASFR
ASFR_temp <- 
  tbl_exp_NofBth |> 
  filter(Weight == "wt") |> 
  group_by(Year5y, CAge5y, MarStat_40, MarStat_t) |> 
  mutate(NofBthT = sum(NofBth)) |> 
  ungroup() |> 
  mutate(Pfx = NofBthT / Exposure * 12,
         Pfx = ifelse(is.nan(Pfx), 0, Pfx)) |> 
  filter(BirthOrder == 1) |> 
  select(Year5y, CAge5y, MarStat_40, MarStat_t, Pfx)

## Formerly married and remarried women's fx(FstMar) ratio relative to first married ----
ASFR_FstMar <-
  ASFR_temp |> 
  filter(MarStat_40 == "First married(1st)") |> 
  filter(MarStat_t  == "First married(1st)") |> 
  select(-c(MarStat_40, MarStat_t)) |> 
  rename(Pfx_FstMar = Pfx)
    
Ratio2FstMar_byYear5y <- 
  ASFR_temp |> 
  filter(MarStat_40 %in% c("Formerly married", "Remarried")) |> 
  filter(MarStat_t  %in% c("First married(1st)")) |> 
  select(-MarStat_t) |> 
  left_join(ASFR_FstMar, by = c("Year5y", "CAge5y")) |> 
  mutate(RatioPfx = Pfx / Pfx_FstMar,
         RatioPfx = if_else(Pfx == 0 & Pfx_FstMar == 0, 1, RatioPfx))


Fig_FxRatio2FstMar_byYear5y <-
  Ratio2FstMar_byYear5y |> 
  ggplot(aes(x = Year5y,
             y = RatioPfx,
             group = interaction(MarStat_40, CAge5y),
             color = MarStat_40,
             linetype = CAge5y,
             shape = MarStat_40)
         ) +
  geom_line() +
  geom_point(size = 3) +
  scale_y_continuous(limit = c(-4.0, 4.0), breaks = seq(-4.0, 4.0, 1.0)) +
  labs(
    x = "Year",
    y = "Fx Ratio (relative to Fx of the stably first-married)",
    color = "Marital Status",
    shape = "Marital Status",
    linetype = "Age"
  ) +
  theme_bw() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.text  = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        axis.text.x  = element_text(size = rel(1.5)),
        axis.text.y  = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))

print(Fig_FxRatio2FstMar_byYear5y)

ggsave(filename = paste0(OutDir_p, "Fig_FxRatio2FstMar_byYear5y.pdf"),
       device = "pdf",
       width  = 9,
       height = 6)

# average fx ratio across age groups
Ratio2FstMar <-
  Ratio2FstMar_byYear5y |> 
  group_by(CAge5y, MarStat_40) |> 
  reframe(RatioPfx = mean(RatioPfx))

Fig_FxRatio2FstMar <-
  Ratio2FstMar |> 
  ggplot(aes(x = CAge5y,
             y = RatioPfx,
             group = MarStat_40,
             color = MarStat_40)
         ) +
  geom_line() +
  geom_point(size = 3) +
  scale_y_continuous(limit = c(-2, 2), breaks = seq(-2, 2, 0.5)) +
  labs(
    x = "Age",
    y = "Fx Ratio (relative to Fx of the stably first-married)",
    color = NULL
  ) +
  theme_bw() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.text  = element_text(size = rel(1.5)),
        axis.text.x  = element_text(size = rel(1.5)),
        axis.text.y  = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))

print(Fig_FxRatio2FstMar)

ggsave(filename = paste0(OutDir_p, "Fig_FxRatio2FstMar.pdf"),
       device = "pdf",
       width  = 9,
       height = 6)

Ratio2FstMar |> 
  group_by(MarStat_40) |> 
  reframe(min  = min(RatioPfx),
          max  = max(RatioPfx),
          mean = mean(RatioPfx))

## Remarried women's fx(Div) relative to formerly women ----
ASFR_Div <- 
  ASFR_temp |> 
  filter(MarStat_40 == "Formerly married") |> 
  filter(MarStat_t  == "Formerly married") |> 
  select(-c(MarStat_40, MarStat_t)) |> 
  rename(Pfx_Div = Pfx)

Ratio2Div_byYear5y <- 
  ASFR_temp |> 
  filter(MarStat_40 %in% c("Remarried")) |> 
  filter(MarStat_t  %in% c("Formerly married")) |> 
  select(-MarStat_t) |> 
  left_join(ASFR_Div, by = c("Year5y", "CAge5y")) |> 
  mutate(RatioPfx = Pfx / Pfx_Div,
         RatioPfx = if_else(Pfx == 0 & Pfx_Div == 0, 1, RatioPfx))

Fig_Ratio2Div_byYear5y <-
  Ratio2Div_byYear5y |> 
  ggplot(aes(x = Year5y,
             y = RatioPfx,
             group = CAge5y,
             color = CAge5y,
             linetype = CAge5y)
  ) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    x = "Year",
    y = "Fx Ratio (relative to Fx of the formerly married)",
    color =    "Age",
    linetype = "Age"
  ) +
  theme_bw() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.text  = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        axis.text.x  = element_text(size = rel(1.5)),
        axis.text.y  = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))

print(Fig_Ratio2Div_byYear5y)

ggsave(filename = paste0(OutDir_p, "Fig_FxRatio2Div_Year5y.pdf"),
       device = "pdf",
       width  = 9,
       height = 6)

# average fx ratio across age groups
Ratio2Div <-
  Ratio2Div_byYear5y |> 
  filter(!is.infinite(RatioPfx)) |> 
  group_by(CAge5y) |> 
  reframe(RatioPfx = mean(RatioPfx, na.rm = TRUE)) |> 
  ungroup()

Fig_Ratio2Div <-
  Ratio2Div |> 
  ggplot(aes(x = CAge5y,
             y = RatioPfx,
             group = 1)
         ) +
  geom_line() +
  geom_point(size = 3) +
  scale_y_continuous(limit = c(-3.0, 3.0), breaks = seq(-3.0, 3.0, 1.0)) +
  labs(
    x = "Age",
    y = "Fx Ratio (relative to Fx of the formerly married)",
    color = NULL
  ) +
  theme_bw() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.text  = element_text(size = rel(1.5)),
        axis.text.x  = element_text(size = rel(1.5)),
        axis.text.y  = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))

print(Fig_Ratio2Div)

ggsave(filename = paste0(OutDir_p, "Fig_FxRatio2Div.pdf"),
       device = "pdf",
       width  = 9,
       height = 6)

Ratio2Div |> 
  reframe(min  = min(RatioPfx),
          max  = max(RatioPfx),
          mean = mean(RatioPfx))

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Counterfactual analysis ----
#   Pattern A: First-married women paired with remarried men do NOT marry (returned to never-married state).
#   Pattern B: First-married women paired with remarried men marry first-married men.
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## No divorce A ----
PropExp_NoDiv_A <- 
  tbl_fx_obs |> 
  filter(Weight == "wt") |> 
  select(Year5y, CAge5y, BirthOrder, MarStat, Pfx, PropExp) |> 
  mutate(
    MarStat_cf = MarStat,
    MarStat_cf = if_else(MarStat %in% c("First married(Remar)"), "Never married", MarStat_cf)
  ) |> 
  group_by(Year5y, CAge5y, BirthOrder, MarStat_cf) |> 
  reframe(PropExp_cf = sum(PropExp)) |> 
  rename(MarStat = MarStat_cf)

Ratio2FstMar_temp <-
  Ratio2FstMar |> 
  rename(MarStat = MarStat_40)

tbl_ASFR_FstMar_temp <-
  tbl_fx_obs |> 
  filter(Weight == "wt") |> 
  filter(MarStat == "First married(1st)") |> 
  select(Year5y, CAge5y, BirthOrder, Pfx_FstMar = Pfx)

PSTFR_NoDiv_A <-
  tbl_fx_obs |> 
  filter(Weight == "wt") |> 
  filter(MarStat %in% c("Never married", "First married(1st)", "Formerly married", "Remarried")) |> 
  select(Year5y, CAge5y, BirthOrder, MarStat, Pfx_obs = Pfx) |>
  left_join(tbl_ASFR_FstMar_temp, by = c("Year5y", "CAge5y", "BirthOrder")) |>
  left_join(Ratio2FstMar_temp, by = c("CAge5y", "MarStat")) |> 
  mutate(RatioPfx = if_else(is.na(RatioPfx), 1, RatioPfx)) |>
  mutate(Pfx_temp = if_else(MarStat %in% c("Formerly married", "Remarried"), Pfx_FstMar, Pfx_obs)) |> 
  mutate(Pfx = Pfx_temp * RatioPfx) |> 
  select(Year5y, CAge5y, BirthOrder, MarStat, Pfx) |> 
  left_join(PropExp_NoDiv_A, by = c("Year5y", "CAge5y", "BirthOrder", "MarStat")) |> 
  mutate(WeightedASFR = Pfx * PropExp_cf) |> 
  group_by(Year5y, BirthOrder) |> 
  reframe(PSTFR_NoDiv_A = sum(WeightedASFR) * 5)

TFR_NoDiv_A <- 
  PSTFR_NoDiv_A |> 
  group_by(Year5y) |> 
  reframe(TFR_NoDiv_A = sum(PSTFR_NoDiv_A))

## No divorce B ----
PropExp_NoDiv_B <- 
  tbl_fx_obs |> 
  filter(Weight == "wt") |> 
  select(Year5y, CAge5y, BirthOrder, MarStat, Pfx, PropExp) |> 
  mutate(
    MarStat_cf = MarStat,
    MarStat_cf = if_else(MarStat %in% c("First married(Remar)"), "First married(1st)", MarStat_cf)
  ) |> 
  group_by(Year5y, CAge5y, BirthOrder, MarStat_cf) |> 
  reframe(PropExp_cf = sum(PropExp)) |> 
  rename(MarStat = MarStat_cf)

Ratio2FstMar_temp <-
  Ratio2FstMar |> 
  rename(MarStat = MarStat_40)

tbl_ASFR_FstMar_temp <-
  tbl_fx_obs |> 
  filter(Weight == "wt") |> 
  filter(MarStat == "First married(1st)") |> 
  select(Year5y, CAge5y, BirthOrder, Pfx_FstMar = Pfx)

PSTFR_NoDiv_B <-
  tbl_fx_obs |> 
  filter(Weight == "wt") |> 
  filter(MarStat %in% c("Never married", "First married(1st)", "Formerly married", "Remarried")) |> 
  select(Year5y, CAge5y, BirthOrder, MarStat, Pfx_obs = Pfx) |>
  left_join(tbl_ASFR_FstMar_temp, by = c("Year5y", "CAge5y", "BirthOrder")) |>
  left_join(Ratio2FstMar_temp, by = c("CAge5y", "MarStat")) |> 
  mutate(RatioPfx = if_else(is.na(RatioPfx), 1, RatioPfx)) |>
  mutate(Pfx_temp = if_else(MarStat %in% c("Formerly married", "Remarried"), Pfx_FstMar, Pfx_obs)) |> 
  mutate(Pfx = Pfx_temp * RatioPfx) |> 
  select(Year5y, CAge5y, BirthOrder, MarStat, Pfx) |> 
  left_join(PropExp_NoDiv_B, by = c("Year5y", "CAge5y", "BirthOrder", "MarStat")) |> 
  mutate(WeightedASFR = Pfx * PropExp_cf) |> 
  group_by(Year5y, BirthOrder) |> 
  reframe(PSTFR_NoDiv_B = sum(WeightedASFR) * 5)

TFR_NoDiv_B <- 
  PSTFR_NoDiv_B |> 
  group_by(Year5y) |> 
  reframe(TFR_NoDiv_B = sum(PSTFR_NoDiv_B))

## No remarriage A ----
PropExp_NoRemar_A <- 
  tbl_fx_obs |> 
  filter(Weight == "wt") |> 
  select(Year5y, CAge5y, BirthOrder, MarStat, Pfx, PropExp) |> 
  mutate(
    MarStat_cf = MarStat,
    MarStat_cf = if_else(MarStat %in% c("First married(Remar)"), "Never married", MarStat_cf)
  ) |> 
  group_by(Year5y, CAge5y, BirthOrder, MarStat_cf) |> 
  reframe(PropExp_cf = sum(PropExp)) |> 
  rename(MarStat = MarStat_cf)

Ratio2Div_temp <-
  Ratio2Div |> 
  mutate(MarStat = "Remarried")

tbl_ASFR_Div_temp <-
  tbl_fx_obs |> 
  filter(Weight == "wt") |> 
  filter(MarStat == "Formerly married") |> 
  select(Year5y, CAge5y, BirthOrder, Pfx_Div = Pfx)

PSTFR_NoRemar_A <-
  tbl_fx_obs |> 
  filter(Weight == "wt") |> 
  filter(MarStat %in% c("Never married", "First married(1st)", "Formerly married", "Remarried")) |> 
  select(Year5y, CAge5y, BirthOrder, MarStat, Pfx_obs = Pfx) |>
  left_join(tbl_ASFR_Div_temp, by = c("Year5y", "CAge5y", "BirthOrder")) |>
  left_join(Ratio2Div_temp, by = c("CAge5y", "MarStat")) |> 
  mutate(RatioPfx = if_else(is.na(RatioPfx), 1, RatioPfx)) |>
  mutate(Pfx_temp = if_else(MarStat %in% c("Remarried"), Pfx_Div, Pfx_obs)) |> 
  mutate(Pfx = Pfx_temp * RatioPfx) |> 
  select(Year5y, CAge5y, BirthOrder, MarStat, Pfx) |> 
  left_join(PropExp_NoRemar_A, by = c("Year5y", "CAge5y", "BirthOrder", "MarStat")) |> 
  mutate(WeightedASFR = Pfx * PropExp_cf) |> 
  group_by(Year5y, BirthOrder) |> 
  reframe(PSTFR_NoRemar_A = sum(WeightedASFR) * 5)

TFR_NoRemar_A <- 
  PSTFR_NoRemar_A |> 
  group_by(Year5y) |> 
  reframe(TFR_NoRemar_A = sum(PSTFR_NoRemar_A))

## No remarriage B ----
PropExp_NoRemar_B <- 
  tbl_fx_obs |> 
  filter(Weight == "wt") |> 
  select(Year5y, CAge5y, BirthOrder, MarStat, Pfx, PropExp) |> 
  mutate(
    MarStat_cf = MarStat,
    MarStat_cf = if_else(MarStat %in% c("First married(Remar)"), "First married(1st)", MarStat_cf)
  ) |> 
  group_by(Year5y, CAge5y, BirthOrder, MarStat_cf) |> 
  reframe(PropExp_cf = sum(PropExp)) |> 
  rename(MarStat = MarStat_cf)

Ratio2Div_temp <-
  Ratio2Div |> 
  mutate(MarStat = "Remarried")

tbl_ASFR_Div_temp <-
  tbl_fx_obs |> 
  filter(Weight == "wt") |> 
  filter(MarStat == "Formerly married") |> 
  select(Year5y, CAge5y, BirthOrder, Pfx_Div = Pfx)

PSTFR_NoRemar_B <-
  tbl_fx_obs |> 
  filter(Weight == "wt") |> 
  filter(MarStat %in% c("Never married", "First married(1st)", "Formerly married", "Remarried")) |> 
  select(Year5y, CAge5y, BirthOrder, MarStat, Pfx_obs = Pfx) |>
  left_join(tbl_ASFR_Div_temp, by = c("Year5y", "CAge5y", "BirthOrder")) |>
  left_join(Ratio2Div_temp, by = c("CAge5y", "MarStat")) |> 
  mutate(RatioPfx = if_else(is.na(RatioPfx), 1, RatioPfx)) |>
  mutate(Pfx_temp = if_else(MarStat %in% c("Remarried"), Pfx_Div, Pfx_obs)) |> 
  mutate(Pfx = Pfx_temp * RatioPfx) |> 
  select(Year5y, CAge5y, BirthOrder, MarStat, Pfx) |> 
  left_join(PropExp_NoRemar_B, by = c("Year5y", "CAge5y", "BirthOrder", "MarStat")) |> 
  mutate(WeightedASFR = Pfx * PropExp_cf) |> 
  group_by(Year5y, BirthOrder) |> 
  reframe(PSTFR_NoRemar_B = sum(WeightedASFR) * 5)

TFR_NoRemar_B <- 
  PSTFR_NoRemar_B |> 
  group_by(Year5y) |> 
  reframe(TFR_NoRemar_B = sum(PSTFR_NoRemar_B))

## Combine observed and counterfactual PTFRs ----
# observed PTFR
tbl_PSTFR_obs <-
  tbl_fx_obs |> 
  group_by(Year5y, BirthOrder, Weight) |> 
  reframe(PSTFR = sum(Comp2TFR) * 5)

tbl_TFR_obs <-
  tbl_PSTFR_obs |> 
  group_by(Year5y, Weight) |> 
  reframe(TFR = sum(PSTFR)) |> 
  pivot_wider(names_from  = Weight,
              values_from = TFR,
              names_prefix = "TFR_"
  )

# observed and counterfactual PTFR by parity
tbl_PSTFR_obs_cf_wide <-
  tbl_PSTFR_obs |> 
  filter(Weight == "wt") |> 
  select(Year5y, BirthOrder, PSTFR_obs = PSTFR) |> 
  left_join(PSTFR_NoDiv_A,   by = c("Year5y", "BirthOrder")) |> 
  left_join(PSTFR_NoDiv_B,   by = c("Year5y", "BirthOrder")) |> 
  left_join(PSTFR_NoRemar_A, by = c("Year5y", "BirthOrder")) |> 
  left_join(PSTFR_NoRemar_B, by = c("Year5y", "BirthOrder"))

tbl_PSTFR_obs_cf_long <-
  tbl_PSTFR_obs_cf_wide |> 
  pivot_longer(cols = starts_with("PSTFR"),
               names_to  = "Type",
               values_to = "PSTFR",
               names_prefix = "PSTFR_") |> 
  mutate(Type = case_match(Type,
                           "obs"       ~ "Observed",
                           "NoDiv_A"   ~ "No divorce A",
                           "NoDiv_B"   ~ "No divorce B",
                           "NoRemar_A" ~ "No remarriage A",
                           "NoRemar_B" ~ "No remarriage B"
  ) |> 
    fct_relevel("Observed", "No divorce A", "No divorce B", "No remarriage A", "No remarriage B")
  )

# parity progression ratio
tbl_PPR_obs_cf <-
  tbl_PSTFR_obs_cf_long |> 
  mutate(BirthOrder_3Grp = if_else(BirthOrder >= 3, 3L, BirthOrder) |> as.integer()) |> 
  group_by(Year5y, Type, BirthOrder_3Grp) |>
  reframe(PSTFR = sum(PSTFR)) |> 
  group_by(Year5y, Type) |> 
  mutate(PPR = PSTFR / lag(PSTFR),
         PPR = replace_na(PPR, PSTFR[BirthOrder_3Grp == 1])) |> 
  ungroup() |> 
  mutate(ParPro = case_match(BirthOrder_3Grp,
                             1 ~ "PPR(0,1)",
                             2 ~ "PPR(1,2)",
                             3 ~ "PPR(2,3)"))

# observed and counterfactual PTFR (total)
tbl_TFR_obs_cf_wide <-
  tbl_TFR_obs |> 
  left_join(TFR_NoDiv_A,   by = "Year5y") |> 
  left_join(TFR_NoDiv_B,   by = "Year5y") |> 
  left_join(TFR_NoRemar_A, by = "Year5y") |> 
  left_join(TFR_NoRemar_B, by = "Year5y")

tbl_TFR_obs_cf_long <-
  tbl_TFR_obs_cf_wide |> 
  pivot_longer(cols = starts_with("TFR"),
               names_to  = "Type",
               values_to = "TFR",
               names_prefix = "TFR_") |> 
  filter(Type != "uwt") |> 
  mutate(Type = case_match(Type,
                           "wt"        ~ "Observed",
                           "NoDiv_A"   ~ "No divorce A",
                           "NoDiv_B"   ~ "No divorce B",
                           "NoRemar_A" ~ "No remarriage A",
                           "NoRemar_B" ~ "No remarriage B"
                           ) |> 
                fct_relevel("Observed", "No divorce A", "No divorce B", "No remarriage A", "No remarriage B")
         )

## Present results for assumption set A ----
### Figure A1: Observed and counterfactual PTFRs ----
FigA1 <-
  tbl_TFR_obs_cf_long |> 
  filter(Year5y %in% c("2000-04", "2005-09", "2010-14", "2015-21")) |> 
  filter(Type %in% c("Observed", "No divorce A", "No remarriage A")) |> 
  ggplot(aes(x = Year5y,
             y = TFR,
             fill = fct_relevel(Type, "Observed", "No divorce A", "No remarriage A"))
  ) +
  geom_col(position="dodge", color = "black") +
  geom_text(aes(label = sprintf("%.2f", TFR)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 5) +
  ylim(0.0, 1.5) +
  scale_fill_manual(values=c("black", "gray50", "white")) +
  labs(
    x = "Year",
    y = "Period TFR",
    fill = NULL
  ) +
  guides(color    = guide_legend(NULL),
         linetype = guide_legend(NULL),
         shape    = guide_legend(NULL)
  ) + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text  = element_text(size = rel(1.5)),
        axis.text.x  = element_text(size = rel(1.5)),
        axis.text.y  = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))

print(FigA1)

ggsave(filename = paste0(OutDir_p, "FigureA1.pdf"),
       device = "pdf",
       width  = 9,
       height = 6)

### Figure A3: Observed and counterfactual parity progression ratios ----
FigA3 <-
  tbl_PPR_obs_cf |> 
  filter(Year5y %in% c("2000-04", "2005-09", "2010-14", "2015-21")) |> 
  filter(Type %in% c("Observed", "No divorce A", "No remarriage A")) |> 
  ggplot(aes(x = Year5y,
             y = PPR,
             fill = fct_relevel(Type, "Observed", "No divorce A", "No remarriage A"))
  ) +
  facet_grid(~ ParPro) +
  geom_col(position="dodge", color = "black") +
  geom_text(aes(label = sprintf("%.2f", PPR)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  ylim(0.0, 1.0) +
  scale_fill_manual(values=c("black", "gray50", "white")) +
  labs(
    x = "Year",
    y = "Parity Progression Ratio",
    fill = NULL
  ) +
  guides(color    = guide_legend(NULL),
         linetype = guide_legend(NULL),
         shape    = guide_legend(NULL)
  ) + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text  = element_text(size = rel(1.3)),
        strip.text   = element_text(size = rel(1.3)),
        axis.text.x  = element_text(size = rel(1.3)),
        axis.text.y  = element_text(size = rel(1.3)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))

print(FigA3)

ggsave(filename = paste0(OutDir_p, "FigureA3.pdf"),
       device = "pdf",
       width  = 12,
       height = 6)

## Present results for assumption set B ----
### Figure A2: Observed and counterfactual PTFRs ----
FigA2 <-
  tbl_TFR_obs_cf_long |> 
  filter(Year5y %in% c("2000-04", "2005-09", "2010-14", "2015-21")) |> 
  filter(Type %in% c("Observed", "No divorce B", "No remarriage B")) |> 
  ggplot(aes(x = Year5y,
             y = TFR,
             fill = fct_relevel(Type, "Observed", "No divorce B", "No remarriage B"))
  ) +
  geom_col(position="dodge", color = "black") +
  geom_text(aes(label = sprintf("%.2f", TFR)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 5) +
  ylim(0.0, 1.5) +
  scale_fill_manual(values=c("black", "gray50", "white")) +
  labs(
    x = "Year",
    y = "Period TFR",
    fill = NULL
  ) +
  guides(color    = guide_legend(NULL),
         linetype = guide_legend(NULL),
         shape    = guide_legend(NULL)
  ) + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text  = element_text(size = rel(1.5)),
        axis.text.x  = element_text(size = rel(1.5)),
        axis.text.y  = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))

print(FigA2)

ggsave(filename = paste0(OutDir_p, "FigureA2.pdf"),
       device = "pdf",
       width  = 9,
       height = 6)

### Figure A4: Observed and counterfactual parity progression ratios ----
FigA4 <-
  tbl_PPR_obs_cf |> 
  filter(Year5y %in% c("2000-04", "2005-09", "2010-14", "2015-21")) |> 
  filter(Type %in% c("Observed", "No divorce B", "No remarriage B")) |> 
  ggplot(aes(x = Year5y,
             y = PPR,
             fill = fct_relevel(Type, "Observed", "No divorce B", "No remarriage B"))
  ) +
  facet_grid(~ ParPro) +
  geom_col(position="dodge", color = "black") +
  geom_text(aes(label = sprintf("%.2f", PPR)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  ylim(0.0, 1.0) +
  scale_fill_manual(values=c("black", "gray50", "white")) +
  labs(
    x = "Year",
    y = "Parity Progression Ratio",
    fill = NULL
  ) +
  guides(color    = guide_legend(NULL),
         linetype = guide_legend(NULL),
         shape    = guide_legend(NULL)
  ) + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text  = element_text(size = rel(1.3)),
        strip.text   = element_text(size = rel(1.3)),
        axis.text.x  = element_text(size = rel(1.3)),
        axis.text.y  = element_text(size = rel(1.3)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))

print(FigA4)

ggsave(filename = paste0(OutDir_p, "FigureA4.pdf"),
       device = "pdf",
       width  = 12,
       height = 6)

## Figure A6 ----
# Observed completed cohort fertility and counterfactual values calculated 
# by holding marital status distributions constant at levels for the earliest (2000-04) period.
PropExp_t1 <-
  tbl_fx_obs |> 
  filter(Weight == "wt") |> 
  filter(Year5y == "2000-04") |> 
  select(CAge5y, MarStat, BirthOrder, PropExp)

tbl_PSTFR_cf_cons_ms <-
  tbl_fx_obs |> 
  filter(Weight == "wt") |> 
  select(Year5y, CAge5y, MarStat, BirthOrder, Pfx) |> 
  left_join(PropExp_t1, by = c("CAge5y", "MarStat", "BirthOrder")) |> 
  mutate(Comp2TFR = Pfx * PropExp) |> 
  group_by(Year5y, BirthOrder) |> 
  reframe(PSTFR_cf_cons_ms = sum(Comp2TFR) * 5)

tbl_TFR_cf_cons_ms <-
  tbl_PSTFR_cf_cons_ms |> 
  group_by(Year5y) |> 
  reframe(TFR_cf_cons_ms = sum(PSTFR_cf_cons_ms))

tbl_TFR_obs_cf_cons_ms <-
  tbl_TFR_obs |> 
  left_join(tbl_TFR_cf_cons_ms, by = "Year5y") |> 
  pivot_longer(cols = starts_with("TFR"),
               names_to  = "Type",
               values_to = "TFR",
               names_prefix = "TFR_")

tbl_TFR_obs_cf_cons_ms |> 
  filter(Type != "uwt") |> 
  group_by(Year5y) |> 
  mutate(Ratio2cf = TFR / TFR[Type == "wt"]) |> 
  filter(Type == "cf_cons_ms") |> 
  select(-Type)

FigA6 <-
  tbl_TFR_obs_cf_cons_ms |>  
  filter(Type != "uwt") |> 
  mutate(Type = if_else(Type == "wt", "Observed", "No change in marital status")) |> 
  ggplot(aes(x = Year5y,
             y = TFR,
             fill = fct_relevel(Type, "Observed", "No change in marital status"))
  ) +
  geom_col(position="dodge", color = "black") +
  geom_text(aes(label = sprintf("%.2f", TFR)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 5) +
  ylim(0.0, 2.0) +
  scale_fill_manual(values=c("black", "white")) +
  labs(
    x = "Year",
    y = "Period TFR",
    fill = NULL
  ) +
  guides(color    = guide_legend(NULL),
         linetype = guide_legend(NULL),
         shape    = guide_legend(NULL)
  ) + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text  = element_text(size = rel(1.3)),
        strip.text   = element_text(size = rel(1.3)),
        axis.text.x  = element_text(size = rel(1.3)),
        axis.text.y  = element_text(size = rel(1.3)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))

print(FigA6)

ggsave(filename = paste0(OutDir_p, "FigureA6.pdf"),
       device = "pdf",
       width  = 9,
       height = 6)
