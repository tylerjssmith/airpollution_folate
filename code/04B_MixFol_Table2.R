################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Table 2

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Overall ##################################################################
# Concentrations
df %>%
  select(starts_with("FOL_")) %>%
  select(-ends_with("_LN")) %>%
  select(-ends_with("_PROP")) %>%
  pivot_longer(everything()) %>%
  na.omit() %>%
  group_by(name) %>%
  summarise(
    n = n(),
    median = median(value),
    q1 = quantile(value, 0.25),
    q3 = quantile(value, 0.75)
  )

# Proportions
df %>%
  select(starts_with("FOL_")) %>%
  select(ends_with("_PROP")) %>%
  pivot_longer(everything()) %>%
  na.omit() %>%
  group_by(name) %>%
  summarise(
    n = n(),
    median = median(value),
    q1 = quantile(value, 0.25),
    q3 = quantile(value, 0.75)
  )

##### Plasma Total Folate by Covariates ########################################
df %>% head()

tbl2 = rbind(
  # Concentrations
  df %>% make_tbl(y = FOL_TM1_TOTAL, ylab = "FOL_TM1_TOTAL"),
  df %>% make_tbl(y = FOL_TM3_TOTAL, ylab = "FOL_TM3_TOTAL"),

  df %>% make_tbl(y = FOL_TM1_5methylTHF, ylab = "FOL_TM1_5methylTHF"),
  df %>% make_tbl(y = FOL_TM3_5methylTHF, ylab = "FOL_TM3_5methylTHF"),

  df %>% make_tbl(y = FOL_TM1_NONMETHYL, ylab = "FOL_TM1_NONMETHYL"),
  df %>% make_tbl(y = FOL_TM3_NONMETHYL, ylab = "FOL_TM3_NONMETHYL"),

  df %>% make_tbl(y = FOL_TM1_UMFA, ylab = "FOL_TM1_UMFA"),
  df %>% make_tbl(y = FOL_TM3_UMFA, ylab = "FOL_TM3_UMFA"),
  
  # Proportions
  df %>% make_tbl(y = FOL_TM1_5methylTHF_PROP, ylab = "FOL_TM1_5methylTHF_PROP"),
  df %>% make_tbl(y = FOL_TM3_5methylTHF_PROP, ylab = "FOL_TM3_5methylTHF_PROP"),

  df %>% make_tbl(y = FOL_TM1_NONMETHYL_PROP, ylab = "FOL_TM1_NONMETHYL_PROP"),
  df %>% make_tbl(y = FOL_TM3_NONMETHYL_PROP, ylab = "FOL_TM3_NONMETHYL_PROP"),

  df %>% make_tbl(y = FOL_TM1_UMFA_PROP, ylab = "FOL_TM1_UMFA_PROP"),
  df %>% make_tbl(y = FOL_TM3_UMFA_PROP, ylab = "FOL_TM3_UMFA_PROP")
)

tbl2 %>% head()

##### Write Table ##############################################################
write_csv(tbl2, "tables/MixFol_Table2.csv", col_names = TRUE)

