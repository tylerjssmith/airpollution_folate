################################################################################
# Air Pollution and Plasma Folate among Pregnant Women in Canada, 2008-2011
# Linear Regression

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

# Remove Observations with Missing Outcome
imp_lm_v1 = imp %>% 
  filter(!is.na(FOL_TM1_TOTAL_LN))
imp_lm_v3 = imp %>% 
  filter(!is.na(FOL_TM3_TOTAL_LN))

##### By Folic Acid Supplementation ############################################
fit_lm_by_fa3 = function(data, x, y = "FOL_TM1_TOTAL_LN")
{
  fit = with(data, lm(get(y) ~ get(x) * FOLIC_ACID3 + 
      AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
      HEALTHY_EATING + SITE_ID))
  pool(fit)
}

fit_lm_by_fa2 = function(data, x, y = "FOL_TM1_TOTAL_LN")
{
  fit = with(data, lm(get(y) ~ get(x) * FOLIC_ACID2 + 
      AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
      HEALTHY_EATING + SITE_ID))
  pool(fit)
}

# Visit 1
imp_lm_v1 %>% 
  fit_lm_by_fa2(x = "NO2_V1") %>% 
  summary()

imp_lm_v1 %>% 
  fit_lm_by_fa2(x = "O3_V1") %>% 
  summary()

imp_lm_v1 %>% 
  fit_lm_by_fa2(x = "PM25_V1") %>% 
  summary()

imp_lm_v1 %>% 
  fit_lm_by_fa2(x = "SO2_V1") %>% 
  summary()

# Visit 3
imp_lm_v1 %>% 
  fit_lm_by_fa2(
    x = "NO2_V3", 
    y = "FOL_TM3_TOTAL_LN") %>% 
  summary()

imp_lm_v1 %>% 
  fit_lm_by_fa2(
    x = "O3_V3", 
    y = "FOL_TM3_TOTAL_LN") %>% 
  summary()

imp_lm_v1 %>% 
  fit_lm_by_fa2(
    x = "PM25_V3", 
    y = "FOL_TM3_TOTAL_LN") %>% 
  summary()

imp_lm_v1 %>% 
  fit_lm_by_fa2(
    x = "SO2_V3", 
    y = "FOL_TM3_TOTAL_LN") %>% 
  summary()

