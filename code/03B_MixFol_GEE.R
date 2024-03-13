################################################################################
# Air Pollution and Plasma Folate among Pregnant Women in Canada, 2008-2011
# Generalized Estimating Equations

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(geepack)

##### Overall ##################################################################
fit_gee = function(data, x, id = "SUBJECT_ID2", corstr = "exchangeable")
{
  fit = with(data, geeglm(FOL_TOTAL_LN ~ get(x) + 
      AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
      HEALTHY_EATING + SITE_ID + FOLIC_ACID3 + VISIT, 
    id = get(id), corstr = corstr))
  pool(fit)
}

fit_gee_no2 = imp_gee %>% 
  fit_gee(x = "NO2") %>% 
  summary() %>%
  mutate(
    x = "NO2", 
    model = "overall"
  )

fit_gee_o3 = imp_gee %>% 
  fit_gee(x = "O3") %>% 
  summary() %>%
  mutate(
    x = "O3", 
    model = "overall"
  )

fit_gee_pm25 = imp_gee %>% 
  fit_gee(x = "PM25") %>% 
  summary() %>%
  mutate(
    x = "PM25", 
    model = "overall"
  )

fit_gee_so2 = imp_gee %>% 
  fit_gee(x = "SO2") %>% 
  summary() %>%
  mutate(
    x = "SO2", 
    model = "overall"
  )

##### By Visit #################################################################
fit_gee_by_visit = function(data, x, id = "SUBJECT_ID2", corstr = "exchangeable")
{
  fit = with(data, geeglm(FOL_TOTAL_LN ~ get(x) * VISIT + 
      AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
      HEALTHY_EATING + SITE_ID + FOLIC_ACID3, 
    id = get(id), corstr = corstr))
  pool(fit)
}

fit_gee_by_visit_no2 = imp_gee %>% 
  fit_gee_by_visit(x = "NO2") %>% 
  summary() %>%
  mutate(
    x = "NO2", 
    model = "by_visit"
  )

fit_gee_by_visit_o3 = imp_gee %>% 
  fit_gee_by_visit(x = "O3") %>% 
  summary() %>%
  mutate(
    x = "O3", 
    model = "by_visit"
  )

fit_gee_by_visit_pm25 = imp_gee %>% 
  fit_gee_by_visit(x = "PM25") %>% 
  summary() %>%
  mutate(
    x = "PM25", 
    model = "by_visit"
  )

fit_gee_by_visit_so2 = imp_gee %>% 
  fit_gee_by_visit(x = "SO2") %>% 
  summary() %>%
  mutate(
    x = "SO2", 
    model = "by_visit"
  )

##### By Folic Acid Supplementation ############################################
# Three Levels
fit_gee_by_fa3 = function(data, x, id = "SUBJECT_ID2", corstr = "exchangeable")
{
  fit = with(data, geeglm(FOL_TOTAL_LN ~ get(x) * FOLIC_ACID3 + 
      AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
      HEALTHY_EATING + SITE_ID + VISIT, 
    id = get(id), corstr = corstr))
  pool(fit)
}

fit_gee_by_fa3_no2 = imp_gee %>% 
  fit_gee_by_fa3(x = "NO2") %>% 
  summary() %>%
  mutate(
    x = "NO2", 
    model = "by_fa3"
  )

fit_gee_by_fa3_o3 = imp_gee %>% 
  fit_gee_by_fa3(x = "O3") %>% 
  summary() %>%
  mutate(
    x = "O3", 
    model = "by_fa3"
  )

fit_gee_by_fa3_pm25 = imp_gee %>% 
  fit_gee_by_fa3(x = "PM25") %>% 
  summary() %>%
  mutate(
    x = "PM25", 
    model = "by_fa3"
  )

fit_gee_by_fa3_so2 = imp_gee %>% 
  fit_gee_by_fa3(x = "SO2") %>% 
  summary() %>%
  mutate(
    x = "SO2", 
    model = "by_fa3"
  )

##### Summarize Results ########################################################
rbind(fit_gee_no2,fit_gee_o3,fit_gee_pm25,fit_gee_so2) %>%
  filter(grepl("get", term)) %>%
  select(x, estimate:p.value)

rbind(fit_gee_by_visit_no2,fit_gee_by_visit_o3,fit_gee_by_visit_pm25,fit_gee_by_visit_so2) %>%
  filter(grepl("get", term)) %>%
  select(model, x, term, estimate:p.value)

rbind(fit_gee_by_fa3_no2,fit_gee_by_fa3_o3,fit_gee_by_fa3_pm25,fit_gee_by_fa3_so2) %>%
  filter(grepl("get", term)) %>%
  select(x, term, estimate:p.value)



