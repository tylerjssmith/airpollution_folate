################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Linear Models

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(splines)
library(car)

##### Nitrous Oxide ############################################################
# Trimester 1
summary(lm_no2_tm1_md1 <- lm(FOL_TM1_TOTAL_LN ~ NO2_TRIM1_L2, 
  data = df))

summary(lm_no2_tm1_md2 <- lm(FOL_TM1_TOTAL_LN ~ NO2_TRIM1_L2 + 
    AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
    FOLIC_ACID_CAT + HEALTHY_EATING + SITE_ID, 
  data = df))

residualPlots(lm_no2_tm1_md1)
residualPlots(lm_no2_tm1_md2)

vif(lm_no2_tm1_md2)

##### Ozone ####################################################################
# Trimester 1
summary(lm_o3_tm1_md1 <- lm(FOL_TM1_TOTAL_LN ~ O3_TRIM1_L2, 
  data = df))

summary(lm_o3_tm1_md2 <- lm(FOL_TM1_TOTAL_LN ~ O3_TRIM1_L2 + 
    AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
    FOLIC_ACID_CAT + HEALTHY_EATING + SITE_ID, 
  data = df))

residualPlots(lm_o3_tm1_md1)
residualPlots(lm_o3_tm1_md2)

vif(lm_o3_tm1_md2)

##### Particulate Matter #######################################################
# Trimester 1
summary(lm_pm25_tm1_md1 <- lm(FOL_TM1_TOTAL_LN ~ PM25_TRIM1_L2, 
  data = df))

summary(lm_pm25_tm1_md2 <- lm(FOL_TM1_TOTAL_LN ~ PM25_TRIM1_L2 + 
    AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
    FOLIC_ACID_CAT + HEALTHY_EATING + SITE_ID, 
  data = df))

residualPlots(lm_pm25_tm1_md1)
residualPlots(lm_pm25_tm1_md2)

vif(lm_pm25_tm1_md2)

##### Sulfur Dioxide ###########################################################
# Trimester 1
summary(lm_so2_tm1_md1 <- lm(FOL_TM1_TOTAL_LN ~ SO2_TRIM1_L2, 
  data = df))

summary(lm_so2_tm1_md2 <- lm(FOL_TM1_TOTAL_LN ~ SO2_TRIM1_L2 + 
    AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
    FOLIC_ACID_CAT + HEALTHY_EATING + SITE_ID, 
  data = df))

residualPlots(lm_so2_tm1_md1)
residualPlots(lm_so2_tm1_md2)

vif(lm_so2_tm1_md2)




