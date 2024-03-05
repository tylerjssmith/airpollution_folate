################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Generalized Estimating Equations

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(geepack)
library(broom)

##### Select Data ##############################################################
df_period = df %>%
  select(
    SUBJECT_ID,
    FOL_TM1_TOTAL_LN,
    FOL_TM3_TOTAL_LN,
    contains("NO2"),
    contains("O3"),
    contains("PM25"),
    contains("SO2"),
    AGE,
    EDUCATION,
    RACE,
    BIRTH_COUNTRY,
    INCOME,
    FOLIC_ACID = FOLIC_ACID_CAT,
    HEALTHY_EATING,
    SITE_ID
  )

##### Pivot Longer to Person-Period Dataset ####################################
# Pivot Longer
df_period = df_period %>%
  pivot_longer(
    cols = c(FOL_TM1_TOTAL_LN, FOL_TM3_TOTAL_LN), 
    names_to = "VISIT", 
    values_to = "FOL_TOTAL_LN"
  )

# Arrange Columns
df_period = df_period %>%
  select(SUBJECT_ID, VISIT, FOL_TOTAL_LN, everything())

# Name Visits
df_period = df_period %>%
  mutate(
    VISIT =
      ifelse(VISIT == "FOL_TM1_TOTAL_LN", "1",
      ifelse(VISIT == "FOL_TM3_TOTAL_LN", "3", NA))
  )

df_period %>% head()

df_period = df_period %>%
  arrange(SUBJECT_ID, VISIT)

df_period %>% head()
