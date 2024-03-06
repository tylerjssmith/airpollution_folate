################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Make Person-Period Data Set

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

df_period %>% head()

##### Prepare Variables ########################################################
# Subject ID
df_period = df_period %>%
  arrange(SUBJECT_ID, VISIT) %>%
  group_by(SUBJECT_ID) %>%
  mutate(SUBJECT_ID = cur_group_id()) %>%
  ungroup() %>%
  select(SUBJECT_ID, VISIT, everything())

df_period %>% head()

# Visit
df_period = df_period %>%
  mutate(
    VISIT =
      ifelse(VISIT == "FOL_TM1_TOTAL_LN", "Visit1",
      ifelse(VISIT == "FOL_TM3_TOTAL_LN", "Visit3", NA))) %>%
  select(SUBJECT_ID, VISIT, everything())

df_period %>% head()

# Air Pollution
df_period = df_period %>%
  mutate(NO2  = ifelse(VISIT == "Visit1", NO2_V1,  ifelse(VISIT == "Visit3", NO2_V3,  NA))) %>%
  mutate(O3   = ifelse(VISIT == "Visit1", O3_V1,   ifelse(VISIT == "Visit3", O3_V3,   NA))) %>%
  mutate(PM25 = ifelse(VISIT == "Visit1", PM25_V1, ifelse(VISIT == "Visit3", PM25_V3, NA))) %>%
  mutate(SO2  = ifelse(VISIT == "Visit1", SO2_V1,  ifelse(VISIT == "Visit3", SO2_V3,  NA)))

# Select Variables
df_period = df_period %>%
  select(SUBJECT_ID, VISIT, FOL_TOTAL_LN, NO2, O3, PM25, SO2, 
    AGE, EDUCATION, RACE, BIRTH_COUNTRY, INCOME, FOLIC_ACID, HEALTHY_EATING, SITE_ID) %>%
  arrange(SUBJECT_ID, VISIT)

df_period %>% head()
