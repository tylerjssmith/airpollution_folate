################################################################################
# Air Pollution and Plasma Folate among Pregnant Women in Canada, 2008-2011
# Multiple Imputation

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(mice)

##### Read Data ################################################################
df = readRDS("data/MixFol_Complete.rds")
df %>% head()

##### Select Data ##############################################################
# Coarsen Time Variables
df = df %>%
  mutate(VISIT1_MONTH = month(VISIT1_DATE)) %>%
  mutate(VISIT3_MONTH = month(VISIT3_DATE))

df_sub = df %>%
  select(
    # Administrative
    SUBJECT_ID,
    SITE_ID,
    VISIT1_MONTH,
    VISIT3_MONTH,
    
    # Outcomes
    FOL_TM1_TOTAL_LN, 
    FOL_TM3_TOTAL_LN, 
    
    # Exposures
    starts_with("NO2"), 
    starts_with("O3"), 
    starts_with("PM25"), 
    starts_with("SO2"),
    
    # Confounders
    AGE,
    EDUCATION,
    RACE,
    INCOME,
    HOUSEHOLD_SIZE,
    BIRTH_COUNTRY,
    HEALTHY_EATING,
    
    # Modifiers
    FOLIC_ACID3,
    FOLIC_ACID2
  )

##### Multiple Imputation ######################################################
# Imputations
m = 50

# Iterations
maxit = 100

# Predictor Matrix
pred = make.predictorMatrix(df_sub)
pred[c("SUBJECT_ID","FOLIC_ACID2"),] = pred[,c("SUBJECT_ID","FOLIC_ACID2")] = 0
pred

# Set Random Seed
set.seed(2708)

# Run MICE
imp = mice(df_sub, m = m, predictorMatrix = pred, maxit = maxit, 
  printFlag = TRUE)
imp$loggedEvents

saveRDS(imp, file = "data/MixFol_Complete_Imputed.rds")

# Check Imputations
pdf("figures/mice_traceplots.pdf")
plot(imp)
dev.off()

pdf("figures/mice_densityplots.pdf")
densityplot(imp)
dev.off()

##### Make Person-period Dataset ###############################################
# Pivot Longer
imp_gee = imp %>%
  complete(action = "long", include = TRUE) %>%
  pivot_longer(
    cols = c(FOL_TM1_TOTAL_LN, FOL_TM3_TOTAL_LN),
    names_to = "VISIT",
    values_to = "FOL_TOTAL_LN"
  )

# Set Subject ID
imp_gee = imp_gee %>%
  arrange(.imp, .id, SUBJECT_ID) %>%
  group_by(SUBJECT_ID) %>%
  mutate(SUBJECT_ID2 = cur_group_id()) %>% 
  ungroup()

# Set Visit
imp_gee = imp_gee %>%
  mutate(VISIT = ifelse(VISIT == "FOL_TM1_TOTAL_LN", "Visit1", "Visit3"))

# Set Visit Month
imp_gee = imp_gee %>%
  mutate(VISIT_MONTH = ifelse(VISIT == "Visit1", VISIT1_MONTH, VISIT3_MONTH))

# Set Exposure
imp_gee = imp_gee %>%
  mutate(NO2  = ifelse(VISIT == "Visit1", NO2_V1,  NO2_V3))  %>%
  mutate(O3   = ifelse(VISIT == "Visit1", O3_V1,   O3_V3))   %>%
  mutate(PM25 = ifelse(VISIT == "Visit1", PM25_V1, PM25_V3)) %>%
  mutate(SO2  = ifelse(VISIT == "Visit1", SO2_V1,  SO2_V3))

imp_gee = imp_gee %>%
  mutate(across(NO2:SO2, ~ log2(.x)))

# Select Variables
imp_gee = imp_gee %>%
  select(
    # MICE
    .imp,
    .id,
    
    # Administrative
    SUBJECT_ID,
    SUBJECT_ID2,
    SITE_ID,
    VISIT,
    VISIT_MONTH,
    
    # Outcome
    FOL_TOTAL_LN,
    
    # Exposure
    NO2,
    O3,
    PM25,
    SO2,
    
    # Confounders
    AGE,
    EDUCATION,
    RACE,
    INCOME,
    HOUSEHOLD_SIZE,
    BIRTH_COUNTRY,
    HEALTHY_EATING,
    
    # Modifiers
    FOLIC_ACID3,
    FOLIC_ACID2
  )

imp_gee %>% head()

# Remove Observations with Missing Outcomes
missing_fol_v1 = imp_gee %>%
  filter(.imp == 0) %>%
  filter(VISIT == "Visit1" & is.na(FOL_TOTAL_LN)) %>%
  pull(SUBJECT_ID)

missing_fol_v3 = imp_gee %>%
  filter(.imp == 0) %>%
  filter(VISIT == "Visit3" & is.na(FOL_TOTAL_LN)) %>%
  pull(SUBJECT_ID)

imp_gee = imp_gee %>%
  filter(
    (VISIT == "Visit1" & (!SUBJECT_ID %in% missing_fol_v1)) |
    (VISIT == "Visit3" & (!SUBJECT_ID %in% missing_fol_v3))
  )

# Transform to mids Object
imp_gee = imp_gee %>%
  arrange(.imp, .id, VISIT) %>%
  group_by(.imp) %>%
  mutate(.id = row_number()) %>%
  ungroup() %>%
  arrange(.imp,.id,SUBJECT_ID2) %>%
  as.mids()

# Check Sample Size by Imputation and Visit
imp_gee %>%
  complete(action = "long") %>%
  as_tibble() %>%
  group_by(.imp, VISIT) %>%
  summarise(n = n_distinct(SUBJECT_ID))






