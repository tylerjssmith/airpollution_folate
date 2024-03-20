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
  mutate(TM1_MONTH = month(TM1_DATE)) %>%
  mutate(TM3_MONTH = month(TM3_DATE))

df_sub = df %>%
  select(
    # Administrative
    SUBJECT_ID,
    SITE_ID,
    TM1_MONTH,
    TM3_MONTH,
    
    # Outcomes: Trimester 1
    FOL_TM1_TOTAL_LN,
    FOL_TM1_5methylTHF_LN,
    FOL_TM1_NONMETHYL_LN,
    FOL_TM1_UMFA_LN,
    
    # Outcomes: Trimester 3
    FOL_TM3_TOTAL_LN, 
    FOL_TM3_5methylTHF_LN,
    FOL_TM3_NONMETHYL_LN,
    FOL_TM3_UMFA_LN,
    
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
m = 25

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
    cols = starts_with("FOL_"),
    names_to = "FOL_VIT",
    values_to = "FOL_CON_LN"
  )

imp_gee %>% head()

imp_gee %>% select(FOL_VIT, FOL_CON_LN)

# Set Subject ID
imp_gee = imp_gee %>%
  arrange(.imp, .id, SUBJECT_ID) %>%
  group_by(SUBJECT_ID) %>%
  mutate(SUBJECT_ID2 = cur_group_id()) %>% 
  ungroup()

imp_gee %>% head()

# Set Visit
imp_gee = imp_gee %>%
  mutate(VISIT = ifelse(grepl("TM1", FOL_VIT), "TM1", "TM3"))

imp_gee = imp_gee %>%
  mutate(FOL_VIT = gsub("TM1_", "", FOL_VIT)) %>%
  mutate(FOL_VIT = gsub("TM3_", "", FOL_VIT))

imp_gee %>% select(VISIT, FOL_VIT, FOL_CON_LN)

# Set Visit Month
imp_gee = imp_gee %>%
  mutate(VISIT_MONTH = ifelse(VISIT == "TM1", VISIT1_MONTH, VISIT3_MONTH))

# Set Exposure
imp_gee = imp_gee %>%
  mutate(NO2  = ifelse(VISIT == "TM1", NO2_V1,  NO2_V3))  %>%
  mutate(O3   = ifelse(VISIT == "TM1", O3_V1,   O3_V3))   %>%
  mutate(PM25 = ifelse(VISIT == "TM1", PM25_V1, PM25_V3)) %>%
  mutate(SO2  = ifelse(VISIT == "TM1", SO2_V1,  SO2_V3))

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
    FOL_VIT,
    FOL_CON_LN,
    
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
imp_gee %>%
  filter(.imp == 0) %>%
  group_by(VISIT, FOL_VIT) %>%
  count(!is.na(FOL_CON_LN)) %>%
  mutate(p = n / sum(n) * 100) %>%
  filter(`!is.na(FOL_CON_LN)`)

missing_fol_v1 = imp_gee %>%
  filter(.imp == 0) %>%
  filter(VISIT == "TM1" & is.na(FOL_CON_LN)) %>%
  pull(SUBJECT_ID)

missing_fol_v3 = imp_gee %>%
  filter(.imp == 0) %>%
  filter(VISIT == "TM3" & is.na(FOL_CON_LN)) %>%
  pull(SUBJECT_ID)

imp_gee = imp_gee %>%
  filter(
    (VISIT == "TM1" & (!SUBJECT_ID %in% missing_fol_v1)) |
    (VISIT == "TM3" & (!SUBJECT_ID %in% missing_fol_v3))
  )

imp_gee %>%
  filter(.imp == 0) %>%
  group_by(VISIT, FOL_VIT) %>%
  count(!is.na(FOL_CON_LN)) %>%
  mutate(p = n / sum(n) * 100) %>%
  filter(`!is.na(FOL_CON_LN)`)

# Transform to mids Object
imp_gee = imp_gee %>%
  pivot_wider(
    id_cols = c(.imp, .id, SUBJECT_ID, SUBJECT_ID2, SITE_ID, VISIT, VISIT_MONTH, 
      NO2, O3, PM25, SO2, AGE, EDUCATION, RACE, INCOME, HOUSEHOLD_SIZE, 
      BIRTH_COUNTRY, HEALTHY_EATING, FOLIC_ACID2, FOLIC_ACID3), 
    names_from = FOL_VIT, 
    values_from = FOL_CON_LN
  )

imp_gee = imp_gee %>%
  select(.imp, .id, SUBJECT_ID, SUBJECT_ID2, SITE_ID, VISIT, VISIT_MONTH, 
    starts_with("FOL_"), everything()) %>%
  arrange(.imp, .id, VISIT) %>%
  group_by(.imp) %>%
  mutate(.id = row_number()) %>%
  ungroup() %>%
  arrange(.imp,.id,SUBJECT_ID2) %>%
  as.mids()

complete(imp_gee, 1) %>%
  head()

saveRDS(imp_gee, file = "data/MixFol_Complete_Imputed_Long.rds")

# Check Sample Size by Imputation and Visit
imp_gee %>%
  complete(action = "long") %>%
  as_tibble() %>%
  group_by(.imp, VISIT) %>%
  summarise(n = n_distinct(SUBJECT_ID))






