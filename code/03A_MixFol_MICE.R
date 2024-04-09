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
  mutate(TM1_MONTH = month(VISIT1_DATE)) %>%
  mutate(TM3_MONTH = month(VISIT3_DATE))

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
mice_time = system.time({
  
  imp = mice(df_sub, m = m, predictorMatrix = pred, maxit = maxit, 
    printFlag = TRUE)
  
})

saveRDS(imp, file = "data/MixFol_Complete_Imputed.rds")
write(mice_time, file = "data/mice_time.txt")

# Check Logged Events
imp$loggedEvents

# Check Imputations
pdf("figures/mice_traceplots.pdf")
plot(imp)
dev.off()

pdf("figures/mice_densityplots.pdf")
densityplot(imp)
dev.off()

##### Make Long Dataset ########################################################
# Read Imputation Object
imp = readRDS("data/MixFol_Complete_Imputed.rds")

# Pivot Longer
imp_tmp = imp %>%
  complete(action = "long", include = TRUE) %>%
  pivot_longer(
    cols = starts_with("FOL_"),
    names_to = "FOL_VIT",
    values_to = "FOL_CON_LN"
  )

imp_tmp %>% head()
imp_tmp %>% select(FOL_VIT, FOL_CON_LN)

# Set Subject ID
imp_tmp = imp_tmp %>%
  arrange(.imp, .id, SUBJECT_ID) %>%
  group_by(SUBJECT_ID) %>%
  mutate(SUBJECT_ID2 = cur_group_id()) %>% 
  ungroup()

imp_tmp %>% head()

# Set Visit
imp_tmp = imp_tmp %>%
  mutate(VISIT = ifelse(grepl("TM1", FOL_VIT), "TM1", "TM3"))

imp_tmp = imp_tmp %>%
  mutate(FOL_VIT = gsub("TM1_", "", FOL_VIT)) %>%
  mutate(FOL_VIT = gsub("TM3_", "", FOL_VIT))

imp_tmp %>% select(VISIT, FOL_VIT, FOL_CON_LN)

# Set Visit Month
imp_tmp = imp_tmp %>%
  mutate(VISIT_MONTH = ifelse(VISIT == "TM1", TM1_MONTH, TM3_MONTH))

# Set Exposure
imp_tmp = imp_tmp %>%
  mutate(NO2  = ifelse(VISIT == "TM1", NO2_V1,  NO2_V3))  %>%
  mutate(O3   = ifelse(VISIT == "TM1", O3_V1,   O3_V3))   %>%
  mutate(PM25 = ifelse(VISIT == "TM1", PM25_V1, PM25_V3)) %>%
  mutate(SO2  = ifelse(VISIT == "TM1", SO2_V1,  SO2_V3))

imp_tmp = imp_tmp %>%
  mutate(across(NO2:SO2, ~ log2(.x)))

# Select Variables
imp_tmp = imp_tmp %>%
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

imp_tmp %>% head()

# Remove Observations with Missing Outcomes
imp_tmp %>%
  filter(.imp == 0) %>%
  group_by(VISIT, FOL_VIT) %>%
  count(!is.na(FOL_CON_LN)) %>%
  mutate(p = n / sum(n) * 100) %>%
  filter(`!is.na(FOL_CON_LN)`)

missing_fol_v1 = imp_tmp %>%
  filter(.imp == 0) %>%
  filter(VISIT == "TM1" & is.na(FOL_CON_LN)) %>%
  pull(SUBJECT_ID)

missing_fol_v3 = imp_tmp %>%
  filter(.imp == 0) %>%
  filter(VISIT == "TM3" & is.na(FOL_CON_LN)) %>%
  pull(SUBJECT_ID)

imp_tmp = imp_tmp %>%
  filter(
    (VISIT == "TM1" & (!SUBJECT_ID %in% missing_fol_v1)) |
    (VISIT == "TM3" & (!SUBJECT_ID %in% missing_fol_v3))
  )

imp_tmp %>%
  filter(.imp == 0) %>%
  group_by(VISIT, FOL_VIT) %>%
  count(!is.na(FOL_CON_LN)) %>%
  mutate(p = n / sum(n) * 100) %>%
  filter(`!is.na(FOL_CON_LN)`)

# Pivot Wider
imp_tmp = imp_tmp %>%
  pivot_wider(
    id_cols = c(.imp, .id, SUBJECT_ID, SUBJECT_ID2, SITE_ID, VISIT, VISIT_MONTH, 
      NO2, O3, PM25, SO2, AGE, EDUCATION, RACE, INCOME, HOUSEHOLD_SIZE, 
      BIRTH_COUNTRY, HEALTHY_EATING, FOLIC_ACID2, FOLIC_ACID3), 
    names_from = FOL_VIT, 
    values_from = FOL_CON_LN
  )

# Calculate Folate Vitamer Proportions
imp_tmp %>%
  mutate(FOL_5methylTHF_PR = exp(FOL_5methylTHF_LN) / exp(FOL_TOTAL_LN)) %>%
  mutate(FOL_NONMETHYL_PR  = exp(FOL_NONMETHYL_LN ) / exp(FOL_TOTAL_LN)) %>%
  mutate(FOL_UMFA_PR       = exp(FOL_UMFA_LN      ) / exp(FOL_TOTAL_LN))

# (Check Proportions)
tmp = df %>%
  complete(action = "long") %>%
  select(.imp, VISIT, ends_with("_PR")) %>%
  pivot_longer(ends_with("_PR"))

tmp %>%
  group_by(.imp, VISIT, name) %>%
  summarise(
    min = min(value),
    max = max(value)
  ) %>%
  filter(min < 0 | max > 1)

rm(tmp)

# Transform to mids Object
imp_tmp = imp_tmp %>%
  select(.imp, .id, SUBJECT_ID, SUBJECT_ID2, SITE_ID, VISIT, VISIT_MONTH, 
    starts_with("FOL_"), everything()) %>%
  arrange(.imp, .id, VISIT) %>%
  group_by(.imp) %>%
  mutate(.id = row_number()) %>%
  ungroup() %>%
  arrange(.imp,.id,SUBJECT_ID2) %>%
  as.mids()

complete(imp_tmp, 1) %>%
  head()

# Check Sample Size by Imputation and Visit
imp_tmp %>%
  complete(action = "long") %>%
  as_tibble() %>%
  group_by(.imp, VISIT) %>%
  summarise(n = n_distinct(SUBJECT_ID))

# Export
saveRDS(imp_tmp, file = "data/MixFol_Complete_Imputed_Long.rds")




