################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Join Data

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Table 2: Plasma Total Folate by Covariates ###############################
# Overall
df %>%
  select(ends_with("TRIM1")) %>%
  pivot_longer(everything()) %>%
  na.omit() %>%
  group_by(name) %>%
  summarise(
    n = n(),
    median = median(value),
    q1 = quantile(value, 0.25),
    q3 = quantile(value, 0.75)
  )

# NO2
tmp_tbl1_no2 <- rbind(
  df %>% make_table(x = AGE4,
    y = NO2_TRIM1),
  df %>% make_table(x = EDUCATION, 
    y = NO2_TRIM1),
  df %>% make_table(x = RACE, 
    y = NO2_TRIM1),
  df %>% make_table(x = INCOME, 
    y = NO2_TRIM1),
  df %>% make_table(x = HOUSEHOLD_SIZE, 
    y = NO2_TRIM1),
  df %>% make_table(x = BIRTH_COUNTRY, 
    y = NO2_TRIM1),
  df %>% make_table(x = FOLIC_ACID_CAT, 
    y = NO2_TRIM1),
  df %>% make_table(x = HEALTHY_EATING4,
    y = NO2_TRIM1)
)

tmp_tbl1_no2 %>% head()

# O3
tmp_tbl1_o3 <- rbind(
  df %>% make_table(x = AGE4,
    y = O3_TRIM1),
  df %>% make_table(x = EDUCATION, 
    y = O3_TRIM1),
  df %>% make_table(x = RACE, 
    y = O3_TRIM1),
  df %>% make_table(x = INCOME, 
    y = O3_TRIM1),
  df %>% make_table(x = HOUSEHOLD_SIZE, 
    y = O3_TRIM1),
  df %>% make_table(x = BIRTH_COUNTRY, 
    y = O3_TRIM1),
  df %>% make_table(x = FOLIC_ACID_CAT, 
    y = O3_TRIM1),
  df %>% make_table(x = HEALTHY_EATING4,
    y = O3_TRIM1)
)

tmp_tbl1_o3 %>% head()

# PM2.5
tmp_tbl1_pm25 <- rbind(
  df %>% make_table(x = AGE4,
    y = PM25_TRIM1),
  df %>% make_table(x = EDUCATION, 
    y = PM25_TRIM1),
  df %>% make_table(x = RACE, 
    y = PM25_TRIM1),
  df %>% make_table(x = INCOME, 
    y = PM25_TRIM1),
  df %>% make_table(x = HOUSEHOLD_SIZE, 
    y = PM25_TRIM1),
  df %>% make_table(x = BIRTH_COUNTRY, 
    y = PM25_TRIM1),
  df %>% make_table(x = FOLIC_ACID_CAT, 
    y = PM25_TRIM1),
  df %>% make_table(x = HEALTHY_EATING4,
    y = PM25_TRIM1)
)

tmp_tbl1_pm25 %>% head()

# SO2
tmp_tbl1_so2 <- rbind(
  df %>% make_table(x = AGE4,
    y = SO2_TRIM1),
  df %>% make_table(x = EDUCATION, 
    y = SO2_TRIM1),
  df %>% make_table(x = RACE, 
    y = SO2_TRIM1),
  df %>% make_table(x = INCOME, 
    y = SO2_TRIM1),
  df %>% make_table(x = HOUSEHOLD_SIZE, 
    y = SO2_TRIM1),
  df %>% make_table(x = BIRTH_COUNTRY, 
    y = SO2_TRIM1),
  df %>% make_table(x = FOLIC_ACID_CAT, 
    y = SO2_TRIM1),
  df %>% make_table(x = HEALTHY_EATING4,
    y = SO2_TRIM1)
)
    
tmp_tbl1_so2 %>% head()

# Join Pollutants
tbl1 <- left_join(tmp_tbl1_no2, tmp_tbl1_o3, by = c("var","val"))
tbl1 <- left_join(tbl1, tmp_tbl1_pm25, by = c("var","val"))
tbl1 <- left_join(tbl1, tmp_tbl1_so2, by = c("var","val"))

tbl1 %>% head()

##### Write Table ##############################################################
write_csv(tbl1, "../Analysis/MixFol_Table1.csv", col_names = TRUE)
