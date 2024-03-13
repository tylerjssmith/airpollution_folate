################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Table 2

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Plasma Total Folate by Covariates ########################################
# Overall
df %>%
  select(ends_with("TOTAL")) %>%
  pivot_longer(everything()) %>%
  na.omit() %>%
  group_by(name) %>%
  summarise(
    n = n(),
    median = median(value),
    q1 = quantile(value, 0.25),
    q3 = quantile(value, 0.75)
  )

# First Trimester
tmp_tbl2_tm1 <- rbind(
  df %>% make_table(x = AGE4,
    y = FOL_TM1_TOTAL),
  df %>% make_table(x = EDUCATION, 
    y = FOL_TM1_TOTAL),
  df %>% make_table(x = RACE, 
    y = FOL_TM1_TOTAL),
  df %>% make_table(x = INCOME, 
    y = FOL_TM1_TOTAL),
  df %>% make_table(x = HOUSEHOLD_SIZE, 
    y = FOL_TM1_TOTAL),
  df %>% make_table(x = BIRTH_COUNTRY, 
    y = FOL_TM1_TOTAL),
  df %>% make_table(x = FOLIC_ACID_CAT, 
    y = FOL_TM1_TOTAL),
  df %>% make_table(x = HEALTHY_EATING4,
    y = FOL_TM1_TOTAL)
)

# Third Trimester
tmp_tbl2_tm3 <- rbind(
  df %>% make_table(x = AGE4,
    y = FOL_TM3_TOTAL),
  df %>% make_table(x = EDUCATION, 
    y = FOL_TM3_TOTAL),
  df %>% make_table(x = RACE, 
    y = FOL_TM3_TOTAL),
  df %>% make_table(x = INCOME, 
    y = FOL_TM3_TOTAL),
  df %>% make_table(x = HOUSEHOLD_SIZE, 
    y = FOL_TM3_TOTAL),
  df %>% make_table(x = BIRTH_COUNTRY, 
    y = FOL_TM3_TOTAL),
  df %>% make_table(x = FOLIC_ACID_CAT, 
    y = FOL_TM3_TOTAL),
  df %>% make_table(x = HEALTHY_EATING4,
    y = FOL_TM3_TOTAL)
)

# Join Trimesters
tbl2 <- left_join(tmp_tbl2_tm1, tmp_tbl2_tm3, by = c("var","val"))

tbl2 %>% head()

##### Write Table ##############################################################
write_csv(tbl2, "tables/MixFol_Table2.csv", col_names = TRUE)

