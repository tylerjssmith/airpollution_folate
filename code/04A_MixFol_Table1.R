################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Table 1

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

# Check Data
df %>% head()

##### Overall ##################################################################
df %>%
  select(ends_with("_V1"), ends_with("_V3")) %>%
  select(-starts_with("n_")) %>%
  pivot_longer(everything()) %>%
  na.omit() %>%
  group_by(name) %>%
  summarise(
    n = n(),
    median = median(value),
    q1 = quantile(value, 0.25),
    q3 = quantile(value, 0.75)
  )

##### Air Pollution by Covariates ##############################################
df %>% head()

tbl1 = rbind(
  df %>% make_tbl(y = NO2_V1, ylab = "NO2_V1"),
  df %>% make_tbl(y = NO2_V3, ylab = "NO2_V3"),

  df %>% make_tbl(y = O3_V1, ylab = "O3_V1"),
  df %>% make_tbl(y = O3_V3, ylab = "O3_V3"),

  df %>% make_tbl(y = PM25_V1, ylab = "PM25_V1"),
  df %>% make_tbl(y = PM25_V3, ylab = "PM25_V3"),

  df %>% make_tbl(y = SO2_V1, ylab = "SO2_V1"),
  df %>% make_tbl(y = SO2_V3, ylab = "SO2_V3")
)

tbl1 %>% head()

##### Write Table ##############################################################
write_csv(tbl1, "tables/MixFol_Table1.csv", col_names = TRUE)
