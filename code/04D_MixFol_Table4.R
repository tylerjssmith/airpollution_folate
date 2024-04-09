################################################################################
# Air Pollution and Plasma Folate among Pregnant Women in Canada
# Table 4 - Beta Regression Estimates

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

# Check Estimates
df_bt %>% head()

##### Make Table ###############################################################
tbl4 = df_bt %>%
  # select relevant variables
  select(
    visit, 
    yf, 
    xf, 
    estimate, 
    conf.low, 
    conf.high, 
    p.value, 
    adj
  ) %>%
  
  # format estimate columns
  mutate(across(c(estimate, conf.low, conf.high), ~ format(round(.x, digits = 2), nsmall = 2))) %>%
  mutate(beta_95ci = paste0(estimate, " (", conf.low, ",", conf.high, ")")) %>%
  
  # drop components of estimate columns
  select(
    visit, 
    yf, 
    xf, 
    beta_95ci, 
    adj
  ) %>%
  
  # pivot wider
  pivot_wider(
    id_cols = c(visit, yf, xf), 
    names_from = adj, 
    values_from = beta_95ci
  ) %>%
  
  pivot_wider(
    id_cols = c(yf, xf), 
    names_from = visit, 
    values_from = c(Unadjusted, Adjusted)
  )

# Check Table
tbl4 %>% head()

##### Export Table #############################################################
write_csv(tbl4, "tables/MixFol_Table4.csv", col_names = TRUE)

