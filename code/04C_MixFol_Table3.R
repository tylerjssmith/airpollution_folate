################################################################################
# Air Pollution and Plasma Folate among Pregnant Women in Canada
# Table 3 - Linear Regression Estimates

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

# Check Estimates
df_lm %>% head()

##### Make Table ###############################################################
tbl3 = df_lm %>%
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
tbl3 %>% head()

##### Export Table #############################################################
write_csv(tbl3, "tables/MixFol_Table3.csv", col_names = TRUE)

