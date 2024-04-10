################################################################################
# Air Pollution and Plasma Folate among Pregnant Women in Canada
# Fit Beta Regression Models

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(broom)
library(betareg)

##### Prepare Data #############################################################
# Read Data
imp_post = read_rds("data/MixFol_Complete_Imputed_Long.rds")

# Check Data
imp_post %>% complete(action = 1) %>% head()

##### Fit Beta Regression Models ###############################################
# Set Vectors of Visits, Outcomes, and Exposures
tmp_bt_v = c("TM1","TM3")
tmp_bt_y = c("FOL_5methylTHF_PR","FOL_NONMETHYL_PR","FOL_UMFA_PR")
tmp_bt_x = c("NO2","O3","PM25","SO2")

# Initialize Tibble
df_bt = tibble()

# Fit Models
for( v in seq_along(tmp_bt_v)) {

  for(i in seq_along(tmp_bt_y)) {
  
    for(j in seq_along(tmp_bt_x)) {
  
    # Unadjusted
    fit_unaj = with(filter(imp_post, VISIT == tmp_bt_v[v]), 
      betareg(get(tmp_bt_y[i]) ~ get(tmp_bt_x[j]) | FOL_TOTAL_LN))
    
    df_unaj = fit_unaj %>%
      pool() %>%
      tidy(conf.int = TRUE) %>%
      mutate(visit = tmp_bt_v[v]) %>%
      mutate(y = tmp_bt_y[i]) %>%
      mutate(x = tmp_bt_x[j]) %>%
      mutate(adj = "Unadjusted")
    
    # Adjusted
    fit_adj1 = with(filter(imp_post, VISIT == tmp_bt_v[v]), 
      betareg(get(tmp_bt_y[i]) ~ get(tmp_bt_x[j]) + 
        AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
        HEALTHY_EATING + FOLIC_ACID3 + SITE_ID | FOL_TOTAL_LN))
    
    df_adj1 = fit_adj1 %>%
      pool() %>%
      tidy(conf.int = TRUE) %>%
      mutate(visit = tmp_bt_v[v]) %>%
      mutate(y = tmp_bt_y[i]) %>%
      mutate(x = tmp_bt_x[j]) %>%
      mutate(adj = "Adjusted")
    
    # Compile Estimates
    df_bt_ij = rbind(
      df_unaj,
      df_adj1
    )
      
    df_bt = rbind(
      df_bt,
      df_bt_ij
    )
    
    rm(list = c("fit_unaj","fit_adj1","df_unaj","df_adj1","df_bt_ij"))
    
    }
  }
}

# Select Estimates
df_bt = df_bt %>%
  filter(term == "get(tmp_bt_x[j])") %>%
  select(visit, y, x, adj, estimate, conf.low, conf.high, p.value)

df_bt %>% head()
