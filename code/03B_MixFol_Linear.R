################################################################################
# Air Pollution and Plasma Folate among Pregnant Women in Canada
# Fit Linear Regression Models

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(broom)

##### Prepare Data #############################################################
# Read Data
imp_post = read_rds("data/MixFol_Complete_Imputed_Long.rds")

# Check Data
imp_post %>% complete(action = 1) %>% head()

##### Fit Linear Regression Models #############################################
# Set Vectors of Visits, Outcomes, and Exposures
tmp_lm_v = c("TM1","TM3")
tmp_lm_y = c("FOL_TOTAL_LN","FOL_5methylTHF_LN","FOL_NONMETHYL_LN","FOL_UMFA_LN")
tmp_lm_x = c("NO2","O3","PM25","SO2")

# Initialize Tibble
df_lm = tibble()

# Fit Models
for(v in seq_along(tmp_lm_v)) {

  for(i in seq_along(tmp_lm_y)) {
  
    for(j in seq_along(tmp_lm_x)) {
  
    # Unadjusted
    fit_unaj = with(filter(imp_post, VISIT == tmp_lm_v[v]), 
      lm(get(tmp_lm_y[i]) ~ get(tmp_lm_x[j])))
    
    df_unaj = fit_unaj %>%
      pool() %>%
      tidy(conf.int = TRUE) %>%
      mutate(visit = tmp_lm_v[v]) %>%
      mutate(y = tmp_lm_y[i]) %>%
      mutate(x = tmp_lm_x[j]) %>%
      mutate(adj = "Unadjusted")
    
    # Adjusted
    fit_adj1 = with(filter(imp_post, VISIT == tmp_lm_v[v]), 
      lm(get(tmp_lm_y[i]) ~ get(tmp_lm_x[j]) + 
        AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
        HEALTHY_EATING + FOLIC_ACID3 + SITE_ID))
    
    df_adj1 = fit_adj1 %>%
      pool() %>%
      tidy(conf.int = TRUE) %>%
      mutate(visit = tmp_lm_v[v]) %>%
      mutate(y = tmp_lm_y[i]) %>%
      mutate(x = tmp_lm_x[j]) %>%
      mutate(adj = "Adjusted")
    
    # Compile Estimates
    df_lm_ij = rbind(
      df_unaj,
      df_adj1
    )
      
    df_lm = rbind(
      df_lm,
      df_lm_ij
    )
    
    rm(list = c("fit_unaj","fit_adj1","df_unaj","df_adj1","df_lm_ij"))
    
    }
  }
}

# Select Estimates
df_lm = df_lm %>%
  filter(term == "get(tmp_lm_x[j])") %>%
  select(visit, y, x, adj, estimate, conf.low, conf.high, p.value)

df_lm %>% head()

