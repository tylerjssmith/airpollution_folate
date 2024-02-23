################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Penalized Regression

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(glmnet)
library(broom)
library(car)

# Set Options
options(scipen = 999)

##### Fit Lasso Regression Model ###############################################
# Select Data
df_shrink_tm1 <- df %>%
  select(
    FOL_TM1_TOTAL_LN, 
    SO2_TRIM1_L2,
    AGE, 
    EDUCATION,
    RACE,
    INCOME,
    HOUSEHOLD_SIZE,
    BIRTH_COUNTRY, 
    FOLIC_ACID_CAT,
    HEALTHY_EATING,
    SITE_ID
  )

# Drop Incomplete Cases
df_shrink_tm1 <- df_shrink_tm1 %>%
  na.omit()

# Extract Predictor Matrix and Response Vector
shrink_tm1_x <- model.matrix(FOL_TM1_TOTAL_LN ~ ., df_shrink_tm1)[,-1]
shrink_tm1_y <- df_shrink_tm1$FOL_TM1_TOTAL_LN

head(shrink_tm1_x)
head(shrink_tm1_y)

##### Ridge Regression #########################################################
# Fit Ridge Regression with Cross-validation
ridge_tm1_cv <- cv.glmnet(
  x = shrink_tm1_x,
  y = shrink_tm1_y, 
  alpha = 0
)

# Select Best Lambda
plot(ridge_tm1_cv)

# Refit Ridge Regression with Best Lambda
ridge_tm1_md <- glmnet(
  x = shrink_tm1_x, 
  y = shrink_tm1_y, 
  alpha = 0, 
  lambda = ridge_tm1_cv$lambda.min, 
  standardize = FALSE
)

##### Lasso ####################################################################
# Fit Lasso with Cross-validation
lasso_tm1_cv <- cv.glmnet(
  x = shrink_tm1_x,
  y = shrink_tm1_y, 
  alpha = 1
)

# Select Best Lambda
plot(lasso_tm1_cv)

# Refit Lasso with Best Lambda
lasso_tm1_md <- glmnet(
  x = shrink_tm1_x, 
  y = shrink_tm1_y, 
  alpha = 1, 
  lambda = lasso_tm1_cv$lambda.min, 
  standardize = FALSE
)

##### Elastic Net ##############################################################
# Fit Elastic Net with Cross-validation
elast_tm1_cv <- cv.glmnet(
  x = shrink_tm1_x,
  y = shrink_tm1_y, 
  alpha = 0.5
)

# Select Best Lambda
plot(elast_tm1_cv)

# Refit Lasso with Best Lambda
elast_tm1_md <- glmnet(
  x = shrink_tm1_x, 
  y = shrink_tm1_y, 
  alpha = 0.5, 
  lambda = elast_tm1_cv$lambda.min, 
  standardize = FALSE
)

##### Get Coefficients #########################################################
# Fit Ordinary Least Squares for Comparison
summary(ols_tm1_md <- lm(FOL_TM1_TOTAL_LN ~ SO2_TRIM1_L2 + 
    AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
    FOLIC_ACID_CAT + HEALTHY_EATING + SITE_ID, 
  data = df_shrink_tm1))

vif(ols_tm1_md)

# Compile Results
shrink_results <- cbind(
  # Ordinary Least Squares
  tidy(ols_tm1_md) %>% 
    select(term, estimate) %>%
    rename(ols = estimate),
  
  # Ridge Regression
  tidy(ridge_tm1_md, return_zeros = TRUE) %>% 
    select(estimate) %>%
    rename(ridge = estimate),
  
  # Lasso
  tidy(lasso_tm1_md, return_zeros = TRUE) %>% 
    select(estimate) %>%
    rename(lasso = estimate),
  
  # Elastic Net
  tidy(elast_tm1_md, return_zeros = TRUE) %>% 
    select(estimate) %>%
    rename(elastic = estimate)
)

shrink_results <- shrink_results %>%
  mutate(term = gsub("shrink_tm1_x", "", term))

shrink_results <- shrink_results %>%
  mutate(across(-term, ~ round(.x, 3)))

shrink_results


