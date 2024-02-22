################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Penalized Regression

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(glmnet)
library(broom)

##### Fit Lasso Regression Model ###############################################
# Select Data
df_lasso_tm1 <- df %>%
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
df_lasso_tm1 <- df_lasso_tm1 %>%
  na.omit()

# Extract Predictor Matrix and Response Vector
lasso_tm1_x <- model.matrix(FOL_TM1_TOTAL_LN ~ ., df_lasso_tm1)[,-1]
lasso_tm1_y <- df_lasso_tm1$FOL_TM1_TOTAL_LN

# Fit Lasso with Cross-validation
lasso_tm1_cv <- cv.glmnet(
  x = lasso_tm1_x,
  y = lasso_tm1_y, 
  alpha = 1
)

# Select Best Lambda
plot(lasso_tm1_cv)

# Refit Lasso with Best Lambda
lasso_tm1_md <- glmnet(
  x = lasso_tm1_x, 
  y = lasso_tm1_y, 
  alpha = 1, 
  lambda = lasso_tm1_cv$lambda.min, 
  standardize = FALSE
)

# Get Coefficients
tidy(lasso_tm1_md, return_zeros = TRUE)

