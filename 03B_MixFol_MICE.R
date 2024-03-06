################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Run Multiple Imputation for Missing Data

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(mice)
library(geepack)
library(broom)

##### Set Imputation Parameters ################################################
df_period %>% head()

# Set Predictor Matrix
pred = make.predictorMatrix(df_period)

pred[1,] = 0
pred[,1] = 0

pred

##### Run Imputation Procedure #################################################
# Run Procedure
imp = mice(df_period, predictorMatrix = pred, m = 25, maxit = 10)

# Remove People with Missing Outcomes
df_mice = filter(imp, !is.na(FOL_TOTAL_LN))

##### Check Imputations ########################################################
densityplot(df_mice)
