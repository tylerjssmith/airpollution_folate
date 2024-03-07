################################################################################
# Air Pollution and Plasma Folate
# Make File

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Set-up ###################################################################
# Functions
source("code/01A_MixFol_Functions.R")

##### Read Data ################################################################
df <- readRDS("data/MixFol_Complete.rds")

df %>% head()

##### Exploratory Data Analysis ################################################
# Explore
source("code/02A_MixFol_Explore.R")

##### Regression Models ########################################################
# Make Person-Period Data
source("code/03A_MixFol_PersonPeriod.R")

# Run Multiple Imputation
source("code/03B_MixFol_MICE.R")

# Fit Generalized Estimating Equations Models

##### Tables ###################################################################
# Table 1
source("code/04A_MixFol_Table1.R")

# Table 2
source("code/04B_MixFol_Table2.R")

# Table 3

##### Figures ##################################################################
# Figure S1

# Figure S2

# Figure S3

# Figure S4