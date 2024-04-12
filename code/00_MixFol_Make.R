################################################################################
# Air Pollution and Plasma Folate
# Make File

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Set-up ###################################################################
# Functions
source("code/01A_MixFol_Functions.R")

##### Exploratory Data Analysis ################################################
# Explore
source("code/02A_MixFol_Explore.R")

##### Regression Models ########################################################
# Run Multiple Imputation
source("code/03A_MixFol_MICE.R")

# Fit Linear Regression Models
source("code/03B_MixFol_Linear.R")

# Fit Beta Regression Models
source("code/03C_MixFol_Beta.R")

##### Tables ###################################################################
# Table 1
source("code/04A_MixFol_Table1.R")

# Table 2
source("code/04B_MixFol_Table2.R")

# Table 3
source("code/04C_MixFol_Table3.R")

# Table 4
source("code/04D_MixFol_Table4.R")

##### Figures ##################################################################
# Figure 1
source("code/05A_MixFol_Fig1.R")

# Figure 2
source("code/05B_MixFol_Fig2.R")

##### Supplemental Figures #####################################################
# Figure S1
source("code/06A_MixFol_FigS1.R")

# Figures S2-S3
source("code/06B_MixFol_FigS2_FigS3.R")

# Figure S4
source("code/06C_MixFol_FigS4.R")
