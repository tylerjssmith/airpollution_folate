################################################################################
# Air Pollution and Plasma Folate
# Make File

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

# Set Working Directory
setwd("E:/BBK22/1. BBK22_folates_Oct5 2023/Analysis")

##### Set-up ###################################################################
# Functions
source("MixFol_Functions.R")

##### Read Data ################################################################
df <- readRDS("MixFol_Complete.rds")

df %>% head()

##### Analysis #################################################################
# Explore
source("MixFol_Explore.R")

# Linear Models
source("MixFol_LinearModels.R")

# Penalized Regression
source("MixFol_PenalizedModels.R")

# Treed Distributed Non-linear Models

# Table 1
source("MixFol_Table1.R")

# Table 2
source("MixFol_Table2.R")

# Table 3

# Figure S1

# Figure S2

# Figure S3

# Figure S4