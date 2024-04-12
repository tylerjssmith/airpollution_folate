################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Explore Data

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(ggcorrplot)

##### Read Data ################################################################
df = readRDS("data/MixFol_Complete.rds")
df %>% head()

##### Sample Size by Visit #####################################################
# By Visit
df %>% count(!is.na(FOL_TM1_TOTAL))
df %>% count(!is.na(FOL_TM3_TOTAL))

# Both Visits
df %>% count(!is.na(FOL_TM1_TOTAL) & !is.na(FOL_TM3_TOTAL))

##### Plasma Folate Correlations ###############################################
df %>%
  select(starts_with("FOL_") & ends_with("_LN")) %>%
  na.omit() %>%
  cor(method = "spearman") %>%
  ggcorrplot(lab = TRUE, show.legend = FALSE)
