################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Join Data

##### Sample Size by Visit #####################################################
df %>% count(!is.na(FOL_TM1_TOTAL))
df %>% count(!is.na(FOL_TM3_TOTAL))
