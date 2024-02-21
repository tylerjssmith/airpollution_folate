################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Join Data

##### Sample Size by Visit #####################################################
df %>% count(!is.na(FOL_TM1_TOTAL))
df %>% count(!is.na(FOL_TM3_TOTAL))

summary(df$NO2_TRIM1)
summary(df$O3_TRIM1)
summary(df$PM25_TRIM1)
summary(df$SO2_TRIM1)
