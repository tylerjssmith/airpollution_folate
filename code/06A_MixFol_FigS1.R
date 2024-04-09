################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Figure S1: Air Pollution Distributions

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(patchwork)

##### Prepare Data #############################################################
df_fig1 = df %>%
  select(SUBJECT_ID, contains("NO2"), contains("O3"), contains("PM25"), 
    contains("SO2")) %>%
  pivot_longer(-SUBJECT_ID, values_to = "CONCENTRATION")

df_fig1 = df_fig1 %>%
  separate(name, into = c("POLLUTANT","VISIT"), sep = "_") %>%
  mutate(
    VISIT = 
      ifelse(VISIT == "V1", "Trimester 1", 
      ifelse(VISIT == "V3", "Trimester 3", NA))
  )

##### Generate Figure ##########################################################
fig1 =
  df_fig1 %>% fn_fig1(x = "NO2")  / 
  df_fig1 %>% fn_fig1(x = "O3")   /
  df_fig1 %>% fn_fig1(x = "PM25") / 
  df_fig1 %>% fn_fig1(x = "SO2")

fig1

##### Export Figure ############################################################
ggsave(
  plot = fig1,
  filename = "figures/MixFol_FigS1_Air.jpg",
  device = "jpeg",
  width = 6.5,
  height = 8,
  units = "in",
  dpi = 400
)

