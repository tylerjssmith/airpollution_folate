################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Figure 1: Air Pollution Distributions

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

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

df_fig1 %>% head()

##### Prepare Labels ###########################################################
fig1_labels = as_labeller(c(NO2="NO[2] (ppb)", O3="O[3] (ppb)", 
  PM25="PM[2.5] (µg/m^3)", SO2="SO[2] (ppb)"), default = label_parsed)

##### Generate Figure ##########################################################
fig1 = df_fig1 %>%
  ggplot(aes(x = CONCENTRATION)) +
  geom_density() +
  facet_wrap(POLLUTANT ~ VISIT, ncol = 2, scales = "free", 
    labeller = labeller(POLLUTANT = fig1_labels)) +
  labs(
    x = "Concentration",
    y = "Density") +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

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

