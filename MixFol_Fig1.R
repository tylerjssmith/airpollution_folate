################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Figure 1: Air Pollution

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Figure 1: Air Pollution ##################################################
df_fig1 <- df %>%
  select(SUBJECT_ID, contains("_TRIM1")) %>%
  pivot_longer(-SUBJECT_ID)

df_fig1 %>% head()

labs_fig1 <- c(
  "NO2 (ppb), Linear",
  "NO2 (ppb), Binary Log",
  "O3 (ppb), Linear",
  "O3 (ppb), Binary Log",
  "PM2.5 (mcg/m3), Linear",
  "PM2.5 (mcg/m3), Binary Log",
  "SO2 (ppb), Linear",
  "SO2 (ppb), Binary Log"
)

names(labs_fig1) <- c(
  "NO2_TRIM1",
  "NO2_TRIM1_L2",
  "O3_TRIM1",
  "O3_TRIM1_L2",
  "PM25_TRIM1",
  "PM25_TRIM1_L2",
  "SO2_TRIM1",
  "SO2_TRIM1_L2"
)

(fig1 <- df_fig1 %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(. ~ name, ncol = 2, scales = "free", labeller = labeller(name = labs_fig1)) +
  labs(
    title = "Average Daily Air Pollutant Concentrations in the First Trimester",
    x = "Average Daily Air Pollution in the First Trimester",
    y = "Density") +
  theme_bw())

ggsave(
  plot = fig1,
  filename = "MixFol_Fig1_Air.jpg",
  device = "jpeg",
  width = 6.5,
  height = 8,
  units = "in",
  dpi = 400
)

