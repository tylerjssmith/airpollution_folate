################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Figure 2: Plasma Total Folate

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Figure 2: Plasma Total Folate ############################################
df_fig2 <- df %>%
  select(SUBJECT_ID, contains("_TOTAL")) %>%
  pivot_longer(-SUBJECT_ID)

df_fig2 %>% head()

labs_fig2 <- c(
  "Trimester 1: Folate, Linear",
  "Trimester 1: Folate, Natural Log",
  "Trimester 3: Folate, Linear",
  "Trimester 3: Folate, Natural Log"
)

names(labs_fig2) <- c(
  "FOL_TM1_TOTAL",
  "FOL_TM1_TOTAL_LN",
  "FOL_TM3_TOTAL",
  "FOL_TM3_TOTAL_LN"
)

(fig2 <- df_fig2 %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(. ~ name, scales = "free", labeller = labeller(name = labs_fig2)) +
  labs(
    title = "Plasma Total Folate by Trimester",
    x = "Plasma Total Folate (nmol/L)",
    y = "Density") +
  theme_bw())

ggsave(
  plot = fig2,
  filename = "MixFol_Fig2_Folate.jpg",
  device = "jpeg",
  width = 6.5,
  height = 8,
  units = "in",
  dpi = 400
)

