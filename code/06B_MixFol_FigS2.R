################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Figure S2: Plasma Folate Distributions

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Prepare Data #############################################################
df_fig2 <- df %>%
  select(SUBJECT_ID, contains("_TOTAL")) %>%
  pivot_longer(-SUBJECT_ID)

df_fig2 %>% head()

##### Prepare Labels ###########################################################
labs_fig2 <- c(
  "Trimester 1: Total Folate",
  "Trimester 1: ln(Total Folate)",
  "Trimester 3: Total Folate",
  "Trimester 3: ln(Total Folate)"
)

names(labs_fig2) <- c(
  "FOL_TM1_TOTAL",
  "FOL_TM1_TOTAL_LN",
  "FOL_TM3_TOTAL",
  "FOL_TM3_TOTAL_LN"
)

##### Generate Figure ##########################################################
fig2 <- df_fig2 %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(. ~ name, scales = "free", labeller = labeller(name = labs_fig2)) +
  labs(
    x = "Plasma Folate (nmol/L)",
    y = "Density") +
  theme_bw()

fig2

##### Export Figure ############################################################
ggsave(
  plot = fig2,
  filename = "figures/MixFol_FigS2_Folate.jpg",
  device = "jpeg",
  width = 6.5,
  height = 8,
  units = "in",
  dpi = 400
)

