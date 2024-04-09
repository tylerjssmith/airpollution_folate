################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Figure 3

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Plasma Total Folate by Air Pollution #####################################
# Prepare Data for Figure
df_fig3 <- df %>%
  select(
    SUBJECT_ID, 
    ends_with("TRIM1_L2"), 
    FOL_TM1_TOTAL_LN, 
    FOL_TM3_TOTAL_LN
  )

df_fig3 <- df_fig3 %>%
  pivot_longer(
    ends_with("TRIM1_L2"), 
    names_to = "POL_VAR", 
    values_to = "POL_VAL"
  )

df_fig3 <- df_fig3 %>%
  pivot_longer(
    ends_with("TOTAL_LN"), 
    names_to = "FOL_VAR", 
    values_to = "FOL_VAL"
  )
  
# Define Labels
labs_fig3_fol <- c("Trimester 1","Trimester 3")
names(labs_fig3_fol) <- c("FOL_TM1_TOTAL_LN","FOL_TM3_TOTAL_LN")

labs_fig3_air <- c("NO2 (ppb)","O3 (ppb)","PM2.5 (mcg/m3)","SO2 (ppb)")
names(labs_fig3_air) <- c("NO2_TRIM1_L2","O3_TRIM1_L2","PM25_TRIM1_L2","SO2_TRIM1_L2")

# Generate Figure
(fig3 <- df_fig3 %>%
  filter(POL_VAL >= -5) %>%
  ggplot(aes(x = POL_VAL, y = FOL_VAL)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "blue") +
  geom_smooth(method = "gam", color = "red", linetype = "dashed") +
  facet_grid(POL_VAR ~ FOL_VAR, 
    labeller = labeller(FOL_VAR = labs_fig3_fol, POL_VAR = labs_fig3_air)) +
  labs(
    title = "Plasma Total Folate by\nAverage Daily Air Pollutant Concentration in the First Trimester",
    x = "Air Pollutant (log2[unit])",
    y = "Plasma Total Folate (ln[nmol/L])") +
  theme_bw())

ggsave(
  plot = fig3,
  filename = "figures/MixFol_Fig3_Folate_by_Air.jpg",
  device = "jpeg",
  width = 6.5,
  height = 8,
  units = "in",
  dpi = 400
)


