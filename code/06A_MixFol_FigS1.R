################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Figure S1: Air Pollution Distributions

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(patchwork)

##### Prepare Data #############################################################
df_fig1 = imp_post %>%
  complete(action = 1) %>%
  select(SUBJECT_ID, VISIT, contains("NO2"), contains("O3"), contains("PM25"), 
    contains("SO2")) %>%
  mutate(across(-c(SUBJECT_ID, VISIT), ~ 2 ^ .x)) %>%
  pivot_longer(
    cols = -c(SUBJECT_ID, VISIT), 
    names_to = "AIR", 
    values_to = "CONCENTRATION"
  )

df_fig1 = df_fig1 %>%
  mutate(VISIT = factor(VISIT,
    levels = c("TM1","TM3"),
    labels = c("Trimester 1","Trimester 3"))
  )

df_fig1 %>% head()

##### Generate Figure ##########################################################
# Function: Figure S1
fig1 = function(data, pollutant, xlab = NULL) {
  if(is.null(xlab)) xlab = pollutant
  data %>%
    filter(AIR == pollutant) %>%
    ggplot(aes(x = CONCENTRATION)) +
    geom_density() +
    facet_grid(AIR ~ VISIT) +
    labs(
      x = xlab,
      y = "Density") +
    theme_bw() +
    theme(panel.grid = element_blank())
}

# Make Panels
fig1A = df_fig1 %>% 
  fig1(pollutant = "NO2", 
    xlab = expression("NO"[2] * " (ppb)")) 

fig1B = df_fig1 %>% 
  fig1(pollutant = "O3",  
    xlab = expression("O"[3] * " (ppb)"))

fig1C = df_fig1 %>% 
  fig1(pollutant = "PM25", 
    xlab = expression("PM"[2.5] * " (" * mu * "g/m" ^ 3 * ")")) 

fig1D = df_fig1 %>% 
  fig1(pollutant = "SO2", 
    xlab = expression("SO"[2] * " (ppb)"))

# Combine Panels
(fig1 = fig1A / fig1B / fig1C / fig1D)

##### Export Figure ############################################################
ggsave(
  plot = fig1,
  filename = "figures/MixFol_FigS1_Air.jpg",
  device = "jpeg",
  width = 8,
  height = 8,
  units = "in",
  dpi = 400
)

