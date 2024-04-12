################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Figure S4: Plasma Folate by Air Pollution

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(patchwork)

##### Prepare Data #############################################################
df_figS4 = imp_post %>%
  # use first MICE imputation
  complete(action = 1) %>%
  
  # select relevant variables
  select(SUBJECT_ID, VISIT, starts_with("FOL_") & ends_with("_LN"), 
    NO2, O3, PM25, SO2) %>%
  
  # pivot longer
  pivot_longer(
    starts_with("FOL_"), 
    names_to = "FOL", 
    values_to = "FOLCON"
  ) %>%
  pivot_longer(
    c(NO2,O3,PM25,SO2), 
    names_to = "AIR", 
    values_to = "AIRCON"
  )

df_figS4 = df_figS4 %>%
  mutate(FOL = factor(FOL,
    levels = c("FOL_TOTAL_LN","FOL_5methylTHF_LN","FOL_NONMETHYL_LN","FOL_UMFA_LN"),
    labels = c("log(Total)","log(5methylTHF)","log(Nonmethyl)","log(UMFA)"))
  )

df_figS4 = df_figS4 %>%
  mutate(VISIT = factor(VISIT,
    levels = c("TM1","TM3"),
    labels = c("Trimester 1","Trimester 3"))
  )

df_figS4 %>% head()

##### Figure S4 ################################################################
figS4 = function(data, pollutant, xlab) {
  data %>%
    filter(AIR == pollutant) %>%
    ggplot(aes(x = AIRCON, y = FOLCON)) +
    geom_point(alpha = 0.2, size = 0.7) +
    geom_smooth(size = 0.5, color = "orange") + 
    facet_grid(FOL ~ VISIT) +
    labs(
      x = xlab,
      y = "Plasma Folate Concentration") +
    theme_bw() +
    theme(strip.text = element_text(size = 8))
}

figS4A = df_figS4 %>% figS4("NO2", 
  xlab = expression("NO"[2]))
figS4B = df_figS4 %>% figS4("O3", 
  xlab = expression("O"[3]))
figS4C = df_figS4 %>% figS4("PM25", 
  xlab = expression("PM"[2.5]))
figS4D = df_figS4 %>% figS4("SO2", 
  xlab = expression("SO"[2]))

(figS4 = (figS4A + figS4B) / (figS4C + figS4D))

##### Export Figures ###########################################################
# Figure S4
ggsave(
  plot = figS4,
  filename = "figures/figS4.jpg",
  device = "jpeg",
  width = 9,
  height = 12,
  units = "in",
  dpi = 400
)


