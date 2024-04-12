################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Figures S2-S3: Plasma Folate Distributions

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(patchwork)

##### Prepare Data #############################################################
# Figure S2: Folate Vitamer Concentrations
df_figS2 = imp_post %>%
  complete(action = 1) %>%
  select(SUBJECT_ID, VISIT, starts_with("FOL_") & ends_with("_LN")) %>%
  mutate(across(-c(SUBJECT_ID, VISIT), ~ exp(.x))) %>%
  rename_with(~ gsub("_LN", "", .x)) %>%
  pivot_longer(-c(SUBJECT_ID, VISIT))

df_figS2 = df_figS2 %>%
  mutate(name = factor(name,
    levels = c("FOL_TOTAL","FOL_5methylTHF","FOL_NONMETHYL","FOL_UMFA"),
    labels = c("Total Folate","5methylTHF","Nonmethyl Folate","UMFA"))
  )

df_figS2 = df_figS2 %>%
  mutate(VISIT = factor(VISIT,
    levels = c("TM1","TM3"),
    labels = c("Trimester 1","Trimester 3"))
  )

df_figS2 %>% head()

# Figure S3: Folate Vitamer Proportions
df_figS3 = imp_post %>%
  complete(action = 1) %>%
  select(SUBJECT_ID, VISIT, starts_with("FOL_") & ends_with("_PR")) %>%
  pivot_longer(-c(SUBJECT_ID, VISIT))

df_figS3 = df_figS3 %>%
  mutate(name = factor(name,
    levels = c("FOL_5methylTHF_PR","FOL_NONMETHYL_PR","FOL_UMFA_PR"),
    labels = c("5methylTHF","Nonmethyl Folate","UMFA"))
  )

df_figS3 = df_figS3 %>%
  mutate(VISIT = factor(VISIT,
    levels = c("TM1","TM3"),
    labels = c("Trimester 1","Trimester 3"))
  )

df_figS3 %>% head()

##### Figure S2 ################################################################
# Function: Figure S2
figS2 = function(data) {
  
  data %>%
    ggplot(aes(x = value)) +
    geom_density() +
    facet_grid(name ~ VISIT, scales = "free") +
    labs(
      x = "Concentration (nmol/L)",
      y = "Density") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
}

# Make Panels
figS2A = df_figS2 %>% 
  filter(name == "Total Folate") %>% 
  figS2()
figS2B = df_figS2 %>% 
  filter(name == "5methylTHF") %>% 
  figS2()
figS2C = df_figS2 %>% 
  filter(name == "Nonmethyl Folate") %>% 
  figS2()
figS2D = df_figS2 %>% 
  filter(name == "UMFA") %>% 
  figS2()

# Combine Panels
(figS2 = figS2A / figS2B / figS2C / figS2D)

##### Generate Figure ##########################################################
# Function: Figure S3
figS3 = function(data) {

  data %>%
    ggplot(aes(x = value)) +
    geom_density() +
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
    facet_grid(name ~ VISIT, scales = "free") +
    labs(
      x = "Proportion of Plasma Total Folate",
      y = "Density") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
}

# Make Panels
figS3A = df_figS3 %>% 
  filter(name == "5methylTHF") %>% 
  figS3()
figS3B = df_figS3 %>% 
  filter(name == "Nonmethyl Folate") %>% 
  figS3()
figS3C = df_figS3 %>% 
  filter(name == "UMFA") %>% 
  figS3()

# Combine Panels
(figS3 = figS3A / figS3B / figS3C)

##### Export Figure ############################################################
# Figure S2
ggsave(
  plot = figS2,
  filename = "figures/MixFol_FigS2_Folate_Conc.jpg",
  device = "jpeg",
  width = 6,
  height = 8,
  units = "in",
  dpi = 400
)

# Figure S3
ggsave(
  plot = figS3,
  filename = "figures/MixFol_FigS3_Folate_Prop.jpg",
  device = "jpeg",
  width = 6,
  height = 8,
  units = "in",
  dpi = 400
)


