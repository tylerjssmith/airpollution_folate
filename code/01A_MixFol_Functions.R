################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Functions

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Function: Make Descriptive Table #########################################
make_tbl_chunk <- function(data, x, y, ylab) {

  # Generate Summary Statistics
  x <- df %>%
    group_by({{ x }}) %>%
    filter(!is.na({{ y }})) %>%
    summarise(
      # Group Size
      n = n(),
      # Median
      med = median({{ y }}),
      # Quartile 1
      q1 = quantile({{ y }}, 0.25),
      # Quartile 3
      q3 = quantile({{ y }}, 0.75)
    )
  
  # Format Statistics; Rename and Select Columns
  x %>%
    # Round Statistics
    mutate(across(c(med,q1,q3), ~ format(round(.x, digits = 1), nsmall = 1))) %>%
    # Combine Rounded Statistics for Presentation
    mutate(med_iqr = paste0(med, " (", q1, ", ", q3, ")")) %>%
    # Rename and Select Columns
    mutate(var = colnames(x)[1]) %>%
    rename(val = 1) %>%
    mutate(ylab = ylab) %>%
    select(ylab, var, val, n, med_iqr)

}

make_tbl = function(data, y, ylab) {
  rbind(
    data %>% 
      make_tbl_chunk(x = AGE4,
      y = {{ y }}, ylab = ylab),
    data %>% make_tbl_chunk(x = EDUCATION, 
      y = {{ y }}, ylab = ylab),
    data %>% make_tbl_chunk(x = RACE, 
      y = {{ y }}, ylab = ylab),
    data %>% make_tbl_chunk(x = INCOME, 
      y = {{ y }}, ylab = ylab),
    data %>% make_tbl_chunk(x = HOUSEHOLD_SIZE, 
      y = {{ y }}, ylab = ylab),
    data %>% make_tbl_chunk(x = BIRTH_COUNTRY, 
      y = {{ y }}, ylab = ylab),
    data %>% make_tbl_chunk(x = FOLIC_ACID3, 
      y = {{ y }}, ylab = ylab),
    data %>% make_tbl_chunk(x = HEALTHY_EATING4,
      y = {{ y }}, ylab = ylab)
  )
}

##### Figure S1: Density Plots #################################################
fn_figS1 = function(data, x, labels = TRUE)
{
  if(labels) {
    fig_labels = as_labeller(c(NO2="NO[2] (ppb)", O3="O[3] (ppb)", 
      PM25="PM[2.5] (µg/m^3)", SO2="SO[2] (ppb)"), default = label_parsed)
  }
  
  out = data %>%
    filter(POLLUTANT == x) %>%
    ggplot(aes(x = CONCENTRATION)) +
    geom_density() +
    facet_grid(POLLUTANT ~ VISIT, 
      labeller = labeller(POLLUTANT = fig_labels)) +
    labs(
      x = "Concentration",
      y = "Density") +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(out)
}

##### Figure S2: Density Plots #################################################
fn_figS2 = function(data = df_fig2, x) 
{
  out = data %>%
    filter(SCALE == x) %>%
    ggplot(aes(x = CONCENTRATION)) +
    geom_density() +
    facet_grid(SCALE ~ VISIT) +
    labs(
      x = "Concentration",
      y = "Density") +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(out)
}



