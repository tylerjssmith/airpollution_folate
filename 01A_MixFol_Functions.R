################################################################################
# Environmental Mixtures and Plasma Folate in Pregnancy
# Functions

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Function: Make Descriptive Table #########################################
make_table <- function(data, x, y = FOL_TM1_TOTAL) {
 
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
    select(var, val, n, med_iqr)

}
