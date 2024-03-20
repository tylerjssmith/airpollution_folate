################################################################################
# Air Pollution and Plasma Folate among Pregnant Women in Canada, 2008-2011
# Generalized Estimating Equations

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(geepack)

# Specify Outcomes and Exposures
y = c("FOL_TOTAL_LN","FOL_5methylTHF_LN","FOL_NONMETHYL_LN","FOL_UMFA_LN")
x = c("NO2","O3","PM25","SO2")

##### Overall ##################################################################
# Function: Fit Overall Models
fit_gee = function(data, y, x, id = "SUBJECT_ID2", corstr = "exchangeable",
  model_label = "Overall")
{
  # Fit Model
  fit = with(data, geeglm(get(y) ~ get(x) + 
      AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
      HEALTHY_EATING + SITE_ID + FOLIC_ACID3 + VISIT, 
    id = get(id), corstr = corstr))
  
  # Pool, Summarize, and Label Estimates
  fit %>%
    pool() %>%
    summary(conf.int = TRUE) %>%
    mutate(
      model = model_label,
      y = y,
      x = x
    )
}

# Fit Overall Models
est_overall = tibble()

for(i in 1:length(y)) {
  for(j in 1:length(x)) {
    
    out_ij = imp_gee %>% 
      fit_gee(
        y = y[i], 
        x = x[j]
      )
    
    est_overall = rbind(est_overall, out_ij)
    print(paste0("Done: model=", out_ij[1,"model"], ", x=", x[i], ", y=", y[j]))
    
  }
}

##### By Visit #################################################################
# Function: Fit Models by Visit
fit_gee_visit = function(data, y, x, id = "SUBJECT_ID2", corstr = "exchangeable",
  model_label = "By Visit")
{
  # Fit Model
  fit = with(data, geeglm(get(y) ~ get(x) * VISIT + 
      AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
      HEALTHY_EATING + SITE_ID + FOLIC_ACID3, 
    id = get(id), corstr = corstr))
  
  # Pool, Summarize, and Label Estimates
  fit %>%
    pool() %>%
    summary(conf.int = TRUE) %>%
    mutate(
      model = model_label,
      y = y,
      x = x
    )
}

# Fit Models by Visit
est_visit = tibble()

for(i in 1:length(y)) {
  for(j in 1:length(x)) {
    
    out_ij = imp_gee %>% 
      fit_gee_visit(
        y = y[i], 
        x = x[j]
      )
    
    est_visit = rbind(est_visit, out_ij)
    print(paste0("Done: model=", out_ij[1,"model"], ", x=", x[i], ", y=", y[j]))
    
  }
}

##### By Folic Acid Supplementation ############################################
# Function: Fit Models by Folic Acid Supplementation
fit_gee_fa = function(data, y, x, id = "SUBJECT_ID2", corstr = "exchangeable",
  model_label = "By Folic Acid")
{
  # Fit Model
  fit = with(data, geeglm(get(y) ~ get(x) * FOLIC_ACID3 + 
      AGE + EDUCATION + RACE + INCOME + HOUSEHOLD_SIZE + BIRTH_COUNTRY + 
      HEALTHY_EATING + SITE_ID + VISIT, 
    id = get(id), corstr = corstr))
  
  # Pool, Summarize, and Label Estimates
  fit %>%
    pool() %>%
    summary(conf.int = TRUE) %>%
    mutate(
      model = model_label,
      y = y,
      x = x
    )
}

# Fit Models by Visit
est_fa = tibble()

for(i in 1:length(y)) {
  for(j in 1:length(x)) {
    
    out_ij = imp_gee %>% 
      fit_gee_fa(
        y = y[i], 
        x = x[j]
      )
    
    est_fa = rbind(est_fa, out_ij)
    print(paste0("Done: model=", out_ij[1,"model"], ", x=", x[i], ", y=", y[j]))
    
  }
}

##### Gather Results ###########################################################
# Function: Gather Results
tidy_gee = function(data, sort = y, digits = 3) {
  data %>%
    filter(grepl("get\\(x\\)", term)) %>%
    mutate(y = gsub("FOL_", "", y)) %>%
    select(model, y, x, term, estimate, conf.low = `2.5 %`, 
      conf.high = `97.5 %`, p.value) %>%
    arrange({{ sort }})
}

# Overall
est_overall_tidy = est_overall %>% 
  tidy_gee() %>%
  mutate(term = x)

# By Visit
est_visit_tidy = est_visit %>% 
  tidy_gee() %>%
  mutate(term2 = gsub("get\\(x\\)", "", term)) %>%
  mutate(term2 = gsub("VISIT", "", term2)) %>%
  mutate(term = paste0(x,term2))

# By Folic Acid Supplementation
est_fa_tidy = est_fa %>%
  tidy_gee() %>%
  mutate(term2 = gsub("get\\(x\\)", "", term)) %>%
  mutate(term2 = gsub("VISIT", "", term2)) %>%
  mutate(term = paste0(x,term2)) %>%
  select(-term2)

##### Plot Results #############################################################
# Overall
est_overall_tidy %>%
  ggplot(aes(x = y, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0.1) +
  geom_point() +
  facet_wrap(. ~ term) +
  labs(
    x = "Term",
    y = "Expected Difference in Plasma Folate
    per 2-Fold Difference in Air Pollutant
    (95% Confidence Interval)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
    margin = margin(t = 5)))

# By Visit
est_visit_tidy %>%
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0.1) +
  geom_point() +
  facet_grid(y ~ x, scales = "free_x") +
  labs(
    x = "Term",
    y = "Coefficient per Log2(Pollutant)
    (95% Confidence Interval)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
    margin = margin(t = 5)))

# By Folic Acid Supplementation
est_fa_tidy %>%
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0.1) +
  geom_point() +
  facet_grid(y ~ x, scales = "free_x") +
  labs(
    x = "Term",
    y = "Coefficient per Log2(Pollutant)
    (95% Confidence Interval)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
    margin = margin(t = 5)))



