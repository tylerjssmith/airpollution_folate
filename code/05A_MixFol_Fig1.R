################################################################################
# Air Pollution and Plasma Folate among Pregnant Women in Canada
# Figure 1 - Linear Regression Estimates

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(broom)

# Check Estimates
df_lm %>% head()

##### Plot Estimates ###########################################################
# Label Variables
df_lm = df_lm %>%
  mutate(visitf = factor(visit,
    levels = c("TM1","TM3"),
    labels = c("Trimester 1","Trimester 3")
  ))

df_lm = df_lm %>%
  mutate(yf = factor(y, 
    levels = c("FOL_TOTAL_LN","FOL_5methylTHF_LN","FOL_NONMETHYL_LN","FOL_UMFA_LN"), 
    labels = c("log(Total)","log(5methylTHF)","log(Nonmethyl)","log(UMFA)")
  ))

df_lm = df_lm %>%
  mutate(xf = factor(x,
    levels = c("NO2","O3","PM25","SO2"),
    labels = c("log2(NO2)","log2(O3)","log2(PM2.5)","log2(SO2)")
  ))

# Generate Plot
fig1 = df_lm %>%
  ggplot(aes(x = visitf, y = estimate, ymin = conf.low, ymax = conf.high, 
    color = adj)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0, position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2)) +
  scale_y_continuous(limits = c(-0.4,0.8), breaks = seq(-0.4,0.8,0.2)) +
  labs(
    x = "Visit",
    y = "Expected Difference in Log Plasma Folate
    per Log2 Difference in Air Pollutant
    (95% Confidence Interval)",
    color = "Model") +
  facet_grid(xf ~ yf) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

fig1

##### Export Figures ###########################################################
# Figure 2
ggsave(
  plot = fig2,
  filename = "figures/MixFol_Fig2_Beta.jpg",
  device = "jpeg",
  width = 8,
  height = 8,
  units = "in",
  dpi = 400
)

