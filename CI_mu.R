#confidence intervals of mu
library(tidyverse)

#data and sample statistics from some distribution assuming 95% CI
true_mu <- 10 
true_sd <- 3
x <- rnorm(50, mean = true_mu, sd = true_sd)
xbar <- mean(x)
s <- sd(x)
n <- length(x)

#set of CIS for each confidence level
conf_levels <- c(0.50, 0.80, 0.90, 0.95, 0.99, 0.999)

ci_df <- data.frame(conf_level = conf_levels) %>%
  rowwise() %>%
  mutate(
    alpha = 1 - conf_level,
    t_crit = qt(1 - alpha/2, df = n - 1),
    margin_error = t_crit * s / sqrt(n),
    lower = xbar - margin_error,
    upper = xbar + margin_error
  ) %>%
  ungroup()

ggplot(ci_df, aes(y = factor(conf_level), x = xbar)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "red") +
  geom_vline(xintercept = true_mu, color = "darkgreen", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Confidence Intervals for mu at Different Confidence Levels",
    subtitle = "Green dashed line = true mu",
    x = "μ (with Confidence Interval)", 
    y = "Confidence Level"
  ) +
  theme_minimal(base_size = 14)

#do this many times
#n equals sample size of the dataset
#n_samples is the number of datasets

n <- 50                 
n_samples <- 800

#create table with one row per simulated dataset
#for each row, draw n random values from the normal distribution
#for each observation compute the sample mean and standard deviation
#keep only trhe simulation index and the summary statistics
sim_df <- tibble(sim = 1:n_samples) %>%
  rowwise() %>%
  mutate(x = list(rnorm(n, mean = true_mu, sd = true_sd))) %>%
  ungroup() %>%
  mutate(xbar = sapply(x, mean),
         s    = sapply(x, sd)) %>%
  select(sim, xbar, s)

#build confidence intervals for each simulation
#for each simulation, compute alpha, t_critical, margin of error
#lower and upper CI bounds and an indicator variable to see if the interval
#covers the true value
cis <- expand_grid(sim_df, conf_level = conf_levels) %>%
  mutate(
    alpha   = 1 - conf_level,
    df      = n - 1,
    t_crit  = qt(1 - alpha/2, df = df),
    me      = t_crit * s / sqrt(n),
    lower   = xbar - me,
    upper   = xbar + me,
    cover   = (lower <= true_mu) & (true_mu <= upper)
  )

#group by confidence level and compute coverage probability
coverage_summary <- cis %>%
  group_by(conf_level) %>%
  summarise(coverage = mean(cover), .groups = "drop")

print(coverage_summary)

#plot confidence intervals.  Each row is one simulation.  CIs are blue if they cover mu
#and red if they do not.  the dot is the sample mean, with the dashed line being
#the true mean
ggplot(cis, aes(y = sim)) +
  geom_segment(aes(x = lower, xend = upper, yend = sim, color = cover), linewidth = 0.8) +
  geom_point(aes(x = xbar), size = 1.3) +
  geom_vline(xintercept = true_mu, linetype = "dashed", color = "darkgreen", linewidth = 1) +
  scale_color_manual(values = c(`TRUE` = "steelblue", `FALSE` = "tomato"),
                     labels = c(`TRUE` = "Covers μ", `FALSE` = "Misses μ"),
                     name = NULL) +
  facet_wrap(~ conf_level, ncol = 3,
             labeller = labeller(conf_level = function(z) paste0(100*as.numeric(z), "% CI"))) +
  labs(
    title = "Many Confidence Intervals for mu Across Simulated Samples",
    subtitle = "Blue intervals cover the true mu; red intervals miss it. Dashed green line = true mu.",
    x = "mu",
    y = "Sample index"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")
