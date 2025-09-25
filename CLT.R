#central limit theorem code
library(tidyverse)

#probability of heads
p  <- 0.5

#number of simulations for each sample size
B  <- 20000        
ns <- c(10, 30, 100)

#simulate X bar.  
sim_Xbar <- function(n, p, B) {
  xbar <- replicate(B, mean(rbinom(n, size = 1, prob = p)))
  tibble(n = n, xbar = xbar)
}

#data simulation
df_Z    <- map_dfr(ns, sim_Z, p = p, B = B) %>% mutate(n = factor(n))
df_Xbar <- map_dfr(ns, sim_Xbar, p = p, B = B) %>% mutate(n = factor(n))


df_Z %>%
  group_by(n) %>%
  summarise(mean_Z = mean(z), sd_Z = sd(z))


# ----------------------------
# Figure 2: Unstandardized means Xbar_n vs N(p, p(1-p)/n)
# ----------------------------
# Overlay the corresponding normal density with mean = p and sd = sqrt(p(1-p)/n)
normal_density_xbar <- function(x, n) {
  dnorm(x, mean = p, sd = sqrt(p * (1 - p) / as.numeric(as.character(n))))
}

g2 <- ggplot(df_Xbar, aes(x = xbar)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "skyblue", alpha = 0.6) +
  stat_function(
    fun = function(xx) normal_density_xbar(xx, n = 10),
    data = data.frame(n = factor("10", levels = levels(df_Xbar$n))),
    linewidth = 1.1, color = "red"
  ) +
  stat_function(
    fun = function(xx) normal_density_xbar(xx, n = 30),
    data = data.frame(n = factor("30", levels = levels(df_Xbar$n))),
    linewidth = 1.1, color = "red"
  ) +
  stat_function(
    fun = function(xx) normal_density_xbar(xx, n = 100),
    data = data.frame(n = factor("100", levels = levels(df_Xbar$n))),
    linewidth = 1.1, color = "red"
  ) +
  facet_wrap(~ n, nrow = 1, labeller = label_bquote(n == .(as.character(n)))) +
  labs(
    title = "CLT: Sample Means X̄_n",
    subtitle = bquote(bar(X)[n] ~ "vs." ~ N(.(p), .(p)*(1-.(p))/n) ~ "for coin flips"),
    x = expression(bar(X)[n]), y = "density"
  ) +
  theme_minimal(base_size = 12) +
  coord_cartesian(xlim = c(0, 1))   # <-- restrict x-axis to [0,1]

print(g2)
