#hypothesis testing
library(tidyverse)

#parameters
n <- 30               
true_mus <- c(7, 10, 12, 16)       
true_sd <- 3            
mu0 <- 10              
alpha <- 0.05           
n_sim <- 5000  

#run simulation.  For each value of true mue=
#generate n_sim random samples from a normal distribution
#run a one sample t test for each sample
#collect the test statistic, p valuem and and indicate whether H0 is rejected or not
#compute the computed rejection rate
sim_df <- bind_rows(lapply(true_mus, function(mu) {
  sim_results <- replicate(n_sim, {
    x <- rnorm(n, mean = mu, sd = true_sd)
    t_res <- t.test(x, mu = mu0)
    c(
      t_stat = (mean(x) - mu0) / (sd(x) / sqrt(n)),
      reject = as.integer(t_res$p.value < alpha)
    )
  })
  
  data.frame(t(sim_results)) %>%
    mutate(true_mu = mu)
}))

#what is the rejection rate here for the different values of true mu
rejection_rates <- sim_df %>%
  group_by(true_mu) %>%
  summarise(empirical_rejection_rate = mean(reject), .groups = "drop")

print(rejection_rates)

#plot the histogram of t statistics
#the theoretical t_n-1 density overlaid in red
#and the vertical line at the critcal t values
ggplot(sim_df, aes(x = t_stat)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "skyblue", color = "white") +
  stat_function(fun = dt, args = list(df = n-1), color = "red", linewidth = 1) +
  geom_vline(xintercept = qt(c(alpha/2, 1-alpha/2), df = n-1),
             linetype = "dashed", color = "darkgreen") +
  facet_wrap(~ true_mu, scales = "free_y") +
  labs(
    title = "One-Sample t-Test: Sampling Distribution of t under Different True Means",
    subtitle = paste0(n_sim, " simulations per panel, n = ", n,
                      ", mu true = ", mu0, ". Green lines = rejection region."),
    x = "t-statistic",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

#this plot shows the distribution of the t statistics for a few different 
#true mean values.  This shows how hypothesis testing behaves is the true mean moves away from the null.
#each panel is a true mu scenario, or world.  Blue histogram shows the simulated t statistics
#for n_sim samples.  This is the sampling disttribution under that true mu
#Red curve is the theoretical distribution under the null.  
#green lines show the rejection boundaries
#the t statistic's distribution changes as the truth changes.  
#the type 1 error rate stays the same when H0 is true
#power grows as the difference between mu and mu0 increases.  