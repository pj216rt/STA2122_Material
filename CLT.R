#central limit theorem code
library(tidyverse)

#parameters
n_sim <- 10000
lambda <- 1
sample_sizes <- c(1, 5, 10, 20, 30, 100, 400, 1000)

#lambda is the rate parameter for the exponential distribution (mean = 1/lambda)
#sample sizes is the vector of different samples that we're comparing

data.original <- data.frame(x=rexp(50000, rate = lambda))
#the original density

clt_data <- lapply(sample_sizes, function(n) {
  sample_means <- replicate(n_sim, mean(rexp(n, rate = lambda)))
  data.frame(sample_mean = sample_means, n = factor(n))
}) %>% bind_rows()

#rexp(n, rate) is drawing n samples from an exponential distribution
#we compute the mean of these
#doing this n_sims times
#do this for each sample size
#combine the results into a dataframe

#theoretical mean and variance
mean_theory <- 1 / lambda
var_theory <- 1 / (lambda^2)

#compute the mean and variance from central limit theorem

normal_df <- bind_rows(lapply(sample_sizes, function(n) {
  sd_n <- sqrt(var_theory / n)
  x_seq <- seq(mean_theory - 5*sd_n, mean_theory + 5*sd_n, length.out = 400)
  data.frame(
    x = x_seq,
    density = dnorm(x_seq, mean = mean_theory, sd = sd_n),
    n = factor(n)
  )
}))

#for each sample size, compute the theoretical standard deviation of the sample mean
#generate x values around the mean (length 400)
#compute the normal density at each x using dnorm

#plot of the original data
p1 <- ggplot(data.original, aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "orange", color = "white") +
  stat_function(fun = dexp, args = list(rate = lambda),
                color = "red", linewidth = 1) +
  labs(title = "Original Exponential(λ = 1) Distribution",
       x = "x", y = "Density") +
  theme_minimal(base_size = 14)


#Plot
p2 <- ggplot(clt_data, aes(x = sample_mean)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "skyblue", color = "white") +
  geom_line(data = normal_df, aes(x = x, y = density), 
            color = "red", linewidth = 1) +
  facet_wrap(~ n, scales = "free", ncol = 2) +
  labs(
    title = "Central Limit Theorem with Exponential(mean = 1)",
    subtitle = "Histograms of sample means (blue) with normal approximation (red)",
    x = "Sample Mean", y = "Density"
  ) +
  theme_minimal(base_size = 14)


#plotting simulated sample means for each sample size
#adding a red line showing the normal distribution predicted by the CLT
p1
p2

#exponential distribution used a lot to model failure times, like for a machine component
#think like piece on a circuit board on your phone or a lightblub
