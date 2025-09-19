#showing the long run average of rolling a die
library(tidyverse)


#function for a bunch of die rolls
simulate.die <- function(n_tosses, sim.id){
  #simulate rolling a fair 6 sided die
  rolls <- sample(1:6, size = n_tosses, replace = TRUE)
  
  #compute cumulative running mean
  running_mean <- cumsum(rolls) / seq_along(rolls)
  
  #save output for plotting, recording the toss number, the cum mean and sim setting
  output <- data.frame(
    toss = seq_len(n_tosses),
    running_mean = running_mean,
    sim = paste0(n_tosses, " Tosses"),
    stringsAsFactors = FALSE
  )
}

#series of tosses
n_values <- c(4,7, 21, 100, 500, 25000)
simulations <- do.call(rbind,
                       lapply(seq_along(n_values), function(i) simulate.die(n_values[i], i))
)

#plotting this
ggplot(simulations, aes(x = toss, y = running_mean, color = sim)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 3.5, linetype = "dashed", color = "black") +
  annotate("text", x = max(simulations$toss)*0.4, y = 3.48,
           label = "Theoretical Mean (3.5)", hjust = 0.5, vjust = -2.0) +
  scale_x_log10() +
  labs(
    title = "Running Mean of Die Rolls for Different Numbers of Tosses",
    x = "Number of Tosses",
    y = "Running Mean",
    color = "Simulation"
  ) +
  theme_minimal(base_size = 14)
