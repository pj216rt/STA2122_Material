#central limit theorem code
library(tidyverse)

#probability of heads
p  <- 0.5

#number of simulations for each sample size
B  <- 20000        
ns <- c(10, 30, 100)

hist_df <- data.frame()
for (n in ns) {

  k  <- rbinom(B, size = n, prob = p)
  xb <- k / n
  
  #put on grid, seelect bin width and scale for density
  tb <- as.data.frame(table(xb), stringsAsFactors = FALSE)
  tb$xb <- as.numeric(tb$xb)
  bw <- 1 / n                     
  tb$density <- tb$Freq / (sum(tb$Freq) * bw)  
  
  tb$n <- factor(n, levels = ns)
  tb$width <- bw
  hist_df <- rbind(hist_df, tb[, c("n", "xb", "density", "width")])
}


#creating normal distribution
curve_df <- data.frame()
for (n in ns) {
  x <- seq(0, 1, by = 0.001)
  den <- dnorm(x, mean = p, sd = sqrt(p * (1 - p) / n))
  tmp <- data.frame(n = factor(n, levels = ns), x = x, den = den)
  curve_df <- rbind(curve_df, tmp)
}

#plot of histograms overlaid with a smooth normal distribution
g2 <- ggplot() +
  geom_col(
    data = hist_df,
    aes(x = xb, y = density, width = width),
    fill = "grey70", color = "white", alpha = 0.8, linewidth = 0.2
  ) +
  geom_line(
    data = curve_df,
    aes(x = x, y = den),
    color = "red", linewidth = 1.2
  ) +
  facet_wrap(~ n, nrow = 1, labeller = label_bquote(n == .(as.character(n)))) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(
    title = "CLT: Sample Means X̄_n",
    subtitle = bquote(bar(X)[n] ~ "vs." ~ N(.(p), .(p)*(1-.(p))/n) ~ "for coin flips"),
    x = expression(bar(X)[n]), y = "density"
  ) +
  theme_minimal(base_size = 12)

print(g2)
