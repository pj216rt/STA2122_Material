#want R code showing that the normal distribution can take on many different
#appearances

#need to pick parameters
params <- data.frame(
  mu    = c(0),
  sigma = c(0.1, 1, 5, 10),
  label = c("N(0, 0.1)", "N(0, 1)", "N(0, 5)", "N(0, 10)")
)

#areas to find area between
a <- 0.5
b <- 1.5

#dataframes for curves, shaded areas, areas
curve_df <- data.frame()
shade_df <- data.frame()
areas <- data.frame()

for (i in seq_len(nrow(params))) {
  mu    <- params$mu[i]
  sig   <- params$sigma[i]
  lab   <- params$label[i]
  
  # x-range covering most mass for this distribution
  x_min <- mu - 4*sig
  x_max <- mu + 4*sig
  x     <- seq(x_min, x_max, by = 0.001)
  
  den <- dnorm(x, mean = mu, sd = sig)
  
  # full curve for line
  curve_df <- rbind(curve_df,
                    data.frame(label = lab, x = x, den = den))
  
  # shaded region restricted to [a, b]
  x_shade  <- x[x >= a & x <= b]
  if (length(x_shade) > 1) {
    shade_df <- rbind(shade_df,
                      data.frame(label = lab,
                                 x = x_shade,
                                 den = dnorm(x_shade, mean = mu, sd = sig)))
  }
  
  # analytic area under curve on [a, b]
  area <- pnorm(b, mean = mu, sd = sig) - pnorm(a, mean = mu, sd = sig)
  areas <- rbind(areas, data.frame(label = lab, mu = mu, sigma = sig, area = area))
}

areas$lab_with_area <- sprintf("%s\nArea[a,b]=%.3f", areas$label, areas$area)

curve_df$facet <- factor(curve_df$label,
                         levels = areas$label,
                         labels = areas$lab_with_area)
shade_df$facet <- factor(shade_df$label,
                         levels = areas$label,
                         labels = areas$lab_with_area)


p <- ggplot() +
  #shaded area under each curve on [a, b]
  geom_area(data = shade_df, aes(x = x, y = den), alpha = 0.35) +
  #density curve
  geom_line(data = curve_df, aes(x = x, y = den), linewidth = 1.1) +
  #vertical markers for a and b (draw across all facets)
  geom_vline(xintercept = c(a, b), linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~ facet, nrow = 1, scales = "free_x") +
  labs(
    title = "Areas under Normal Curves for Different (μ, σ)",
    subtitle = sprintf("Shaded probability P(%.2f ≤ X ≤ %.2f)", a, b),
    x = "x", y = "density"
  ) +
  theme_minimal(base_size = 12)

print(p)


