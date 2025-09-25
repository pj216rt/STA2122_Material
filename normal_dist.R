#want R code showing that the normal distribution can take on many different
#appearances

#need to pick parameters
params <- data.frame(
  mu    = c(0),
  sigma = c(0.1, 1, 5, 10),
  label = c("N(0, 0.1)", "N(0, 1)", "N(0, 5)", "N(0, 10)")
)

#areas to find area between
a <- -2
b <- 2

#dataframes for curves, shaded areas, areas
curve_df <- data.frame()
shade_df <- data.frame()
areas <- data.frame()

for (i in seq_len(nrow(params))) {
  mu  <- params$mu[i]
  sg  <- params$sigma[i]
  lab <- params$label[i]
  
  # panel range that covers most mass
  x_min <- mu - 4*sg
  x_max <- mu + 4*sg
  x     <- seq(x_min, x_max, by = 0.001)
  den   <- dnorm(x, mean = mu, sd = sg)
  
  curve_df <- rbind(curve_df, data.frame(label = lab, x = x, den = den))
  
  # clip [a,b] to panel range so ribbon draws only when it overlaps
  a_clip <- max(a, x_min)
  b_clip <- min(b, x_max)
  if (a_clip < b_clip) {
    xs  <- seq(a_clip, b_clip, by = 0.001)
    dns <- dnorm(xs, mean = mu, sd = sg)
    shade_df <- rbind(shade_df, data.frame(label = lab, x = xs, den = dns))
  }
  
  areas <- rbind(areas, data.frame(
    label = lab, mu = mu, sigma = sg,
    area = pnorm(b, mu, sg) - pnorm(a, mu, sg)
  ))
}

areas$facet <- sprintf("%s\nArea[a,b]=%.3f", areas$label, areas$area)
curve_df$facet <- factor(curve_df$label, levels = areas$label, labels = areas$facet)
shade_df$facet <- factor(shade_df$label, levels = areas$label, labels = areas$facet)


p <- ggplot() +
  geom_ribbon(data = shade_df, aes(x = x, ymin = 0, ymax = den), 
              fill = "steelblue", alpha = 0.35) + 
  geom_line(data = curve_df, aes(x = x, y = den),
            linewidth = 1.1, color = "black") +
  geom_vline(xintercept = c(a, b), linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~ facet, nrow = 1, scales = "free_x") +
  labs(
    title = "Areas under Normal Curves for Different (μ, σ)",
    subtitle = sprintf("Shaded probability P(%.2f ≤ X ≤ %.2f)", a, b),
    x = "x", y = "density"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

print(p)

