library(ggplot2)

plotHist <- function(vec, bins = 30) {
  df <- data.frame(x = vec)
  ggplot(df, aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins, fill = "steelblue", color = "black", alpha = 0.6) +
    geom_density(color = "darkred", size = 1) +
    geom_rug(sides = "b", alpha = 0.5) +
    labs(title = "Histogram and Density", x = "Value", y = "Density") +
    theme_minimal()
}