library(dplyr)
library(plotly)
library(ggplot2)

plotBoxPlot <- function(vec, nameVariable) {
  media <- mean(vec)
  
  plot_ly(
    x = vec,
    type = 'box',
    name = '',
    boxmean=TRUE
  ) %>%
    layout(
      title = paste("Boxplot of", nameVariable),
      xaxis = list(
        zeroline = FALSE
      )
    )
}

plotHist <- function(vec, nameVariable, bins = 30) {
  df <- data.frame(x = vec)
  ggplot(df, aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins, fill = "steelblue", color = "black", alpha = 0.6) +
    geom_density(color = "darkred", linewidth = 1) +
    geom_rug(sides = "b", alpha = 0.5) +
    labs(title = paste("Histogram and Density of", nameVariable), x = "Value", y = "Density") +
    theme_minimal()
}

plotHistModel <- function(variable, model) {
  histModel <- histDist(variable, model$family[1] , density=TRUE, line.col=c(1,1), line.ty=c(1,2))
  histModel
}

plotHistModelMix <- function(variable, model) {
  colors <- c("red3", "green3")
  x_vals <- seq(min(variable), max(variable), length.out = 500)
  
  hist(variable, breaks = 30, probability = T, col = "white", border = "black",
       main = "Gamma Mixture Model K = 2",
       xlab = "Values", ylab = "Density")
  
  for (i in 1:length(model$pi)) {
    lines(x_vals, model$pi[i] * dgamma(x_vals, shape = model$alpha[i], rate = model$lambda[i]), col = colors[i], lwd = 2)
  }
  
  overall_density <- rowSums(sapply(1:length(model$pi), function(i) {
    model$pi[i] * dgamma(x_vals, shape = model$alpha[i], rate = model$lambda[i])
  }))
  lines(x_vals, overall_density, col = "black", lwd = 2, lty = 2)
}