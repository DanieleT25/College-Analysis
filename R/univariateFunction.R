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
