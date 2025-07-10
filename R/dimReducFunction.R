computeDf <- function(pca) {
  eig_vals <- pca$sdev^2
  explained_var <- eig_vals / sum(eig_vals)
  cumulative_var <- cumsum(explained_var)
  
  df <- data.frame(
    PC = paste0("PC", 1:length(explained_var)),
    Explained = explained_var,
    Cumulative = cumulative_var
  )
  
  return(df)
}

plotScreeCore <- function(df, y_val, y_label, y_text, title) {
  ggplot(df, aes(x = seq_along(Explained))) +
    geom_col(aes(y = Explained), fill = "steelblue") +
    geom_line(aes(y = y_val), color = "red", linewidth = 1.2) +
    geom_point(aes(y = y_val), color = "red", size = 2) +
    geom_text(aes(x = seq_along(Explained) + 0.2, y = y_val, label = y_text),
              vjust = -0.8, color = "black", size = 3) +
    labs(x = "Principal Component", y = y_label, title = title) +
    scale_x_continuous(breaks = 1:length(df$Explained)) +
    theme_minimal()
}

screePlot <- function(pca, cumulative=FALSE) {
  df <- computeDf(pca)
  
  if (cumulative) {
    y_val <- df$Cumulative
    y_label <- "Cumulative Variance Explained"
    y_text <- paste0(round(df$Cumulative * 100, 1), "%")
    title <- "Scree Plot with Cumulative Variance"
  } else {
    y_val <- df$Explained
    y_label <- "Variance Explained"
    y_text <- paste0(round(df$Explained * 100, 1), "%")
    title <- "Scree Plot"
  }
  
  plotScreeCore(df, y_val, y_label, y_text, title)
}

createLoadingsDf <- function(pca, n_pc, threshold) {
  loadings_df <- as.data.frame(pca$rotation[, 1:n_pc])
  loadings_df <- loadings_df %>%
    mutate(across(everything(), ~ ifelse(abs(.) > threshold, round(., 3), "")))
  
  return(loadings_df)
}


plotLoadingsTable <- function(pca, n_pca, threshold) {
  loadings_df <- createLoadingsDf(pca, n_pca, threshold)
  
  kable(loadings_df, align = "c", booktabs = TRUE) %>%
    kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover"))
}

computeDf_eigen <- function(pca) {
  eigenvalues <- pca$sdev^2
  nComp <- length(which(eigenvalues > 1))
  
  df_eigen <- data.frame(
    PC = paste0("PC", 1:length(eigenvalues)),
    Eigenvalue = eigenvalues
  )
  return(df_eigen)
}

kaiserRulePlot <- function(df_eigen) {
  ggplot(df_eigen, aes(x = seq_along(Eigenvalue), y = Eigenvalue)) +
    geom_col(fill = "steelblue") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = 1:nrow(df_eigen), labels = df_eigen$PC) +
    labs(title = "Scree plot with eigenvalues (Kaiser rule)",
         x = "Principal Components", y = "Eigenvalue") +
    theme_minimal()
}

kaiserRule <- function(pca) {
  df_eigen <- computeDf_eigen(pca)
  kaiserRulePlot(df_eigen)
}