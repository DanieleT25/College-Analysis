source(here::here("R", "objHC.R"))

dicDist <- list(
  eu = "euclidean",
  man = "manhattan"
)

dictLink <- list(
  sng = "single",
  cmp = "complete",
  avg = "average",
  wrd = "ward.D2"
)

hierarchicalFunc <- function(df) {
  lstObj <- list()
  lstCorr <- numeric(0)
  lstType <- list()
  
  for (dist_name in unlist(dicDist)) {
    for (link_name in unlist(dictLink)) {
      tmp <- HierarClust$new(df, distance = dist_name, linkage = link_name)
      lstObj[[length(lstObj) + 1]] <- tmp
      lstCorr <- c(lstCorr, tmp$getCorr())
      lstType <- c(lstType, paste(dist_name, link_name, sep="_"))
    }
  }
  
  idx_max <- which.max(lstCorr)
  objMax <- lstObj[[idx_max]]
  
  return(list(
    objects = lstObj,
    dist_link = lstType,
    correlations = lstCorr,
    best_object = objMax
  ))
}

plotHierarchicalCorrTable <- function(hierarchical_result) {
  df_corr <- data.frame(
    Type = unlist(hierarchical_result$dist_link),
    Correlations = round(hierarchical_result$correlations, 4)
  )
  
  kbl <- kable(df_corr, align = "l", booktabs = TRUE) %>%
    kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover"))
  
  for (i in seq_len(nrow(df_corr))) {
    if (df_corr$Correlations[i] >= 0.75) {
      kbl <- row_spec(kbl, i, bold = TRUE)
    }
  }
  
  return(kbl)
}

get_stability_metrics_custom <- function(df, original_cluster, get_cluster_del, method = "euclidean") {
  apn_vec <- ad_vec <- adm_vec <- fom_vec <- numeric(ncol(df))
  
  for (j in 1:ncol(df)) {
    df_del <- df[, -j, drop = FALSE]
    clustering_del <- get_cluster_del(df_del)
    
    stab <- stability(mat = df,
                      del = j,
                      cluster = original_cluster,
                      clusterDel = clustering_del,
                      method = method)
    
    apn_vec[j] <- stab[1]
    ad_vec[j] <- stab[2]
    adm_vec[j] <- stab[3]
    fom_vec[j] <- stab[4]
  }
  
  return(list(
    APN = mean(apn_vec),
    AD = mean(ad_vec),
    ADM = mean(adm_vec),
    FOM = mean(fom_vec)
  ))
}