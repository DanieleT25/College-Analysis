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


# Funzione per eseguire tutti i clustering gerarchici
hierarchicalFunc <- function(df) {
  lstObj <- list()
  lstCorr <- numeric(0)
  lstType <- list()
  
  for (dist_name in unlist(dicDist)) {
    for (link_name in unlist(dictLink)) {
      # Crea oggetto
      tmp <- HierarClust$new(df, distance = dist_name, linkage = link_name)
      
      # Salva oggetto
      lstObj[[length(lstObj) + 1]] <- tmp
      
      # Salva correlazione cophenetica
      lstCorr <- c(lstCorr, tmp$getCorr())
      
      lstType <- c(lstType, paste(dist_name, link_name, sep="_"))
    }
  }
  
  # Trova oggetto con correlazione massima
  idx_max <- which.max(lstCorr)
  objMax <- lstObj[[idx_max]]
  
  # Mostra dendrogramma dell'oggetto migliore
  #objMax$dendrogram()
  
  # Restituisce la lista completa (se vuoi usarla dopo)
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
  
  # Tabelle base
  kbl <- kable(df_corr, align = "l", booktabs = TRUE) %>%
    kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover"))
  
  # Aggiunta grassetto alle righe con Correlations >= 0.75
  for (i in seq_len(nrow(df_corr))) {
    if (df_corr$Correlations[i] >= 0.75) {
      kbl <- row_spec(kbl, i, bold = TRUE)
    }
  }
  
  return(kbl)
}
# greedy_init <- function(x, k) {
#   n <- nrow(x)
#   S <- matrix(nrow = k, ncol = ncol(x))
# 
#   # 1. Primo centroide scelto a caso
#   first_index <- sample(1:n, 1)
#   S[1, ] <- x[first_index, , drop = FALSE]
# 
#   for (i in 2:k) {
#     # Calcola la distanza minima di ogni punto da quelli in S
#     dists <- apply(x, 1, function(row) {
#       min(sqrt(rowSums((S[1:(i - 1), , drop = FALSE] - row)^2)))
#     })
# 
#     # Trova il punto più lontano da quelli già in S
#     next_index <- which.max(dists)
#     S[i, ] <- x[next_index, , drop = FALSE]
#   }
# 
#   return(S)
# }


greedy_init <- function(x, k) {
  x <- as.matrix(x)
  n <- nrow(x)
  
  if (k > n) stop("k non può essere maggiore del numero di osservazioni")
  
  S <- matrix(nrow = k, ncol = ncol(x))
  
  # Primo centroide scelto a caso
  first_index <- sample(1:n, 1)
  S[1, ] <- x[first_index, , drop = FALSE]
  
  for (i in 2:k) {
    dists <- apply(x, 1, function(row) {
      min(sqrt(rowSums((S[1:(i - 1), , drop = FALSE] - row)^2)))
    })
    
    next_index <- which.max(dists)
    S[i, ] <- x[next_index, , drop = FALSE]
  }
  
  return(S)
}



# greedy_init <- function(x, k) {
#   n <- nrow(x)
#   S <- matrix(nrow = k, ncol = ncol(x))
#   selected_indices <- integer(0)
#   
#   # 1. Primo centroide scelto a caso
#   first_index <- sample(1:n, 1)
#   S[1, ] <- x[first_index, , drop = FALSE]
#   selected_indices <- c(selected_indices, first_index)
#   
#   for (i in 2:k) {
#     # Calcola distanza minima da punti in S
#     dists <- apply(x, 1, function(row) {
#       min(sqrt(rowSums((S[1:(i - 1), , drop = FALSE] - row)^2)))
#     })
#     
#     # Escludi i punti già selezionati
#     dists[selected_indices] <- -1  
#     
#     # Scegli il punto più lontano tra quelli non selezionati
#     next_index <- which.max(dists)
#     S[i, ] <- x[next_index, , drop = FALSE]
#     selected_indices <- c(selected_indices, next_index)
#   }
#   
#   return(S)
# }

greedy_init <- function(x, k) {
  n <- nrow(x)
  if (k > nrow(unique(x))) {
    stop("k cannot be greater than the number of distinct observations in the dataset.")
  }
  
  S <- matrix(nrow = k, ncol = ncol(x))
  selected_indices <- integer(0)
  
  # Primo centroide scelto a caso
  first_index <- sample(1:n, 1)
  S[1, ] <- x[first_index, , drop = FALSE]
  selected_indices <- c(selected_indices, first_index)
  
  for (i in 2:k) {
    # Calcola distanza minima da punti in S
    dists <- apply(x, 1, function(row) {
      min(sqrt(rowSums((S[1:(i - 1), , drop = FALSE] - row)^2)))
    })
    
    # Escludi i punti già scelti
    dists[selected_indices] <- -1  
    
    # Trova il prossimo indice valido
    next_index <- which.max(dists)
    if (dists[next_index] == -1) {
      stop("Not enough unique points to initialize all centroids.")
    }
    
    # Aggiungi il nuovo centroide
    S[i, ] <- x[next_index, , drop = FALSE]
    selected_indices <- c(selected_indices, next_index)
  }
  
  return(S)
}


