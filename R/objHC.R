HierarClust <- R6Class(
  "HierarClust",
  private = list(
    .df = NULL,
    .distance = NULL,
    .linkage = NULL,
    .D = NULL,
    .hc = NULL,
    .cph = NULL
  ),
  
  public = list(
    
    initialize = function(df, distance, linkage) {
      private$.df <- df
      private$.distance <- distance
      private$.linkage <- linkage
      private$.D <- dist(df, method = distance)
      private$.hc <- hclust(d = private$.D, method = linkage)
      private$.cph <- cor(as.numeric(private$.D), cophenetic(private$.hc))
    },
    
    getCorr = function() {
      return(private$.cph)
    },
    
    titleFunc = function(type) {
      return(paste(type,
                   "with",
                   private$.distance,
                   "distance and",
                   private$.linkage,
                   "linkage")
      )
    },
    
    dendrogram = function() {
      fviz_dend(private$.hc,
                main = self$titleFunc("Dendrogram"),
                show_labels = FALSE)
    },
    
    elbow = function(x) {
      fviz_nbclust(private$.df, hcut, method = "wss",
                   hc_metric=private$.distance,
                   hc_method=private$.linkage) +
        geom_vline(xintercept = x, linetype=2) +
        labs(subtitle = self$titleFunc("Elbow method"))
    },
    
    silhouette = function() {
      fviz_nbclust(private$.df, hcut, method = "silhouette",
                   hc_metric=private$.distance,
                   hc_method=private$.linkage) +
        labs(subtitle = self$titleFunc("Silhouette method"))
    },
    
    getHclust = function() {
      return(private$.hc)
    }
    
  )
)