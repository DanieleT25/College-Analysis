packages <- c(
  "ISLR2", "here", "DT", "skimr", "DataExplorer", "dplyr", "plotly", "ggplot2",
  "gamlss", "mixR", "R6", "ggsci", "ggpubr", "corrplot", "factoextra", 
  "knitr", "kableExtra", "cluster", "clustertend", "NbClust", "dbscan",
  "ClusterR", "clValid", "fpc", "fclust", "mclust"
)

installed_packages <- rownames(installed.packages())
for(p in packages){
  if(!p %in% installed_packages){
    install.packages(p)
  }
}

lapply(packages, library, character.only = TRUE)
