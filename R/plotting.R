#
# Plotting functions
#
library(igraph)

#' Plots a discussion tree using nice parameters
#'
#' @param gtree an igraph object with a tree structure (no cicles)
#' @param labels the type of label to be shown over vertices (if any)
#' @details This is a function to fast plot the structure of conversations. 
#' If `label` is NA then no label is used.
#' @export
plot.tree <- function(gtree, labels=c('name', 'id')){
  # Plots a tree graph
  # Arguments:
  #   gtree: a igraph object graph with no cycles (tree)
  if (missing(labels)){
    labels <- NA
  }
  else{
  labels <- switch(labels, 
                   'name' = V(gtree)$name, 
                   'id' = as.numeric(V(gtree)))
  }
  par(mfrow=c(1,1))
  gtree.un <- as.undirected(gtree)
  la = layout_as_tree(gtree.un, mode='out', root=which.min(V(gtree.un)$date))

  plot(gtree.un,
       layout = la,
       vertex.label = labels,
       vertex.size=3,
       edge.arrow.size=0.6)
}
