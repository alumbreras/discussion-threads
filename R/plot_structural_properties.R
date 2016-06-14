# Functions to compare structural properties (degree distribution, subtree size, size vs depth)
# of two thread datasets (e.g. a synthetic dataset and a real dataset)
library(igraph)
library(parallel)
library(foreach)
library(doParallel)
library(data.table)

compare_trees <- function(trees.data, trees.lum, trees.gom){
#  ncores <- detectCores() - 2
#  cl <- makeCluster(ncores, outfile="", port=11439)

  ############################
  # Statistics
  ############################
  # width vs mean depth
  width <- function(tree){
    max(diff(c(1, sapply(1:diameter(tree), function(i) ego_size(tree, order = i, nodes = 1)))))
  }

  # degree distribution
  cat("\nDegree distributions")
  degrees.data <- do.call(c, lapply(trees.data, degree, mode="all"))
  degrees.lum <- do.call(c, lapply(trees.lum, degree, mode="all"))
  degrees.gom <- do.call(c, lapply(trees.gom, degree, mode="all"))

  # subtree size (without the ego)
  cat("\nSubtree sizes")
  subtrees.size.data <- do.call(c, lapply(trees.data, ego_size, order=1000, mode='in')) -1
  subtrees.size.lum <- do.call(c, lapply(trees.lum, ego_size, order=1000, mode='in')) -1
  subtrees.size.gom <- do.call(c, lapply(trees.gom, ego_size, order=1000, mode='in')) -1

  # size - depth - width
  cat("\nSizes vs Depth")
  depths.data <- do.call(c, lapply(trees.data, function(tree) diameter(tree)))
  widths.data <- do.call(c, lapply(trees.data, function(tree) width(tree)))
  sizes.data <- do.call(c, lapply(trees.data, function(tree) vcount(tree)))
  depths.lum <- do.call(c, lapply(trees.lum, function(tree) diameter(tree)))
  widths.lum <- do.call(c, lapply(trees.lum, function(tree) width(tree)))
  sizes.lum <- do.call(c, lapply(trees.lum, function(tree) vcount(tree)))
  depths.gom <- do.call(c, lapply(trees.gom, function(tree) diameter(tree)))
  widths.gom <- do.call(c, lapply(trees.gom, function(tree) width(tree)))
  sizes.gom<- do.call(c, lapply(trees.gom, function(tree) vcount(tree)))

  # size - depth  (mean depth for every size)
  cat("\nSizes vs Mean Depth ")
  df.data <- data.frame(size=sizes.data, depth=depths.data, width=widths.data)
  df.data.mean_depth_by_size <- aggregate(df.data$depth, list(size=df.data$size), FUN=mean)
  df.lum <- data.frame(size=sizes.lum, depth=depths.lum, width=widths.lum)
  df.lum.mean_depth_by_size <- aggregate(df.lum$depth, list(size=df.lum$size), FUN=mean)
  df.gom <- data.frame(size=sizes.gom, depth=depths.gom, width=widths.gom)
  df.gom.mean_depth_by_size <- aggregate(df.gom$depth, list(size=df.gom$size), FUN=mean)

  # size - depth - width (evolution)
  cat("\nSizes vs Depth - evol")
  depths.data.evol <- do.call(c, lapply(trees.data, function(tree) sapply(1:vcount(tree), function(i) diameter(induced_subgraph(tree, 1:i)) )))
  widths.data.evol <- do.call(c, lapply(trees.data, function(tree) sapply(1:vcount(tree), function(i) width(induced_subgraph(tree, 1:i)) )))
  depths.lum.evol <- do.call(c, lapply(trees.lum, function(tree) sapply(1:vcount(tree), function(i) diameter(induced_subgraph(tree, 1:i)) )))
  widths.lum.evol <- do.call(c, lapply(trees.lum, function(tree) sapply(1:vcount(tree), function(i) width(induced_subgraph(tree, 1:i)) )))
  depths.gom.evol <- do.call(c, lapply(trees.gom, function(tree) sapply(1:vcount(tree), function(i) diameter(induced_subgraph(tree, 1:i)) )))
  widths.gom.evol <- do.call(c, lapply(trees.gom, function(tree) sapply(1:vcount(tree), function(i) width(induced_subgraph(tree, 1:i)) )))


  # width - depth (mean depth for every width)
  cat("\nSizes vs Mean Depth - evol")
  df.data.evol <- data.frame(depth=depths.data.evol, width=widths.data.evol)
  df.lum.evol <- data.frame(depth=depths.lum.evol, width=widths.lum.evol)
  df.gom.evol <- data.frame(depth=depths.gom.evol, width=widths.gom.evol)
  df.data.mean_depth_by_width <- aggregate(df.data.evol$depth, list(width=df.data.evol$width), FUN=mean)
  df.lum.mean_depth_by_width <- aggregate(df.lum.evol$depth, list(width=df.lum.evol$width), FUN=mean)
  df.gom.mean_depth_by_width <- aggregate(df.gom.evol$depth, list(width=df.gom.evol$width), FUN=mean)


  ##################################
  # Plot statistics (data vs model)
  ##################################
  #TODO: for each statistic create a dataframe with the three cases (data, model 1, model 2)

  # Properties of the full threads
  #################################
  # Degree distributions
  par(mfrow=c(2,3))
  n.data <- hist(degrees.data, breaks=0:max(degrees.data), plot=FALSE)$counts
  n.lum <- hist(degrees.lum, breaks=0:max(degrees.lum), plot=FALSE)$counts
  n.gom <- hist(degrees.gom, breaks=0:max(degrees.gom), plot=FALSE)$counts
  n.data <- n.data/sum(n.data)
  n.lum <- n.lum/sum(n.lum)
  n.gom <- n.gom/sum(n.gom)

  plot(1:max(degrees.data), n.data, xaxt="n", yaxt="n", log='xy',
       ylab='probability', xlab='total degrees', pch=19, cex=0.5, col='black', ylim = c(10**-3.9,10**-0))
  points(1:max(degrees.lum), n.lum, pch=19, cex=0.5, col='red')
  points(1:max(degrees.gom), n.gom, pch=19, cex=0.5, col='blue')

  xlabels <- expression(10^0, 10^1, 10^2, 10^3)
  axis(1, at=c(1, 10, 100, 1000), labels=xlabels)
  ylabels <- expression(10^-6, 10^-5, 10^-4, 10^-3, 10^-2, 10^-1,  10^0)
  axis(2, at=c(10^(-6), 10^(-5), 10^(-4), 10^(-3), 10^(-2), 10^(-1),  10^0), labels=ylabels, las=1)
  legend("topright", c('data', 'lum', 'gom'), col=c('black', 'red', 'blue'), pch=19, inset = 0.05)
  title(main='Degree distribution')

  # Subtree size distribution
  n.data <- hist(subtrees.size.data, breaks=0:max(subtrees.size.data), plot=FALSE)$counts
  n.lum <- hist(subtrees.size.lum, breaks=0:max(subtrees.size.lum), plot=FALSE)$counts
  n.gom <- hist(subtrees.size.gom, breaks=0:max(subtrees.size.gom), plot=FALSE)$counts
  n.data <- n.data/sum(n.data)
  n.lum <- n.lum/sum(n.lum)
  n.gom <- n.gom/sum(n.gom)

  plot(1:max(subtrees.size.data), n.data, xaxt="n", yaxt="n", log='xy',
       ylab='probability', xlab='subtree size', pch=19, cex=0.5, col='black', ylim = c(10**-3.9,10**-0))
  points(1:max(subtrees.size.lum), n.lum, pch=19, cex=0.5, col='red')
  points(1:max(subtrees.size.gom), n.gom, pch=19, cex=0.5, col='blue')
  xlabels <- expression(10^0, 10^1, 10^2, 10^3)
  axis(1, at=c(1, 10, 100, 1000), labels=xlabels)
  ylabels <- expression(10^-6, 10^-5, 10^-4, 10^-3, 10^-2, 10^-1,  10^0)
  axis(2, at=c(10^(-6), 10^(-5), 10^(-4), 10^(-3), 10^(-2), 10^(-1),  10^0), labels=ylabels, las=1)
  legend("topright", c('data', 'lum', 'gom'), col=c('black', 'red', 'blue'), pch=19, inset = 0.05)
  title(main='Subtree size distribution')

  # Size vs Mean depth
  plot(df.data.mean_depth_by_size, xaxt="n", yaxt="n", log='xy',
       ylab='depth', xlab='size', pch=19, cex=0.5, col='black')
  points(df.lum.mean_depth_by_size, pch=19, cex=0.5, col='red')
  points(df.gom.mean_depth_by_size, pch=19, cex=0.5, col='blue')
  xlabels <- expression(10^0, 10^1, 10^2, 10^3)
  axis(1, at=c(1, 10, 100, 1000), labels=xlabels)
  axis(2, at=c(1, 10, 100, 1000), labels=xlabels)
  legend("topright", c('data', 'lum', 'gom'), col=c('black', 'red', 'blue'), pch=19, inset = 0.05)
  title(main='Size vs Mean depth')

  # Properties of the evolving threads
  ####################################

  # Mean vs Depth
  plot(df.data.mean_depth_by_width,  xaxt="n", yaxt="n", log='xy',
       ylab='depth', xlab='width', pch=19, cex=0.5, col='black')
  points(df.lum.mean_depth_by_width, pch=19, cex=0.5, col='red')
  points(df.gom.mean_depth_by_width, pch=19, cex=0.5, col='blue')
  xlabels <- expression(10^0, 10^1, 10^2, 10^3)
  axis(1, at=c(1, 10, 100, 1000), labels=xlabels)
  axis(2, at=c(1, 10, 100, 1000), labels=xlabels)
  legend("bottomright", c('data', 'lum', 'gom'), col=c('black', 'red', 'blue'), pch=19, inset = 0.05)
  title(main='Width vs Mean depth (evol)')

}
