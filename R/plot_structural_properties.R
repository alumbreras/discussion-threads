# Load graphs of real dataset
# Generate synthetic graphs with some given parameters
# Compare structural properties (degree distribution, subtree size, size vs depth)

library(igraph)
library(parallel)
library(foreach)
library(doParallel)
library(data.table)
source('R/plotting.r')
source('R/thread_generators.r')
source('R/extract_from_db.r')

ncores <- detectCores() - 2
cl <- makeCluster(ncores, outfile="", port=11439)

###############################
# Load real tree graphs
###############################
#load('./R_objects/dfposts_podemos.Rda')
load('./R_objects/dfposts_gameofthrones.Rda')

df.threads <- plyr::count(df.posts, "thread") %>% filter(freq>10)
#df.threads <- df.threads[1:1000,]

# Extract graphs of threads. Ignore NA graphs (exceptions due, e.g. to root not being the oldest post)
clusterEvalQ(cl, {
  library(igraph); library(RSQLite); source('R/extract_from_db.r');
  con <- dbConnect(dbDriver("SQLite"), dbname = paste0("./data/reddit.db"));
})
trees <- parLapply(cl, df.threads$thread, function(x) database.to.graph(x, con=con, 'reddit'))
idx <- sapply(1:length(trees), function(i) class(trees[[i]])!='try-error') # detect NA results
trees.data <- parLapply(cl, trees[idx], function(x) x$gp)

###########################
# Generate synthetic trees
###########################

models <- c("Gomez2011", "Gomez2013", "Lumbreras2016unicluster", "Lumbreras2016")
model <- models[2]

ntrees <- length(trees.data) # same number of synthetic and real trees
thread.lengths <- df.threads$freq # same lengths than real data

# Chose one of the models below
##########################################
# Gomez 2011
if(model == "Gomez2011"){
  alpha.root <- 0.734
  alpha.c <- 0.683
  beta.root <- 1.302
  trees <- replicate(ntrees,  gen.thread.Gomez2011(n=25, alpha.root=alpha.root, alpha.c=alpha.c, beta.root=beta.root), simplify = FALSE)
}

# Gomez 2013
if(model == "Gomez2013"){
  
  # Estimation for Podemos >10 posts: 9.007232e-05 5.48709 0.9504394
  # Estimation for Game of Thrones > 10 posts:  0.07195756 4.043651 0.9372917

  alpha <- 9.007232e-05
  beta <- 5.48709 
  tau <- 0.9504394
  
  alpha <- 5.007232e-01
  beta <- 4.48709 
  tau <- 0.504394
  
  alpha <-  0.07195756 
  beta <- 4.043651 
  tau <- 0.9372917
  
  trees <- lapply(thread.lengths, function(x) gen.thread.Gomez2013(n=x, alpha=alpha, beta=beta, tau=tau))
}

# Lumbreras 2016 one cluster, one user
if(model == "Lumbreras2016unicluster"){
  users.by.cluster <- 50 
  alphas <- 1
  betas <- 0.68
  taus <- 0.75
  z <- do.call(c, lapply(1:length(alphas), function(x) rep(x, users.by.cluster)))
  
  # parallel
  cl <- makeCluster(ncores)  
  clusterEvalQ(cl, {library(igraph); source('R/thread_generators.r')})
  clusterExport(cl, c("alphas", "betas", "taus", "z"))
  trees <- parLapply(cl, 1:ntrees, function(i) gen.thread.Lumbreras2016(n=25, z=z, alphas=alphas, betas=betas, taus=taus) )
  stopCluster(cl)
}

# Lumbreras 2016
if(model == "Lumbreras2016"){
  users.by.cluster <- 1000 
  alphas <- c(0.1, 0.5, 1)
  betas <- c(1, 5, 10)
  taus <- c(0.1, 0.5, 0.99)
  z <- do.call(c, lapply(1:length(alphas), function(x) rep(x, users.by.cluster)))
  
  # parallel
  cl <- makeCluster(ncores)  
  clusterEvalQ(cl, {library(igraph); source('R/thread_generators.r')})
  clusterExport(cl, c("alphas", "betas", "taus", "z"))
  trees <- parLapply(cl, 1:ntrees, function(i) gen.thread.Lumbreras2016(n=25, z=z, alphas=alphas, betas=betas, taus=taus) )
  stopCluster(cl)
}

stopCluster(cl)


############################
# Statistics
############################
# width vs mean depth
width <- function(tree){
  max(diff(c(1, sapply(1:diameter(tree), function(i) ego_size(tree, order = i, nodes = 1)))))
}

# degree distribution
degrees.data <- do.call(c, lapply(trees.data, degree, mode="all"))
degrees.syn <- do.call(c, lapply(trees, degree, mode="all")) 

# subtree size (without the ego)
subtrees.size.data <- do.call(c, lapply(trees.data, ego_size, order=1000, mode='in')) -1
subtrees.size.syn <- do.call(c, lapply(trees, ego_size, order=1000, mode='in')) -1

# size - depth - width
depths.data <- do.call(c, lapply(trees.data, function(tree) diameter(tree)))
widths.data <- do.call(c, lapply(trees.data, function(tree) width(tree)))
sizes.data <- do.call(c, lapply(trees.data, function(tree) vcount(tree)))
depths.syn <- do.call(c, lapply(trees, function(tree) diameter(tree)))
widths.syn <- do.call(c, lapply(trees, function(tree) width(tree)))
sizes.syn <- do.call(c, lapply(trees, function(tree) vcount(tree)))

# size - depth  (mean depth for every size)
df.data <- data.frame(size=sizes.data, depth=depths.data, width=widths.data)
df.syn <- data.frame(size=sizes.syn, depth=depths.syn, width=widths.syn)
df.data.mean_depth_by_size <- aggregate(df.data$depth, list(size=df.data$size), FUN=mean)
df.syn.mean_depth_by_size <- aggregate(df.syn$depth, list(size=df.syn$size), FUN=mean)

# size - depth - width (evolution)
depths.data.evol <- do.call(c, lapply(trees.data, function(tree) sapply(1:vcount(tree), function(i) diameter(induced_subgraph(tree, 1:i)) )))
widths.data.evol <- do.call(c, lapply(trees.data, function(tree) sapply(1:vcount(tree), function(i) width(induced_subgraph(tree, 1:i)) )))
depths.syn.evol <- do.call(c, lapply(trees, function(tree) sapply(1:vcount(tree), function(i) diameter(induced_subgraph(tree, 1:i)) )))
widths.syn.evol <- do.call(c, lapply(trees, function(tree) sapply(1:vcount(tree), function(i) width(induced_subgraph(tree, 1:i)) )))

# width - depth (mean depth for every width)
df.data.evol <- data.frame(depth=depths.data.evol, width=widths.data.evol)
df.syn.evol <- data.frame(depth=depths.syn.evol, width=widths.syn.evol)
df.data.mean_depth_by_width <- aggregate(df.data.evol$depth, list(width=df.data.evol$width), FUN=mean)
df.syn.mean_depth_by_width <- aggregate(df.syn.evol$depth, list(width=df.syn.evol$width), FUN=mean)

##################################
# Plot statistics (data vs model)
##################################

# Properties of the full threads
#################################
# Degree distributions
par(mfrow=c(2,3))
n.data <- hist(degrees.data, breaks=0:max(degrees.data), plot=FALSE)$counts
n.syn <- hist(degrees.syn, breaks=0:max(degrees.syn), plot=FALSE)$counts
n.data <- n.data/sum(n.data)
n.syn <- n.syn/sum(n.syn)

plot(1:max(degrees.data), n.data, xaxt="n", yaxt="n", log='xy', 
     ylab='probability', xlab='total degrees', pch=19, cex=0.5, col='black', ylim = c(10**-3.9,10**-0))
points(1:max(degrees.syn), n.syn, pch=19, cex=0.5, col='red')
xlabels <- expression(10^0, 10^1, 10^2, 10^3)
axis(1, at=c(1, 10, 100, 1000), labels=xlabels)
ylabels <- expression(10^-6, 10^-5, 10^-4, 10^-3, 10^-2, 10^-1,  10^0)
axis(2, at=c(10^(-6), 10^(-5), 10^(-4), 10^(-3), 10^(-2), 10^(-1),  10^0), labels=ylabels, las=1)
legend("topright", c('data', 'model'), col=c('black', 'red'), pch=19, inset = 0.05)
title(main='Degree distribution')

# Subtree size distribution
n.data <- hist(subtrees.size.data, breaks=0:max(subtrees.size.data), plot=FALSE)$counts
n.syn <- hist(subtrees.size.syn, breaks=0:max(subtrees.size.syn), plot=FALSE)$counts
n.data <- n.data/sum(n.data)
n.syn <- n.syn/sum(n.syn)

plot(1:max(subtrees.size.data), n.data, xaxt="n", yaxt="n", log='xy', 
     ylab='probability', xlab='subtree size', pch=19, cex=0.5, col='black', ylim = c(10**-3.9,10**-0))
points(1:max(subtrees.size.syn), n.syn, pch=19, cex=0.5, col='red')
xlabels <- expression(10^0, 10^1, 10^2, 10^3)
axis(1, at=c(1, 10, 100, 1000), labels=xlabels)
ylabels <- expression(10^-6, 10^-5, 10^-4, 10^-3, 10^-2, 10^-1,  10^0)
axis(2, at=c(10^(-6), 10^(-5), 10^(-4), 10^(-3), 10^(-2), 10^(-1),  10^0), labels=ylabels, las=1)
legend("topright", c('data', 'model'), col=c('black', 'red'), pch=19, inset = 0.05)
title(main='Subtree size distribution')

# Size vs Mean depth
plot(df.data.mean_depth_by_size, xaxt="n", yaxt="n", log='xy', 
     ylab='depth', xlab='size', pch=19, cex=0.5, col='black')
points(df.syn.mean_depth_by_size, pch=19, cex=0.5, col='red')
xlabels <- expression(10^0, 10^1, 10^2, 10^3)
axis(1, at=c(1, 10, 100, 1000), labels=xlabels)
axis(2, at=c(1, 10, 100, 1000), labels=xlabels)
legend("topright", c('data', 'model'), col=c('black', 'red'), pch=19, inset = 0.05)
title(main='Size vs Mean depth')

# Properties of the evolving threads
####################################

# Mean vs Depth
plot(df.data.mean_depth_by_width,  xaxt="n", yaxt="n", log='xy', 
     ylab='depth', xlab='width', pch=19, cex=0.5, col='black')
points(df.syn.mean_depth_by_width, pch=19, cex=0.5, col='red')
xlabels <- expression(10^0, 10^1, 10^2, 10^3)
axis(1, at=c(1, 10, 100, 1000), labels=xlabels)
axis(2, at=c(1, 10, 100, 1000), labels=xlabels)
legend("bottomright", c('data', 'model'), col=c('black', 'red'), pch=19, inset = 0.05)
title(main='Width vs Mean depth (evol)')
