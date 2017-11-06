# Given a set of trees, estimate its parameters
# (model Gomez 2013)
# author: Alberto Lumbreras
################################################
source('R/likelihood.r')
library(parallel)
library(data.table)
library(neldermead)
library(dfoptim)
library(dplyr)
library(ggplot2)
source('R/thread_generators.r')
source('R/likelihood.r')
source('R/extract_from_db.r')

tree.to.data <- function(g, thread=0){
  # Creates a dataframe with a row per post
  # and columns "degree of parent", "is_parent_root", "lag to parent", and "t"
  # With the model parameters, the three first columns are used to compute the numerator the likelihood
  # and 't' to compute the denominator of the likelihood
  
  parents <- get.edgelist(g, names=FALSE)[,2] # parents vector without the first two posts
  authors <- V(g)$user[-1] # remove first post
  popularities <- c(1,sapply(2:length(parents), function(t) 1 + sum(parents[1:(t-1)]==parents[t])))
  #popularities[parents==1] <- popularities[parents==1]-1 # the root node has no parent
  posts <- 2:(length(parents)+1)
  data <- data.frame(thread = rep(thread, length(posts)),
                     user = authors,
                     post = posts,
                     t = 1:(length(parents)),
                     parent = parents,
                     popularity = popularities)
  
  data <- mutate(data, lag=t-parent+1)
  data
}


# Set up the clusters for parallelization
ncores <- detectCores() - 2
cl <- makeCluster(ncores, outfile="", port=11439)
clusterEvalQ(cl, {library(igraph); library(dplyr)})

###############################
# Load real tree graphs
###############################

# Load thread ids
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
trees <- parLapply(cl, trees[idx], function(x) x$gp)

#save(trees, file="tree.rda")
#load("tree.rda")

# Create dataframes from trees
data.parlapply <- parLapply(cl, trees, tree.to.data)

# join dataframes adding thread id
for (i in 1:length(data.parlapply)){
  data.parlapply[[i]]$thread <- i
}
data.parlapply <- rbindlist(data.parlapply)
df.trees <- data.frame(thread = data.parlapply$thread, 
                       user = data.parlapply$user,
                       post = data.parlapply$post,
                       t = data.parlapply$t,
                       parent = data.parlapply$parent,
                       popularity = data.parlapply$popularity,
                       lag = data.parlapply$lag)

save(df.trees, file="df.trees.gof.rda")
#load("df.trees.rda")
########################################
# Estimation
########################################

# Likelihood computation using the dataframe
likelihood.post <- function(row, alpha, beta, tau){
  (log(alpha * row['popularity'] + beta*(row['parent']==1) + tau^row['lag']) - 
     log(2*alpha*(row['t']-1/2)   + beta + tau*(tau^row['t']-1)/(tau-1)))
  # -1/2 because root has at least degree 1 (to follow Gomez 2013)
  # if the root starts with degree 0, then it should be:
  # log(2*alpha*(row['t']-1)   + beta + tau*(tau^row['t']-1)/(tau-1))
}

Qopt <- function(params, df.trees){
  sum(apply(df.trees[-2], 1, function(i) likelihood.post(i, params[1], params[2], params[3])))
}

sol <- nmkb(c(1, 0.1, 0.5), Qopt, 
            lower = c(0,0,0), upper = c(Inf, Inf, 1), 
            control = list(maximize=TRUE),
            df.trees = df.trees)
cat('\n', sol$par)