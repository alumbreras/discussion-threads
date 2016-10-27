# Functions to load real datasets
# and to generate synthetic ones
#################################
library(doParallel)
library(parallel)
library(data.table)
library(dplyr)
library(igraph)

#' Build a set of tree objects from the information in the database
#' @param forum the forum (reddit, slashdot,...)
#' @param subforum the subforum (topic)
#' @params min.size minimum thread size.
#' @details This function builds a set of tree objects from the information stored in the databased.
#' For instance, if forum='reddit' and subforum='gameofthrones' then it will extract all posts in the
#' reddit database that belong to gameofthrones and will build a list of trees where every tree represents
#' a discussion thread in gameofthrones.
#' @return a list of trees as igraph objects
#' @export
load_trees <- function(forum='reddit', subforum='gameofthrones', min.size=10){
  
  ncores <- detectCores() - 2
  cl <- makeCluster(ncores, outfile="", port=11439)
  
  load(paste0('./data/dfposts_', subforum, '.Rda'))
  df.threads <- plyr::count(df.posts, "thread") %>% filter(freq > min.size)
  
  # Extract graphs of threads. Ignore NA graphs (exceptions due, e.g. to root not being the oldest post)
  clusterEvalQ(cl, {
    library(igraph); library(RSQLite); source('R/extract_from_db.R');
    con <- dbConnect(dbDriver("SQLite"), dbname = paste0("./data/reddit.db"));
  })
  trees <- parLapply(cl, df.threads$thread, function(x) database.to.graph(x, con=con, 'reddit'))
  idx <- sapply(1:length(trees), function(i) class(trees[[i]])!='try-error') # detect NA results
  trees <- parLapply(cl, trees[idx], function(x) x$gp)
  
  stopCluster(cl)
  
  trees
}


#' Build a set of tree objects from the information in the dataframe
#' @param forum the forum (reddit, slashdot,...)
#' @param subforum the subforum (topic)
#' @params min.size minimum thread size.
#' @details This function builds a set of tree objects from the posts information stored in a dataframe.
#' For instance, if forum='reddit' and subforum='gameofthrones' then it will extract all posts in the
#' reddit database that belong to gameofthrones and will build a list of trees where every tree represents
#' a discussion thread in gameofthrones.
#' @return a list of trees as igraph objects
#' @export
load_trees_from_dataframe <- function(subforum='gameofthrones', min.size=10){
  
  ncores <- detectCores() - 2
  cl <- makeCluster(ncores, outfile="", port=11439)
  
  load(paste0('./data/dfposts_', subforum, '.Rda'))
  df.threads <- plyr::count(df.posts, "thread") %>% filter(freq > min.size)
  
  # Extract graphs of threads. Ignore NA graphs (exceptions due, e.g. to root not being the oldest post)
  clusterEvalQ(cl, {
    library(igraph); library(RSQLite); source('R/extract_from_db.R');
    con <- dbConnect(dbDriver("SQLite"), dbname = paste0("./data/reddit.db"));
  })
  trees <- parLapply(cl, df.threads$thread, function(x) database.to.graph(x, con=con, 'reddit'))
  idx <- sapply(1:length(trees), function(i) class(trees[[i]])!='try-error') # detect NA results
  trees <- parLapply(cl, trees[idx], function(x) x$gp)
  
  stopCluster(cl)
  
  trees
}








#' Generate n trees of sizes s following according to a model with given parameters
#' @param model likelihood model that drives parent choice for every post
#' @param params a vector of model parameters
#' @param n total number of threads
#' @param s size of each thread
#' @details The trees are generated in parallel
#' @return a list of trees
#' @export
generate_trees <- function(model, params, sizes, ...){
  ncores <- detectCores() - 2
  cl <- makeCluster(ncores, outfile="", port=11439)
  
  if(model=='Gomez2013'){
    alpha <-  params[1]
    beta <- params[2]
    tau <- params[3]
    
    clusterEvalQ(cl, {library(igraph); source('R/thread_generators.R')})
    clusterExport(cl, c("alpha", "beta", "tau"), envir = environment())
    
    trees <- parLapply(cl, sizes, function(i){
      gen.thread.Gomez2013(n=i, alpha=alpha, 
                           beta = beta, 
                           tau=tau)})
  }
  
  if(model=='Lumbreras2016'){
    alphas <- params$alphas
    betas <- params$betas
    taus <- params$taus
    z <- params$z
    args <- list(...)
    
    clusterEvalQ(cl, {library(igraph); source('R/thread_generators.R')})
    clusterExport(cl, varlist= c("alphas", "betas", "taus", "z"), envir = environment())
    trees <- parLapply(cl, sizes, 
                       function(i){ 
                         gen.thread.Lumbreras2016(n=i, z=z, 
                                                  alphas=alphas, betas=betas, taus=taus, 
                                                  user.samples=args$user)})
  }
  
  stopCluster(cl)
  trees
}



tree_to_data <- function(g, thread=0){
  # Creates a dataframe with a row per post
  # and columns "degree of parent", "is_parent_root", "lag to parent", and "t"
  # With the model parameters, the three first columns are used to compute the numerator the likelihood
  # and 't' to compute the denominator of the likelihood
  if(vcount(g)==1) stop("Thread has length 1")
  parents <- get.edgelist(g, names=FALSE)[,2] # parents vector (the first parent is always the root)
  authors.all <- V(g)$user
  authors <- authors.all[-1] # remove root since it is not in parents vector
  parents.authors <- authors.all[parents]
  postid <- V(g)$name[-1]
  
  # (first choice is always the root, with starts with degree 1 by convention)
  if(length(parents)==1){
    popularities <- 1
  }else{
    popularities <- c(1, # the root when the first reply arrived
                      sapply(2:length(parents), function(t){
                        1 + sum(parents[1:(t-1)]==parents[t])
                      })
    )
  }
  
  # is the post a reply a <- b <- a ?
  grandparents <- sapply(seq_along(parents), function(t){
    if(parents[t]==1){ # replie to root have no grandparent
      FALSE
    }else{
      # you might need pen and paper to draw the vectors and see it
      authors[t]==authors.all[parents[parents[t]-1]]
    }
  })
  
  # a.k.a user indegree
  grandparents.candidates  <- sapply(seq_along(parents), function(t){
    sum(parents.authors[1:t] == authors[t])
  })
  
  posts <- 2:(length(parents)+1)
  data <- data.frame(thread = thread,
                     user = authors,
                     post = posts, # post order
                     postid = postid,
                     t = 1:(length(parents)), # post time slot
                     parent = parents, # post parent
                     popularity = popularities,
                     grandparent = grandparents,
                     grandparents.candidates = grandparents.candidates, # how many posts have had replies (number of grandparents to choose from)
                     stringsAsFactors=FALSE)
  
  data <- data %>% mutate(lag = t - parent + 1)
  
  # If there is a user integer identifier, use it
  # that is to place a user in a fixed and known row when doing EM computations
  if ('userint' %in% list.vertex.attributes(g)){
    data$userint <- V(g)$userint[-1]
  }
  data
}



#' Builds a dataframe from a list of trees where every row is a post and every column
#' is some post feature
#' @param trees a list of igraph trees
#' @details uses parallelization
#' @return a dataframe with one row per post
#' @export
trees_to_dataframe <- function(trees){
  
  # Create dataframes from trees
  ncores <- detectCores() - 2
  cl <- makeCluster(ncores, outfile="", port=11439)
  clusterEvalQ(cl, {
    library(igraph)
    library(dplyr)
  })
  clusterExport(cl, varlist=c("tree_to_data"))
  
  data.parlapply <- parLapply(cl, trees, tree_to_data)
  stopCluster(cl)
  
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
  df.trees
}
