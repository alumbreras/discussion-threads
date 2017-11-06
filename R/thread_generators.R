# Generates artificial threads with models of Gomez 2010 and Gomez 2013
# author: Alberto Lumbreras
################################
library(igraph)
library(dplyr)

gen.thread.Gomez2013 <- function(n=100, alpha=1, beta = 1, tau=0.75){
  # Generate synthetic thread according to Gomez 2011
  # Args:
  #    n: number of posts
  #    alpha:  preferential attachement exponent
  #    beta: root bias
  g <- graph.empty(n=1)

  # First post has no choice
  g <- add_vertices(g, 1)
  g <- add_edges(g, c(2,1))

  for (i in 3:n){
    betas <- c(beta, rep(0, i-2))
    lags <- (i-1):1
    popularities <- 1 + degree(g, mode="in") # even root starts with degree 1

    # Probability of choosing every node (only one is chosen)
    probs <- alpha*popularities[1:(i-1)] + betas + tau^lags
    probs <- probs/sum(probs)
    j <- sample(1:length(probs), 1, prob=probs)

    # Add new vertex attached to the chosen node
    g <- add_vertices(g, 1)
    g <- add_edges(g, c(i,j))
  }
  #V(g)$user <- 1 # for compatibility with Lumbreras
  g
}

# ' Here we need to pass the users order to check whether it is grandparent
# ' params users users vector that must include the root
gen.thread.Gomez2013plus <- function(users, alpha=1, beta = 1, tau=0.75, gamma=0.5){
  n <- length(users)
  g <- graph.empty(n=1)
  #V(g)[1]$user <- users[1]
  g <- set_vertex_attr(g, 'user', index = 1, users[1])
  
  
  # First post has no choice
  g <- add_vertices(g, 1, user=users[2]) # need to know the user root! :(
  g <- add_edges(g, c(2,1))
  
  
  popularities <- rep(1,n)
  popularities[1] <- 2 # root has the initial one plus the first reply
  
  parents.authors <- rep(NA, n)
  parents.authors[1] <- '-1' # root has no parent
  parents.authors[2] <- users[1] # second post is always reply to root
  
  for (i in 3:n){
    betas <- c(beta, rep(0, i-2))
    lags <- (i-1):1
    #popularities <- 1 + degree(g, mode="in") # even root starts with degree 1
    
    #replieds <- unlist(ego(g, 1, nodes=2:(i-1), mode='out', mindist=1))
    #grandparents <- c(FALSE, (V(g)[replieds]$user==users[i])) # add the root, who replied to no one
    grandparents <- (parents.authors[1:(i-1)] == users[i])
    
    # Probability of choosing every node (only one is chosen)
    probs <- alpha*popularities[1:(i-1)] + betas + gamma*grandparents + tau^lags
    probs <- probs/sum(probs)
    j <- sample(1:length(probs), 1, prob=probs)
    
    popularities[j] <- popularities[j] + 1
    parents.authors[i] <- users[j]
    
    # Add new vertex attached to the chosen node
    g <- add_vertices(g, 1, user=users[i])
    g <- add_edges(g, c(i,j))
  }
  g
}



#' Generates a tree given its posts authors
#' and the estimated parameters of the model (Lumbreras 2016)
#' @param params estimated model parameters
#' @param users sequence of users (equals to tree size)
#' @description Assumes that the identitity and cluster of users are known
gen.thread.Lumbreras2016 <- function(users, alphas, betas, taus){
  
  # add a fake root so that the index is easier to understand
  # i=1 -> root
  # i=2 -> first reply, etc
  #users <- c(NA, users) # removed since we include root in the users
  alphas <- c(NA, alphas)
  betas <- c(NA, betas)
  taus <- c(NA, taus)
  
  n <- length(users)
  if(n < 3) stop('Thread is too short')
  
  # First post has no choice
  g <- graph.empty(n=2)
  g <- add_edges(g, c(2,1))
  
  popularities <- rep(1,n)
  popularities[1] <- 2 # root has the initial one plus the first reply
  
  for (i in 3:n){
    
    # Get post parameters
    bs <- c(betas[i], rep(0, i-2))
    lags <- (i-1):1
    
    #popularities <- 1 + degree(g, mode="in") # even root starts with degree 1
    
    # Probability of choosing every node (only one is chosen)
    probs <- alphas[i]*popularities[1:(i-1)] + bs + taus[i]^lags
    probs <- probs/sum(probs)
    
    j <- sample(1:length(probs), 1, prob=probs)
    
    # update metrics t avoid using igraph (slower) for that
    popularities[j] <- popularities[j] + 1
    
    # Add new vertex attached to the chosen node
    g <- add_vertices(g, 1)
    g <- add_edges(g, c(i,j))
  }
  g
}


#' Generates a tree given its posts authors
#' and the estimated parameters of the model (Lumbreras 2016)
#' @param params estimated model parameters
#' @param users sequence of users (equals to tree size)
#' @description Assumes that the identitity and cluster of users are known
gen.thread.Lumbreras2016plus <- function(users, alphas, betas, taus, gammas){
  
  # add a fake root so that the index is easier to understand
  # i=1 -> root
  # i=2 -> first reply, etc
  #users <- c(NA, users) # removed since we include root in the users
  alphas <- c(NA, alphas)
  betas <- c(NA, betas)
  taus <- c(NA, taus)
  gammas <- c(NA, gammas)
  
  n <- length(users)
  if(n < 3) stop('Thread is too short')
  
  # First post has no choice
  #g <- graph.empty(n=2)
  #g <- add_edges(g, c(2,1))
  
  g <- graph.empty(n=1)
  #V(g)[1]$user <- users[1]
  g <- set_vertex_attr(g, 'user', index = 1, users[1])
  
  g <- add_vertices(g, 1, user=users[2]) # need to know the user root! :(
  g <- add_edges(g, c(2,1))
  
  popularities <- rep(1,n)
  popularities[1] <- 2 # root has the initial one plus the first reply
  
  parents.authors <- rep(NA, n)
  parents.authors[1] <- '-1' # root has no parent
  parents.authors[2] <- users[1] # second post is always reply to root
  
  for (i in 3:n){
    
    # Get post parameters
    bs <- c(betas[i], rep(0, i-2))
    lags <- (i-1):1
    #popularities <- 1 + degree(g, mode="in") # even root starts with degree 1
    
    #replieds <- unlist(ego(g, 1, nodes=2:(i-1), mode='out', mindist=1))
    #grandparents <- c(FALSE, (V(g)[replieds]$user==users[i])) # add the root, who replied to no one
    grandparents <- (parents.authors[1:(i-1)] == users[i])
    
    # Probability of choosing every node (only one is chosen)
    probs <- alphas[i]*popularities[1:(i-1)] + bs + gammas[i]*grandparents + taus[i]^lags
    probs <- probs/sum(probs)
    
    j <- sample(1:length(probs), 1, prob=probs)
    
    popularities[j] <- popularities[j] + 1
    parents.authors[i] <- users[j]
    
    # Add new vertex attached to the chosen node
    g <- add_vertices(g, 1, user=users[i])
    g <- add_edges(g, c(i,j))
  }
  g
}




#  Build a real graph tree from a vector of parents
# 'params parent parents vector 2:N
tree_from_parents_vector <- function(parents){
  size <- length(parents)+1
  g <- graph.empty(n=size)
  edges <- t(rbind(2:size, parents))
  graph_from_edgelist(edges)
}
  



gen.thread.Lumbreras2016_deprecated <- function(n=100, z=c(1,2,3), user.freqs=rep(1,length(z)),
                                     alphas = c(1,2,3), betas = c(1,2,3), taus = c(0.25, 0.5, 0.75),
                                     user.samples){
  # Generate threads like in Gomez 2013 but from a mixture model where users
  # in one component have different parameters (alpha, beta, tau)  than users in the
  # other component
  # then for each post chose a random user and then pick the parameters of its group
  # Args:
  #    n: number of posts
  #    alpha:  preferential attachement exponent
  #    beta: root bias
  nusers <- length(z)
  g <- graph.empty(n=1)

  # First post has no choice
  g <- add_vertices(g, 1)
  g <- add_edges(g, c(2,1))

  # the user of the two first posts is irrelevant for us
  V(g)$user <- 1

  for (i in 3:n){
    # choose random user according to distribution in real data
    u <- sample(user.samples, 1)
    cluster <- z[u]
    alpha <- alphas[cluster]
    beta <- betas[cluster]
    tau <- taus[cluster]

    bs <- c(beta, rep(0, i-2))
    lags <- (i-1):1
    popularities <- 1 + degree(g, mode="in") # even root starts with degree 1

    # Probability of choosing every node (only one is chosen)
    probs <- alpha*popularities + bs + tau^lags
    probs <- probs/sum(probs)

    j <- sample(1:length(probs), 1, prob=probs)

    # Add new vertex attached to the chosen node
    g <- add_vertices(g, 1, attr=list('user'= u))
    g <- add_edges(g, c(i,j))
  }
  g
}

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


# Generate a synthetic dataset of trees
#######################################
if(FALSE){
  n <- 100

  alpha.root <- 0.734
  alpha.c <- 0.683
  beta.root <- 1.302
  trees <- replicate(n,  gen.thread.Gomez2011(n=20, alpha.root=alpha.root, alpha.c=alpha.c, beta.root=beta.root), simplify = FALSE)

  alpha <- 1
  beta <- 0.68
  tau <- 0.75
  trees <- replicate(100, gen.thread.Gomez2013(n=100, alpha=alpha, beta=beta, tau=tau), simplify = FALSE)

  # Synthetic trees for Lumbreras 2016
  set.seed(1)
  alphas <- c(1,2,3)
  betas <- c(1,2,3)
  taus <- c(0.25, 0.5, 0.75)
  z <- c(1,2,3,1,2,3,1,2,3,1,2,3)
  trees <- replicate(1000,  gen.thread.Lumbreras2016(n=100, z=z, alphas=alphas, betas=betas, taus=taus), simplify = FALSE)

  # Very simple synthetic trees for Lumbreras 2016
  set.seed(1)
  alphas <- c(0.1,0.5)
  betas <- c(0.1,0.5)
  taus <- c(0.1, 0.5)
  z <- c(1,2)

  set.seed(1)
  alphas <- c(0.1, 0.5, 1)
  betas <- c(1, 5, 10)
  taus <- c(0.1, 0.5, 0.99)
  z <- do.call(c, lapply(1:length(alphas), function(x) rep(x,10000)))

  # the parallel way
  library(parallel)
  cl <- makeCluster(detectCores()-2)
  clusterEvalQ(cl, {library(igraph); source('thread_generator.r')})
  clusterExport(cl, c("alphas", "betas", "taus", "z"))
  trees <- parLapply(cl, 1:20000, function(i) gen.thread.Lumbreras2016(n=25, z=z, alphas=alphas, betas=betas, taus=taus) )
  stopCluster(cl)

  save(trees, file='trees.Rda')


  # Create dataframe from a set of trees
  ############################################################
  # Prepare all the information needed into a nice dataframe
  library(parallel)
  ncores <- detectCores() - 2
  cl<-makeCluster(ncores, outfile="", port=11439)
  clusterEvalQ(cl, {library(igraph); library(dplyr)})
  data.parlapply <- parLapply(cl, trees, tree.to.data)
  stopCluster(cl)

  # join results adding thread id
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

  save(df.trees, file='df.trees.Rda')

}

# Generate and plot some synthetic tree
########################################
TEST <- FALSE
if(TEST){
  # Values reported in Gomze 2010 (Table 2)
  # Slashdot:
  alpha.root <- 0.734
  alpha.c <- 0.683
  beta.root <- 1.302

  # Barrapunto
  alpha.root <- 0.665
  alpha.c <- -0.116
  beta.root <- 0.781

  # Meneame
  alpha.root <- 0.856
  alpha.c <- 0.196
  beta.root <- 1.588

  # Wikipedia
  alpha.root <- 0.884
  alpha.c <- -1.684
  beta.root <- 0.794

  trees <- replicate(4,  gen.thread.Gomez2011(n=100, alpha.root=alpha.root, alpha.c=alpha.c, beta.root=beta.root), simplify = FALSE)


  alpha <- 1
  beta <- 0.68
  tau <- 0.75
  trees <- replicate(4,  gen.thread.Gomez2013(n=100, alpha=alpha, beta=beta, tau=tau), simplify = FALSE)

  trees <- replicate(4,  gen.thread.Lumbreras2016(n=100), simplify = FALSE)

  par(mfrow=c(2,2))
  for (i in 1:length(trees)){
    g <- trees[[i]]
    root = which(degree(g, mode='out')==0)
    V(g)$color <- 'black'
    V(g)$color[root] <- 'red'
    V(g)$size <- 1
    V(g)$size[root] <- 3
    g.un <- as.undirected(g)
    la = layout_with_fr(g.un)
    plot(g.un, layout = la, vertex.label = NA, edge.arrow.size=0.6)
  }
}
