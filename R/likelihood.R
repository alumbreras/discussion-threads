# Compute likelihood of trees according to generative model and parameters
# author: Alberto Lumbreras
###########################################################################
library(dplyr)
library(data.table)
library(igraph)
library(parallel)


#' Likelihood computation using the dataframe
#' @param row vector representing a post
#' @param alpha popularity parameter
#' @param beta root bias
#' @param tau recency parameter
#' @return loglikelihood of the post
#' @export
likelihood_post <- function(row, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  
  c(as.matrix(log(alpha * row['popularity'] + beta*(row['parent']==1) + tau^row['lag']) -
     log(2*alpha*(row['t']-1/2)   + beta + tau*(tau^row['t']-1)/(tau-1))))
  # -1/2 because root has at least degree 1 (to follow Gomez 2013)
  # if the root starts with degree 0, then it should be:
  # log(2*alpha*(row['t']-1)   + beta + tau*(tau^row['t']-1)/(tau-1))
}


#' Total likelihood of a dataframe according to Gomez2013
#' @param df.trees data.frame with one post per row and features in columns.
#' @param list of model parameters
#' @return loglikelihood of the dataset
#' @details df.tree must not have any non-numerical value since the internal apply
#' won't know how to deal with that
#' @export
likelihood_Gomez2013_deprecated <- function(df.trees, params){
  sum(apply(df.trees, 1, function(x) likelihood_post(x, params)))
}

# x100 times faster (for large dataframes). Shame on you, Alberto.
likelihood_Gomez2013 <- function(df.trees, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  sum(log(alpha*df.trees['popularity'] + beta*(df.trees['parent']==1) + tau^df.trees['lag']))-
  sum(log(2*alpha*(df.trees['t']-1/2)   + beta + tau*(tau^df.trees['t']-1)/(tau-1)))
}

# like Gomez 2013 but does not make the sum
#' @param params list of parameters
likelihood_Gomez2013_all <- function(df.trees, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  log(alpha*df.trees['popularity'] + beta*as.numeric(df.trees['parent']==1) + tau^df.trees['lag'])-
  log(2*alpha*(df.trees['t']-1/2)   + beta + tau*(tau^df.trees['t']-1)/(tau-1))
}


#' The part of the lower bound that we optimize in the M-step
#' @param params vector of initial parameters
Qopt <- function(params, df.trees, responsabilities, pis, k){
  # E[lnp(X,Z|\theta)] likelihoods for clusters k and all users
  # given the current responsabilities
  # Note: pis do not affect the optimization. We include it so that the obtained value corresponds to
  # the complete Q equation
  # This is similar to Bishop eq. 9.40, except that we loop over users, not over posts
  list.params <- list(alpha = params[1], beta = params[2], tau = params[3]) 
  df.trees <- filter(df.trees, t>1)
  a <- responsabilities[,k][df.trees$id_]
  b <- apply(df.trees, 1, function(x) likelihood_post(x, list.params))
  log(pis[k])*sum(responsabilities[,k]) + sum(a*b)
}

#'
#' @param params array of parameters
Qopt_opt <- function(params, df.trees, responsabilities, pis, k){
  # E[lnp(X,Z|\theta)] likelihoods for clusters k and all users
  # given the current responsabilities
  # Note: pis do not affect the optimization. We include it so that the obtained value corresponds to
  # the complete Q equation
  # This is similar to Bishop eq. 9.40, except that we loop over users, not over posts
  list.params <- list(alpha = params[1], beta = params[2], tau = params[3]) 
  a <- responsabilities[,k][df.trees$id_]
  b <- likelihood_Gomez2013_all(df.trees, list.params)
  log(pis[k])*sum(responsabilities[,k]) + sum(a*b)
}


#' The part of the lower bound that we optimize in the M-step
Qopt.par <- function(params, df.trees, responsabilities, pis, k){
  # E[lnp(X,Z|\theta)] likelihoods for clusters k and all users
  # given the current responsabilities
  # Note: pis do not affect the optimization. We include it so that the obtained value corresponds to
  # the complete Q equation
  # This is similar to Bishop eq. 9.40, except that we loop over users, not over posts
  list.params <- list(alpha = params[1], beta = params[2], tau = params[3]) 
  cl <- makeCluster(detectCores()-2)
  clusterExport(cl, c("likelihood_post", "params"))
  a <- responsabilities[,k][df.trees$userint]
  b <- parApply(cl, df.trees, 1, function(x) likelihood_post(x, list.params))
  stopCluster(cl)
  log(pis[k])*sum(responsabilities[,k]) + sum(a*b)
}


#' Total likelihood of a dataframe according to Lumbreras2016
#' @param df.trees data.frame with one post per row and features in columns
#' @param model parameters
#' @param responsabilities responsabilities of users w.r.t clusters
#' @return loglikelihood of the dataset
#' @export
likelihood_Lumbreras2016 <- function(df.trees, params, responsabilities, pis){
  #df.trees <- filter(!is.na(id_))
  alphas <- params$alpha
  betas <- params$beta
  taus <- params$tau
  like <- 0
  K <- length(alphas)

  # Q (see Bishop Eq. 9.40, p.443)
  Q <- 0
  U <- length(unique(df.trees$id_))


  # The next loop does the same than this one but in a vectorized way
  #for(u in 1:U){
  #  Xu <- filter(df.trees, userint==u) # all posts from user
  #  for(k in 1:K){
  #    Q <- Q + responsabilities[u,k]*(log(pis[k]) + sum(apply(Xu[-2], 1, likelihood_post, alphas[k], betas[k], taus[k])))
  #  }
  #}
  for(k in 1:K){
    Q <- Q + Qopt_opt(c(alphas[k], betas[k], taus[k]), df.trees, responsabilities, pis, k)
  }

  # Entropy of the posterior
  entropies <- -responsabilities*log(responsabilities)
  entropies[is.na(entropies)] <- 0 # covers cases of 0*log(0) (very rare)
  entropy <- sum(entropies)

  # Eq. 9.74, p.452
  like <- Q + entropy
  cat("\nQ: ", Q)
  cat("\nH: ", entropy)
  cat("\nTotal like: ", like, '\n')
  like
}


#'Total likelihood of a dataframe using hard assignments from the EM
#' @param df.trees data.frame with one post per row and features in columns
#' @param model parameters
#' @param responsabilities responsabilities of users w.r.t clusters
#' @return loglikelihood of the dataset
#' @export
likelihood_Lumbreras2016_hard <- function(df.trees, params, responsabilities){
  "This is wrong because a pi factor is missing (a priori probabilities)"
  alphas <- params$alphas
  betas <- params$betas
  taus <- params$taus
  z <- apply(responsabilities, 1, which.max)
  like <- 0
  for (i in 1:nrow(df.trees)){
    row <- df.trees[i,]
    k <- z[row$userint]
    like <- like + likelihood_post(df.trees[i, -2], alphas[k], betas[k], taus[k])
  }
  like
}


########################################################################################
########################################################################################
########################################################################################
########################################################################################
#' Deprecated b/c it uses igraph keep it just for testing
#' @param tree
#' @param alpha popularity parameter
#' @param beta root bias
#' @param tau recency parameter
#' @return loglikelihood of the dataset
#' @export
likelihood_Gomez2013_tree <- function(tree, params){
  # Compute the likelihood of the whole set of trees
  # Arguments:
  #   trees: observed list of trees
  #   alpha.root, alpha.c, beta.root: parameters of the model
  parents <- get.edgelist(tree, names=FALSE)[,2] # parents vector

  alpha <- params[1]
  beta <- params[2]
  tau <- params[3]
  
  like <- 0
  for(t in 2:length(parents)){
    b <- rep(0,t)
    b[1] <- beta
    lags <- t:1
    popularities <- 1 + tabulate(parents[1:(t-1)], nbins=t) # event root has ghost parent (to follow Gomez 2013)
    probs <- alpha*popularities + b + tau^lags
    probs <- probs/sum(probs)
    like <- like + log(probs[parents[t]])
  }
like
}

likelihood_Lumbreras2016_tree_hard <- function(tree, params, responsabilities){
  
  parents <- get.edgelist(tree, names=FALSE)[,2] 
  like <- 0 
  z <- apply(responsabilities, 1, which.max)
  
  for(t in 2:length(parents)){
    k <- z[V(tree)$userint[t+1]] # Attention! t+1, not t, since R first element (t=0) has position 1 in tree
    alpha <- params$alphas[k]
    beta <- params$betas[k]
    tau <- params$taus[k]
    
    b <- rep(0,t)
    b[1] <- beta
    lags <- t:1
    popularities <- 1 + tabulate(parents[1:(t-1)], nbins=t) # event root has ghost parent (to follow Gomez 2013)
    probs <- alpha*popularities + b + tau^lags
    probs <- probs/sum(probs)
    like <- like + log(probs[parents[t]])
    cat('\n', t, " ", V(tree)$userint[t+1], " ", log(probs[parents[t]]))
    
  }
  like
}
