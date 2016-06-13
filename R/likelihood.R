# Compute likelihood of trees according to generative model and parameters
# author: Alberto Lumbreras
###########################################################################
library(dplyr)
library(data.table)
library(igraph)

#' Likelihood computation using the dataframe
#' @param row vector representing a post
#' @param alpha popularity parameter 
#' @param beta root bias
#' @param tau recency parameter
#' @return loglikelihood of the post
#' @export
likelihood_post <- function(row, alpha, beta, tau){
  (log(alpha * row['popularity'] + beta*(row['parent']==1) + tau^row['lag']) - 
     log(2*alpha*(row['t']-1/2)   + beta + tau*(tau^row['t']-1)/(tau-1)))
  # -1/2 because root has at least degree 1 (to follow Gomez 2013)
  # if the root starts with degree 0, then it should be:
  # log(2*alpha*(row['t']-1)   + beta + tau*(tau^row['t']-1)/(tau-1))
}


#' Total likelihood of a dataframe according to Gomez2013
#' @param df.trees data.frame with one post per row and features in columns
#' @param model parameters
#' @return loglikelihood of the dataset
#' @export
likelihood_Gomez2013 <- function(df.trees, params){
  sum(apply(df.trees[-2], 1, function(x) likelihood_post(x, params[1], params[2], params[3])))
}

#' The part of the lower bound that we optimize in the M-step
Qopt <- function(params, df.trees, responsabilities, pis, k){
  # E[lnp(X,Z|\theta)] likelihoods for clusters k and all users
  # given the current responsabilities
  # Note: pis do not affect the optimization. We include it so that the obtained value corresponds to 
  # the complete Q equation
  # This is similar to Bishop eq. 9.40, except that we loop over users, not over posts
  a <- responsabilities[,k][df.trees$userint]
  b <- apply(df.trees[-2], 1, function(x) likelihood_post(x, params[1], params[2], params[3]))
  log(pis[k])*sum(responsabilities[,k]) + sum(a*b)
}


#' Total likelihood of a dataframe according to Lumbreras2016
#' @param df.trees data.frame with one post per row and features in columns
#' @param model parameters
#' @param responsabilities responsabilities of users w.r.t clusters
#' @return loglikelihood of the dataset
#' @export
likelihood_Lumbreras2016 <- function(df.trees, params, responsabilities, pis){
  alphas <- params$alphas
  betas <- params$betas
  taus <- params$taus
  like <- 0
  K <- length(alphas)
  
  # Q (see Bishop Eq. 9.40, p.443)
  Q <- 0
  U <- length(unique(df.trees$userint))
  
  
  # The next loop does the same than this one but in a vectorized way
  #for(u in 1:U){
  #  Xu <- filter(df.trees, userint==u) # all posts from user
  #  for(k in 1:K){
  #    Q <- Q + responsabilities[u,k]*(log(pis[k]) + sum(apply(Xu[-2], 1, likelihood_post, alphas[k], betas[k], taus[k]))) 
  #  }
  #}
  for(k in 1:K){
    Q <- Q + Qopt(c(alphas[k], betas[k], taus[k]), df.trees, responsabilities, pis, k)
  }
  
  # Entropy of the posterior
  entropy <- -sum(responsabilities*log(responsabilities))

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
likelihood_Lumbreras2016.hard <- function(df.trees, params, responsabilities){
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
#' @param trees list of trees
#' @param alpha popularity parameter 
#' @param beta root bias
#' @param tau recency parameter
#' @return loglikelihood of the dataset
#' @export
likelihood.Gomez2013.deprecated <- function(trees, alpha=1, beta = 1, tau = 0.75){
  # Compute the likelihood of the whole set of trees
  # Arguments:
  #   trees: observed list of trees
  #   alpha.root, alpha.c, beta.root: parameters of the model
  like <- 0
  for (i in 1:length(trees)){
    g <- trees[[i]]
    parents <- get.edgelist(g)[,2] # parents vector
    
    # skip root and first post
    for(t in 2:length(parents)){
      b <- rep(0,t)
      b[1] <- beta
      lags <- t:1
      popularities <- 1 + tabulate(parents[1:(t-1)], nbins=t)
      popularities[1] <- popularities[1] - 1 # root has no parent 
      probs <- alpha*popularities + b + tau^lags
      probs <- probs/sum(probs)
      like <- like + log(probs[parents[t]])
    }
  }
  like
}
