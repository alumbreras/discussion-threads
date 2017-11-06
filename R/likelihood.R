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
<<<<<<< HEAD
     #log(2*alpha*(row['t']-1/2)   + beta + tau*(tau^row['t']-1)/(tau-1))))
     log(2*alpha*(row['t']-1)   + beta + tau*(tau^row['t']-1)/(tau-1))))
  # FIXED March 2017: it is -1, not -1/2
=======
     log(2*alpha*(row['t']-1/2)   + beta + tau*(tau^row['t']-1)/(tau-1))))
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  # -1/2 because root has at least degree 1 (to follow Gomez 2013)
  # if the root starts with degree 0, then it should be:
  # log(2*alpha*(row['t']-1)   + beta + tau*(tau^row['t']-1)/(tau-1))
}

#  For Gomezplus
likelihood_post_plus <- function(row, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  gamma <- params$gamma
  
  c(as.matrix(log(alpha * row['popularity'] + beta*(row['parent']==1) + gamma*(row['granparent']) + tau^row['lag']) -
<<<<<<< HEAD
                #log(2*alpha*(row['t']-1/2)   + beta  + gamma*row['grandparents.candidates'] + tau*(tau^row['t']-1)/(tau-1))))
                log(2*alpha*(row['t']-1)   + beta  + gamma*row['grandparents.candidates'] + tau*(tau^row['t']-1)/(tau-1))))

=======
                log(2*alpha*(row['t']-1/2)   + beta  + gamma*row['grandparents.candidates'] + tau*(tau^row['t']-1)/(tau-1))))
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  # -1/2 because root has at least degree 1 (to follow Gomez 2013)
  # if the root starts with degree 0, then it should be:
  # log(2*alpha*(row['t']-1)   + beta + tau*(tau^row['t']-1)/(tau-1))
}

#' Total likelihood of a dataframe according to Gomez2013
#' @param data data.frame with one post per row and features in columns.
#' @param list of model parameters
#' @return loglikelihood of the dataset
#' @details df.tree must not have any non-numerical value since the internal apply
#' won't know how to deal with that
#' @export
likelihood_Gomez2013_deprecated <- function(data, params){
  sum(apply(data, 1, function(x) likelihood_post(x, params)))
<<<<<<< HEAD
}

# x100 times faster (for large dataframes)
likelihood_Gomez2013 <- function(data, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  sum(log(alpha*data['popularity'] + beta*(data['parent']==1) + tau^data['lag']))-
  #sum(log(2*alpha*(data['t']-1/2)   + beta + tau*(tau^data['t']-1)/(tau-1)))
  sum(log(2*alpha*(data['t']-1)   + beta + tau*(tau^data['t']-1)/(tau-1)))
  
}

# x100 times faster (for large dataframes)
likelihood_Gomez2013plus <- function(data, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  gamma <- params$gamma
  sum(log(alpha*data['popularity'] + beta*(data['parent']==1) + gamma*data['grandparent'] + tau^data['lag']))-
    #sum(log(2*alpha*(data['t']-1/2)   + beta + gamma*data['grandparents.candidates'] + tau*(tau^data['t']-1)/(tau-1)))
    sum(log(2*alpha*(data['t']-1)   + beta + gamma*data['grandparents.candidates'] + tau*(tau^data['t']-1)/(tau-1)))
  
}

# like Gomez 2013 but does not make the sum
#' Needed during the EM for matrix computations
#' @param params list of parameters
likelihood_Gomez2013_all <- function(data, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  log(alpha*data['popularity'] + beta*as.numeric(data['parent']==1) + tau^data['lag'])-
  #log(2*alpha*(data['t']-1/2)   + beta + tau*(tau^data['t']-1)/(tau-1))
  log(2*alpha*(data['t']-1)   + beta + tau*(tau^data['t']-1)/(tau-1))
  
}

likelihood_Gomez2013_all_plus <- function(data, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  gamma <- params$gamma
  log(alpha*data['popularity'] + beta*as.numeric(data['parent']==1) + gamma*data['grandparent'] + tau^data['lag'])-
    #log(2*alpha*(data['t']-1/2)   + beta + gamma*data['grandparents.candidates'] + tau*(tau^data['t']-1)/(tau-1))
    log(2*alpha*(data['t']-1)   + beta + gamma*data['grandparents.candidates'] + tau*(tau^data['t']-1)/(tau-1))
  
}

=======
}

# x100 times faster (for large dataframes)
likelihood_Gomez2013 <- function(data, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  sum(log(alpha*data['popularity'] + beta*(data['parent']==1) + tau^data['lag']))-
  sum(log(2*alpha*(data['t']-1/2)   + beta + tau*(tau^data['t']-1)/(tau-1)))
}

# x100 times faster (for large dataframes)
likelihood_Gomez2013plus <- function(data, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  gamma <- params$gamma
  sum(log(alpha*data['popularity'] + beta*(data['parent']==1) + gamma*data['grandparent'] + tau^data['lag']))-
    sum(log(2*alpha*(data['t']-1/2)   + beta + gamma*data['grandparents.candidates'] + tau*(tau^data['t']-1)/(tau-1)))
}

# like Gomez 2013 but does not make the sum
#' Needed during the EM for matrix computations
#' @param params list of parameters
likelihood_Gomez2013_all <- function(data, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  log(alpha*data['popularity'] + beta*as.numeric(data['parent']==1) + tau^data['lag'])-
  log(2*alpha*(data['t']-1/2)   + beta + tau*(tau^data['t']-1)/(tau-1))
}

likelihood_Gomez2013_all_plus <- function(data, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- params$tau
  gamma <- params$gamma
  log(alpha*data['popularity'] + beta*as.numeric(data['parent']==1) + gamma*data['grandparent'] + tau^data['lag'])-
    log(2*alpha*(data['t']-1/2)   + beta + gamma*data['grandparents.candidates'] + tau*(tau^data['t']-1)/(tau-1))
}

>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad

#' The part of the lower bound that we optimize in the M-step
#' @param params vector of initial parameters
Qopt <- function(params, data, responsibilities, pis, k){
  # E[lnp(X,Z|\theta)] likelihoods for clusters k and all users
  # given the current responsibilities
  # Note: pis do not affect the optimization. We include it so that the obtained value corresponds to
  # the complete Q equation
  # This is similar to Bishop eq. 9.40, except that we loop over users, not over posts
  list.params <- list(alpha = params[1], beta = params[2], tau = params[3]) 
  data <- filter(data, t>1)
  a <- responsibilities[,k][data$id_]
  b <- apply(data, 1, function(x) likelihood_post(x, list.params))
  log(pis[k])*sum(responsibilities[,k]) + sum(a*b)
}

#'
#' @param params array of parameters
Qopt_opt <- function(params, data, responsibilities, pis, k){
  # E[lnp(X,Z|\theta)] likelihoods for clusters k and all users
  # given the current responsibilities
  # Note: pis do not affect the optimization. We include it so that the obtained value corresponds to
  # the complete Q equation
  # This is similar to Bishop eq. 9.40, except that we loop over users, not over posts
  list.params <- list(alpha = params[1], beta = params[2], tau = params[3]) 
  a <- responsibilities[,k][data$id_]
  b <- likelihood_Gomez2013_all(data, list.params)
  log(pis[k])*sum(responsibilities[,k]) + sum(a*b)
}

Qopt_opt_plus <- function(params, data, responsibilities, pis, k){
  # E[lnp(X,Z|\theta)] likelihoods for clusters k and all users
  # given the current responsibilities
  # Note: pis do not affect the optimization. We include it so that the obtained value corresponds to
  # the complete Q equation
  # This is similar to Bishop eq. 9.40, except that we loop over users, not over posts
  list.params <- list(alpha = params[1], beta = params[2], tau = params[3], gamma = params[4]) 
  a <- responsibilities[,k][data$id_]
  b <- likelihood_Gomez2013_all_plus(data, list.params)
  log(pis[k])*sum(responsibilities[,k]) + sum(a*b)
}



#' The part of the lower bound that we optimize in the M-step
Qopt.par <- function(params, data, responsibilities, pis, k){
  # E[lnp(X,Z|\theta)] likelihoods for clusters k and all users
  # given the current responsibilities
  # Note: pis do not affect the optimization. We include it so that the obtained value corresponds to
  # the complete Q equation
  # This is similar to Bishop eq. 9.40, except that we loop over users, not over posts
  list.params <- list(alpha = params[1], beta = params[2], tau = params[3]) 
  cl <- makeCluster(detectCores()-2)
  clusterExport(cl, c("likelihood_post", "params"))
  a <- responsibilities[,k][data$userint]
  b <- parApply(cl, data, 1, function(x) likelihood_post(x, list.params))
  stopCluster(cl)
  log(pis[k])*sum(responsibilities[,k]) + sum(a*b)
}


#' Total likelihood of a dataframe according to Lumbreras2016
#' @param data trees data.frame with one post per row and features in columns
#' @param model parameters
#' @param responsibilities responsibilities of users w.r.t clusters
#' @return loglikelihood of the dataset
#' @export
likelihood_Lumbreras2016 <- function(data, params, responsibilities, pis){

  if(! "user" %in% names(data)) stop("Missing column in data: user")
  
  alphas <- params$alpha
  betas <- params$beta
  taus <- params$tau
  like <- 0
  K <- length(alphas)

  # The internal id of a user is its row in the matrix of responsibilities
  user.realids <- unique(data$user)
  data$id_ <- match(data$user, rownames(responsibilities))
  data <- data %>% select(id_, t, popularity, parent, lag)
  
  # Check all users have a responsability entry
  if (!all(user.realids %in% rownames(responsibilities))){
    stop ('Some input users are not in the responsibility matrix')
  }
  
  
  # Q (see Bishop Eq. 9.40, p.443)

  # The next loop does the same than this one but in a vectorized way
  # Q <- 0
  # U <- length(unique(data$id_))
  #for(u in 1:U){
  #  Xu <- filter(data, userint==u) # all posts from user
  #  for(k in 1:K){
  #    Q <- Q + responsibilities[u,k]*(log(pis[k]) + sum(apply(Xu[-2], 1, likelihood_post, alphas[k], betas[k], taus[k])))
  #  }
  #}
  
  Q <- 0
  for(k in 1:K){
    Q <- Q + Qopt_opt(c(alphas[k], betas[k], taus[k]), data, responsibilities, pis, k)
    cat("Q, k, ", Q, k)
  }

  # Entropy of the posterior
  #entropies <- -responsibilities*log(responsibilities)
  #entropies[is.na(entropies)] <- 0 # covers cases of 0*log(0) (very rare)
  #entropy <- sum(entropies)
  entropy <- - sum(responsibilities * log(responsibilities), na.rm=TRUE)

  # Eq. 9.74, p.452
  like <- Q + entropy
  cat("\nQ: ", Q)
  cat("\nH: ", entropy)
  cat("\nTotal like: ", like, '\n')
  like
}


likelihood_Lumbreras2016plus <- function(data, params, responsibilities, pis){
  
  if(! "user" %in% names(data)) stop("Missing column in data: user")
  
  alphas <- params$alpha
  betas <- params$beta
  taus <- params$tau
  gammas <- params$gamma
  like <- 0
  K <- length(alphas)
  
  # The internal id of a user is its row in the matrix of responsibilities
  user.realids <- unique(data$user)
  data$id_ <- match(data$user, rownames(responsibilities))
  data <- data %>% select(id_, t, popularity, parent, lag,  grandparent, grandparents.candidates)
  
  # Check all users have a responsability entry
  if (!all(user.realids %in% rownames(responsibilities))){
    stop ('Some input users are not in the responsibility matrix')
  }
  
  
  # Q (see Bishop Eq. 9.40, p.443)
  
  # The next loop does the same than this one but in a vectorized way
  # Q <- 0
  # U <- length(unique(data$id_))
  #for(u in 1:U){
  #  Xu <- filter(data, userint==u) # all posts from user
  #  for(k in 1:K){
  #    Q <- Q + responsibilities[u,k]*(log(pis[k]) + sum(apply(Xu[-2], 1, likelihood_post, alphas[k], betas[k], taus[k])))
  #  }
  #}
  
  Q <- 0
  for(k in 1:K){
    Q <- Q + Qopt_opt_plus(c(alphas[k], betas[k], taus[k], gammas[k]), data, responsibilities, pis, k)
    cat("Q, k, ", Q, k)
  }
  
  # Entropy of the posterior
  entropies <- -responsibilities*log(responsibilities)
  entropies[is.na(entropies)] <- 0 # covers cases of 0*log(0) (very rare)
  entropy <- sum(entropies)
  
  # Eq. 9.74, p.452
  like <- Q + entropy
  cat("\nQ: ", Q)
  cat("\nH: ", entropy)
  cat("\nTotal like: ", like, '\n')
  like
}

# DEPRECATED
#'Total likelihood of a dataframe using hard assignments from the EM
#' @param data data.frame with one post per row and features in columns
#' @param model parameters
#' @param responsibilities responsibilities of users w.r.t clusters
#' @return loglikelihood of the dataset
#' @export
likelihood_Lumbreras2016_hard <- function(data, params, responsibilities){
  "This is wrong because a pi factor is missing (a priori probabilities)"
  alphas <- params$alphas
  betas <- params$betas
  taus <- params$taus
  z <- apply(responsibilities, 1, which.max)
  like <- 0
  for (i in 1:nrow(data)){
    row <- data[i,]
    k <- z[row$userint]
    like <- like + likelihood_post(data[i], alphas[k], betas[k], taus[k])
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

likelihood_Lumbreras2016_tree_hard <- function(tree, params, responsibilities){
  
  parents <- get.edgelist(tree, names=FALSE)[,2] 
  like <- 0 
  z <- apply(responsibilities, 1, which.max)
  
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
