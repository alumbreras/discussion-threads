library(dfoptim)
library(parallel)
library(foreach)
library(doParallel)

#'
#'
#' @param df.tree dataframe or table with t, popularity (parent degree) and lag
estimation_Gomez2013_deprecated <- function(df.trees, params=list(alpha=0.5, beta=0.5, tau=0.5)){
  
  Qopt <- function(params, df.trees){
    params <- list(alpha = params[1], beta=params[2], tau=params[3])
    sum(apply(df.trees, 1, function(i) likelihood_post(i, params)))
  }
  
  sol <- nmkb(unlist(params), Qopt,
              lower = c(0,0,0), upper = c(Inf, Inf, 1),
              control = list(maximize=TRUE),
              df.trees = df.trees)
  sol$par
}

estimation_Gomez2013 <- function(df.trees, params=list(alpha=0.5, beta=0.5, tau=0.5)){
  
  df.trees <- df.trees %>% select(t, popularity, parent, lag)
  # Remove t=1 to avoid strange things like NA
  # stop if some user is lost in the process
  # (users that only have replies to root)
  users.before <- length(unique(df.trees$user))
  df.trees <- df.trees %>% filter(t>1)
  users.after <- length(unique(df.trees$user))
  if(users.before != users.after) warning("Remove users that only reply to root")
  
  Qopt <- function(params, df.trees){
    params <- list(alpha = params[1], beta=params[2], tau=params[3])
    likelihood_Gomez2013(df.trees, params)
  }
  
  sol <- nmkb(unlist(params), Qopt,
              lower = c(0,0,0), upper = c(Inf, Inf, 1),
              control = list(maximize=TRUE),
              df.trees = df.trees)
  
  list(alpha = sol$par[1],
       beta = sol$par[2],
       tau = sol$par[3],
       likelihood = sol$value)
}

estimation_Gomez2013plus <- function(df.trees, params=list(alpha=0.5, beta=0.5, tau=0.5, gamma=0.5)){
  
  df.trees <- df.trees %>% 
    select(t, popularity, parent, lag, grandparent, grandparents.candidates)
  
  # Remove t=1 to avoid strange things like NA
  # stop if some user is lost in the process
  # (users that only have replies to root)
  users.before <- length(unique(df.trees$user))
  df.trees <- df.trees %>% filter(t>1)
  users.after <- length(unique(df.trees$user))
  if(users.before != users.after) warning("Remove users that only reply to root")
  
  Qopt <- function(params, df.trees){
    params <- list(alpha = params[1], beta=params[2], tau=params[3], gamma = params[4])
    likelihood_Gomez2013plus(df.trees, params)
  }
  
  sol <- nmkb(unlist(params), Qopt,
              lower = c(0,0,0,0), upper = c(Inf, Inf, 1, Inf),
              control = list(maximize=TRUE),
              df.trees = df.trees)
  
  list(alpha = sol$par[1],
       beta = sol$par[2],
       tau = sol$par[3],
       gamma = sol$par[4],
       likelihood = sol$value)
}


#' Expectation-Maximization algorithm to find clusters and
#' parameters of each cluster
#' @param data df.trees dataframe of posts
#' @param params list of initial parameters
#' @return list of final parameters
#' EM Implementations 
# https://github.com/scikit-learn/scikit-learn/blob/51a765a/sklearn/mixture/gmm.py#L115
estimation_Lumbreras2016 <- function(data, params, niters=10){
  stopifnot(all(params$taus<1))
  stopifnot(all(params$alphas > 0))
  stopifnot(all(params$betas > 0))
  stopifnot(all(params$taus > 0))
  
  # Copy of the original data with modifications
  data_ <- data
  
  # Remove t=1 to avoid strange things like NA
  # stop if some user is lost in the process
  # (users that only have replies to root)
  users.before <- length(unique(data$user))
  data <- data %>% filter(t>1)
  users.after <- length(unique(data$user))
  if(users.before != users.after) warning("Remove users that only reply to root")
  
  
  # Set up cluster for parallelisation
  ncores <- detectCores() - 2
  cl <- makeCluster(ncores, outfile="", port=11439)
  registerDoParallel(cl)
  
  
  # Internal function to update responsibilities of users
  # Computes E(z_uk) over the posterior distribution p(z_uk | X, theta)
  update_responsibilities <- function(data, u, pis, alphas, betas, taus){
    
    K <- length(alphas) # number of clusters
    Xu <- filter(data, id_==u) # all posts from user
    
    logfactors <- rep(0,K)
    for (k in 1:K){
      params.k <- list(alpha = alphas[k], beta = betas[k], tau = taus[k])
      logfactors[k] <- log(pis[k]) + likelihood_Gomez2013(Xu, params.k)
      #logfactors[k] <- log(pis[k]) + sum(apply(Xu, 1, likelihood_post, params.k))
    }
    
    logfactors <- logfactors - max(logfactors) # avoid numerical underflow
    responsibilities_u <- exp(logfactors)/sum(exp(logfactors))
    responsibilities_u
  }
  
  # Create internal user ids. They will correspond to their row
  # in the matrix of responsibilities
  # We will give their real ids back at the end
  user.realids <- unique(data$user)
  data$id_ <- match(data$user, unique(data$user))
  data <- data %>% select(id_, t, popularity, parent, lag)
  
  U <- length(unique(data$id_))
  userids <- sort(unique(data$id_))
  alphas <- params$alpha
  betas <- params$beta
  taus <- params$tau
  
  K <- length(alphas)
  
  responsibilities <- matrix(1/K, nrow = U, ncol = K)
  pis <- rep(1/ncol(responsibilities), ncol(responsibilities))
  
  likelihoods.complete <- rep(NA, niters)
  likelihoods.Q <- rep(NA, niters)
  
  like.last <- -Inf
  iter <- 1
  
  for(iter in 1:niters){
    
    cat("\n**********ITERATION**********: ", iter, "\n")
    
    # EXPECTATION
    # Given the parameters of each cluster, 
    #find the responsability of each user in each cluster
    #################################################################
    cat("\nExpectation...")
    responsibilities <- foreach(u=userids, .packages=c('dplyr'), 
                                .export=c('likelihood_Gomez2013'), 
                                .combine=rbind) %dopar% 
                                {
                                  update_responsibilities(data, u, pis, alphas, betas, taus)
                                }
    
    cat("\nCluster distribution (1:5):\n", head(colSums(responsibilities)),5)
    
    # MAXIMIZATION
    # Given the current responsibilities and pis, find the best parameters for each cluster
    ################################################################
    cat("\nMaximization...")
    # Parallel optimization for cluster k=1,...,K
    # neldermead::fminbnd does not deal well with boundaries
    # nlminb and nmkb give the same solution.
    # nmkb is a little bit faster
    # sol <- nlminb(c(alphas[k],betas[k],taus[k]), cost.function,
    #              scale = 1, lower=c(0,0,0), upper=c(Inf, Inf, 1))
    # TODO: we might try starting from somewhere else in the parameter space
    # just in case. And discard worse solutions.
    sols <- foreach(k=1:K, .packages=c('dfoptim'), 
                    .export=c('likelihood_post', 'likelihood_Gomez2013_all',
                              'Qopt_opt')) %dopar% {
                                nmkb(c(alphas[k], betas[k], taus[k]), Qopt_opt, 
                                     lower = c(0,0,0), upper = c(Inf, Inf, 1), 
                                     control = list(maximize=TRUE),
                                     data = data, responsibilities = responsibilities, pis = pis, k=k)
                              }
    
    
    # Store updated parameters
    alphas <- unlist(lapply(sols, function(x) x$par[1]))
    betas  <- unlist(lapply(sols, function(x) x$par[2]))
    taus   <- unlist(lapply(sols, function(x) ifelse(x$par[3]==0, 1e-10, x$par[3])))
    params$alphas <-alphas
    params$betas <- betas
    params$taus <- taus
    
    cat("\n\nalphas (1:5): ", head(alphas,5))
    cat("\nbetas (1:5) : ", head(betas,5))
    cat("\ntaus (1:5):", head(taus,5))
    
    ###############################################################
    # EVALUATION OF FULL LIKELIHOOD p(X, Z | \theta)
    # this should be monotonically increasing
    ###############################################################
    
    # Q (sum of component Q's). Should be monotonically increasing
    likelihoods.Q[iter] <- sum(unlist(lapply(sols, function(x) x$value)))
    
    # entropy
    #H <- sum(apply(responsibilities, 1, function(x) sum(x*log(x), na.rm=TRUE)), na.rm=TRUE)
    H <- sum(responsibilities * log(responsibilities), na.rm=TRUE)
    
    # total lower bound L
    like <- likelihoods.Q[iter] - H
    
    #like <- likelihood_Lumbreras2016(data_, params, responsibilities, pis)
    #likes[iter] <- c(t(pis) %*% traces[iter,])
    likelihoods.complete[iter] <- like # Q + H
    
    cat('\nCurrent likelihoods')
    cat('\nQ:', likelihoods.Q[iter])
    cat('\nK:', H)
    cat('\nL:', like )
    
    
    if(like < like.last) stop("Decreasing likelihood!")
    
    if(like == like.last){
      cat('\n\nConverged!')
      break
    }else{
      like.last <- like
    } 
    
    # Update pis
    pis <- colSums(responsibilities)/nrow(responsibilities)
  }
  
  stopCluster(cl)
  
  # real user ids into the responsability matrix
  rownames(responsibilities) <- user.realids
  
  list(alphas=alphas,
       betas=betas,
       taus=taus,
       responsibilities=responsibilities,
       pis = pis,
       likelihoods = likelihoods.complete,
       likelihoods.Q = likelihoods.Q)
}



#' Expectation-Maximization algorithm to find clusters and
#' parameters of each cluster
#' @param data df.trees dataframe of posts
#' @param params list of initial parameters
#' @return list of final parameters
#' # TODO: deal with cases of empty clusters or 1 user, stranger things happens...
estimation_Lumbreras2016plus <- function(data, params, niters=10){
  stopifnot(all(params$taus<1))
  stopifnot(all(params$alphas > 0))
  stopifnot(all(params$betas > 0))
  stopifnot(all(params$gamma > 0))
  
  # Copy of the original data with modifications
  data_ <- data
  
  # Remove t=1 to avoid strange things like NA
  # stop if some user is lost in the process
  # (users that only have replies to root)
  users.before <- length(unique(data$user))
  data <- data %>% filter(t > 1)
  users.after <- length(unique(data$user))
  if(users.before != users.after) warning("Remove users that only reply to root")
  
  
  # Set up cluster for parallelisation
  ncores <- detectCores() - 2
  cl <- makeCluster(ncores, outfile="", port=11439)
  registerDoParallel(cl)
  
  
  
  update_responsibilities <- function(data, u, pis, alphas, betas, taus, gammas){
    # Compute E(z_uk) over the posterior distribution p(z_uk | X, theta)
    
    K <- length(alphas) # number of clusters
    Xu <- filter(data, id_==u) # all posts from user
    
    logfactors <- rep(0,K)
    for (k in 1:K){
      params.k <- list(alpha = alphas[k], beta = betas[k], tau = taus[k], gamma = gammas[k])
      logfactors[k] <- log(pis[k]) + likelihood_Gomez2013(Xu, params.k)
      #logfactors[k] <- log(pis[k]) + sum(apply(Xu, 1, likelihood_post, params.k))
    }
    
    logfactors <- logfactors - max(logfactors) # avoid numerical underflow
    responsibilities_u <- exp(logfactors)/sum(exp(logfactors))
    responsibilities_u
  }
  
  # Create internal user ids. They will correspond to their row
  # in the matrix of responsibilities
  # We will give their real ids back at the end
  user.realids <- unique(data$user)
  data$id_ <- match(data$user, unique(data$user))
  data <- data %>% select(id_, t, popularity, parent, lag, grandparent, grandparents.candidates)
  
  U <- length(unique(data$id_))
  userids <- sort(unique(data$id_))
  alphas <- params$alpha
  betas <- params$beta
  taus <- params$tau
  gammas <- params$gamma
  
  K <- length(alphas)
  
  responsibilities <- matrix(1/K, nrow = U, ncol = K)
  pis <- rep(1/ncol(responsibilities), ncol(responsibilities))
  
  traces <- matrix(0, nrow=niters, ncol=K)
  likes <- rep(NA, niters)
  like.last <- -Inf
  iter <- 1
  for(iter in 1:niters){
    cat("\n**********ITERATION**********: ", iter, "\n")
    # EXPECTATION
    # Given the parameters of each cluster, find the responsability of each user in each cluster
    #################################################################
    cat("\nExpectation...")
    responsibilities <- foreach(u=userids, .packages=c('dplyr'), 
                                .export=c('likelihood_Gomez2013'), 
                                .combine=rbind) %dopar% 
                                {
                                  update_responsibilities(data, u, pis, alphas, betas, taus, gammas)
                                }
    
    cat("\nCluster distribution:\n", colSums(responsibilities))
    
    # MAXIMIZATION
    # Given the current responsibilities and pis, find the best parameters for each cluster
    ################################################################
    cat("\nMaximization...")
    # Parallel optimization for cluster k=1,...,K
    # neldermead::fminbnd does not deal well with boundaries
    # nlminb and nmkb give the same solution.
    # nmkb is a little bit faster
    # sol <- nlminb(c(alphas[k],betas[k],taus[k]), cost.function,
    #              scale = 1, lower=c(0,0,0), upper=c(Inf, Inf, 1))
    sols <- foreach(k=1:K, .packages=c('dfoptim'), 
                    .export=c('likelihood_post_plus', 'likelihood_Gomez2013_all_plus',
                              'Qopt_opt_plus')) %dopar% {
                                nmkb(c(alphas[k], betas[k], taus[k], gammas[k]), Qopt_opt_plus, 
                                     lower = c(0,0,0,0), upper = c(Inf, Inf, 1, Inf), 
                                     control = list(maximize=TRUE),
                                     data = data, responsibilities = responsibilities, pis = pis, k=k)
                              }
    
    for(k in 1:K){
      sol <- sols[[k]]
      alphas[k] <- sol$par[1]
      betas[k]  <-  sol$par[2]
      taus[k]   <-  ifelse(sol$par[3]==0, 1e-10, sol$par[3]) 
      gammas[k]   <-  sol$par[4]
      traces[iter,k] <- sol$value
    }
    params$alpha <- alphas
    params$beta <- betas
    params$tau <- taus
    params$gamma <- gammas
    
    
    ###############################################################
    # EVALUATION OF FULL LIKELIHOOD p(X, Z | \theta)
    # this should be monotonically increasing
    ###############################################################
    rownames(responsibilities) <- user.realids
    like <- likelihood_Lumbreras2016(data_, params, responsibilities, pis)
    likes[iter] <- c(t(pis) %*% traces[iter,])
    cat("\n\nalphas: ", alphas)
    cat("\nbetas: ", betas)
    cat("\ntaus: ", taus)
    cat("\ngammas: ", gammas)
    cat("\n likelihood: ", like)
    cat("\n***")
    
    if(like == like.last){
      cat('\n\nConverged!')
      break
    }else{
      like.last <- like
    } 
    
    # Update pis
    pis <- colSums(responsibilities)/nrow(responsibilities)
  }
  
  stopCluster(cl)
  
  # real user ids into the responsability matrix
  rownames(responsibilities) <- user.realids
  
  list(alphas=alphas,
       betas=betas,
       taus=taus,
       gammas=gammas,
       responsibilities=responsibilities,
       pis = pis,
       traces=traces,
       likes=like)
}


