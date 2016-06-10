source('R/likelihood.R')
library(dfoptim)

estimation_Gomez2013 <- function(df.trees, params=c(0.5,0.5,0.5)){
  Qopt <- function(params, df.trees){
    sum(apply(df.trees[-2], 1, function(i) likelihood_post(i, params[1], params[2], params[3])))
  }
  
  sol <- nmkb(params, Qopt, 
              lower = c(0,0,0), upper = c(Inf, Inf, 1), 
              control = list(maximize=TRUE),
              df.trees = df.trees)
  sol$par
}

#' Expectation-Maximization algorithm to find clusters and 
#' parameters of each cluster
#' @param df.trees dataframe of posts
#' @param params list of initial parameters
#' @return list of final parameters
estimation_Lumbreras2016 <- function(df.trees, params){
  
  update_responsabilities <- function(df.trees, u, pis, alphas, betas, taus){
    # Compute E(z_uk) over the posterior distribution p(z_uk | X, theta)
    
    K <- length(alphas) # number of clusters
    Xu <- filter(df.trees, user==u) # all posts from user
    
    logfactors <- rep(0,K)
    for (k in 1:K){
      logfactors[k] <- log(pis[k]) + sum(apply(Xu[-2], 1, likelihood_post, alphas[k], betas[k], taus[k]))
    }
    
    logfactors <- logfactors - max(logfactors) # avoid numerical underflow
    denominator <- sum(exp(logfactors))
    responsabilities_u <- exp(logfactors)/denominator
    responsabilities_u
  }
  
  Qopt <- function(params, resp_k, df.trees){
    # sum of E[lnp(X,Z|\theta)] likelihoods for all clusters and all users
    # given the current responsabilities
    # Note that the optimizations can be done separatedly
    a <- resp_k[df.trees$user]
    b <- apply(df.trees[-2], 1, function(x) likelihood_post(x, params[1], params[2], params[3]))
    sum(a*b) # each likelihood b is weighted according to how much dos the user belong to the cluster
  }

  alphas <- params$alphas
  betas <- params$betas
  taus <- params$taus
  
  K <- length(alphas)
  users <- unique(df.trees$user)
  U <- length(users)
  
  responsabilities <- matrix(nrow = U, ncol = K)
  pis <- rep(1/ncol(responsabilities), ncol(responsabilities))
  
  niters <- 2
  traces <- matrix(0, nrow=niters, ncol=K)
  for(iter in 1:niters){
    cat("\n**********ITERATION**********: ", iter, "\n")
    # EXPECTATION
    # Given the parameters of each cluster, find the responsability of each user in each cluster 
    #################################################################
    # (this can be parallelizable)
    for (u in 1:U){
      cat('\n u:', u)
      responsabilities[u,] <- update_responsabilities(df.trees, u, pis, alphas, betas, taus)  
    }
    cat("\nCluster distribution:\n", colSums(responsabilities))
    
    # MAXIMIZATION
    # Given the current responsabilities and pis, find the best parameters for each cluster
    ################################################################
    for(k in 1:K){
      # Optimization for cluster k
      # neldermead::fminbnd does not deal well with boundaries
      # nlminb and nmkb give the same solution.
      # nmkb is a little bit faster
      # sol <- nlminb(c(alphas[k],betas[k],taus[k]), cost.function,
      #              scale = 1, lower=c(0,0,0), upper=c(Inf, Inf, 1))
      sol <- nmkb(c(alphas[k], betas[k], taus[k]), Qopt, 
                  lower = c(0,0,0), upper = c(Inf, Inf, 1), 
                  control = list(maximize=TRUE),
                  resp_k = responsabilities[,k], df.trees = df.trees)
      alphas[k] <- sol$par[1]
      betas[k]  <-  sol$par[2]
      taus[k]   <-  sol$par[3]
      traces[iter,k] <- sol$value
    }
    
    # Update pis
    pis <- colSums(responsabilities)/nrow(responsabilities)
    
    ###############################################################
    # EVALUATION OF FULL LIKELIHOOD p(X, Z | \theta)
    # this should be monotonically increasing
    ###############################################################
    params$alphas <- alphas
    params$betas <- betas
    params$taus <- taus
    like <- likelihood_Lumbreras2016(df.trees, params, responsabilities)
      
    cat("\n\nalphas: ", alphas)
    cat("\nbetas: ", betas)
    cat("\ntaus: ", taus)
    cat("\n likelihood: ", like)
  }
  
  list(alphas=alphas,
       betas=betas,
       taus=taus,
       responsabilities=responsabilities,
       traces=traces)
  
}