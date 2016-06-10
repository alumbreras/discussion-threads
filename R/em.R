# Expectation-Maximizaton to fins clusters and alpha, beta, tau parameters 
# of each cluster
#
# author: Alberto Lumbreras
library(data.table)
library(neldermead)
library(dfoptim)

update_responsabilities <- function(df.trees, u, pis, alphas, betas, taus){
  # Compute E(z_uk) over the posterior distribution p(z_uk | X, theta)
  
  K <- length(alphas) # number of clusters
  Xu <- filter(df.trees, user==u) # all posts from user
  
  logfactors <- rep(0,K)
  for (k in 1:K){
    logfactors[k] <- log(pis[k]) + sum(apply(Xu, 1, likelihood.post, alphas[k], betas[k], taus[k]))
  }
  
  logfactors <- logfactors - max(logfactors) # avoid numerical underflow
  denominator <- sum(exp(logfactors))
  responsabilities_u <- exp(logfactors)/denominator
  responsabilities_u
}


# Likelihood computation using the dataframe
likelihood.post <- function(row, alpha, beta, tau){
  (log(alpha * row['popularity'] + beta*(row['parent']==1) + tau^row['lag']) - 
     log(2*alpha*(row['t']-1/2)   + beta + tau*(tau^row['t']-1)/(tau-1)))
}

Qopt <- function(params, resp_k, df.trees){
  # sum of E[lnp(X,Z|\theta)] likelihoods for all clusters and all users
  # given the current responsabilities
  # Note that the optimizations can be done separatedly
  a <- resp_k[df.trees$user]
  b <- apply(df.trees, 1, function(x) likelihood.post(x, params[1], params[2], params[3]))
  sum(a*b) # each likelihood b is weighted according to how much dos the user belong to the cluster
}

# Wrapper for the optimization function
# so that the only parameters are the ones to optimize
#cost.function <- function(params){
#  -1*Qopt(alpha=params[1], beta=params[2], tau=params[3], responsabilities[,k], df.trees)
#}

#Qopt.prima <- function(alpha, beta, tau, resp_k, df.trees){
#  sum(apply(df.trees, 1, likelihood.post, alpha, beta, tau))
#}


EM <-function(df.trees, alphas, betas, taus){
  # Expectation-Maximizationo to find groups of users with different
  # alpha, beta, tau parameters
  # Arguments:
  #   trees: a list of igraph objects representing discussion threads
  #   alphas, betas, taus: initial assignment of users to clusters
  K <- length(alphas)
  users <- unique(df.trees$user)
  U <- length(users)
  
  responsabilities <- matrix(nrow = U, ncol = K)
  pis <- rep(1/ncol(responsabilities), ncol(responsabilities))

  niters <- 10
  traces <- matrix(0, nrow=niters, ncol=K)
  for(iter in 1:niters){
      cat("\n**********ITERATION**********: ", iter, "\n")
      # EXPECTATION
      # Given the parameters of each cluster, find the responsability of each user in each cluster 
      #################################################################
      # (this can be parallelizable)
      for (u in 1:U){
        responsabilities[u,] <- update_responsabilities(df.trees, u, pis, alphas, betas, taus)  
      }
    
      #print(responsabilities)
      cat("Best vectors:\n", apply(responsabilities, 1, which.max))
    
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
      ###############################################################
      cat("\n\nalphas:", alphas)
      cat("\nbetas:", betas)
      cat("\ntaus:", taus)
  }
  
  list(alphas=alphas,
       betas=betas,
       taus=taus,
       z=z,
       traces=traces)
  
}

if(TRUE){
  #load('trees.Rda')
  
  # True parameters
  alphas <- c(0.1, 0.5, 1)
  betas <- c(1, 6, 3)
  taus <- c(0.7, 0.1, 0.2)

  # The first two posts of every thread are trivial since they have no choice
  df.trees <- filter(df.trees, t>2)
  
  # Expectation-Maximization
  res <- EM(df.trees, alphas, betas, taus)
  plot(rowSums(res$traces))
  
  #-7840
  #-7839
  #-7893
  
}