library(dfoptim)
library(parallel)
library(foreach)
library(doParallel)

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
estimation_Lumbreras2016 <- function(df.trees, params, niters=10){
  stopifnot(all(params$taus<1))
  stopifnot(all(params$alphas > 0))
  stopifnot(all(params$betas > 0))
  stopifnot(all(params$taus > 0))

  # Set up cluster for parallelisation
  ncores <- detectCores() - 2
  cl <- makeCluster(ncores, outfile="", port=11439)
  registerDoParallel(cl)

  update_responsabilities <- function(df.trees, u, pis, alphas, betas, taus){
    # Compute E(z_uk) over the posterior distribution p(z_uk | X, theta)

    K <- length(alphas) # number of clusters
    Xu <- filter(df.trees, userint==u) # all posts from user

    logfactors <- rep(0,K)
    for (k in 1:K){
      logfactors[k] <- log(pis[k]) + sum(apply(Xu[-2], 1, likelihood_post, alphas[k], betas[k], taus[k]))
    }

    logfactors <- logfactors - max(logfactors) # avoid numerical underflow
    responsabilities_u <- exp(logfactors)/sum(exp(logfactors))
    responsabilities_u
  }



  U <- length(unique(df.trees$userint))
  alphas <- params$alphas
  betas <- params$betas
  taus <- params$taus

  K <- length(alphas)

  responsabilities <- matrix(1/K, nrow = U, ncol = K)
  pis <- rep(1/ncol(responsabilities), ncol(responsabilities))

  traces <- matrix(0, nrow=niters, ncol=K)
  likes <- rep(NA, niters)
  for(iter in 1:niters){
    cat("\n**********ITERATION**********: ", iter, "\n")
    # EXPECTATION
    # Given the parameters of each cluster, find the responsability of each user in each cluster
    #################################################################
    cat("\nExpectation...")
    responsabilities <- foreach(u=1:U, .packages=c('dplyr'), .export=c('likelihood_post'), .combine=rbind) %dopar%
                            update_responsabilities(df.trees, u, pis, alphas, betas, taus)

    cat("\nCluster distribution:\n", colSums(responsabilities))

    # MAXIMIZATION
    # Given the current responsabilities and pis, find the best parameters for each cluster
    ################################################################
    cat("\nMaximization...")
    # Parallel optimization for cluster k=1,...,K
    # neldermead::fminbnd does not deal well with boundaries
    # nlminb and nmkb give the same solution.
    # nmkb is a little bit faster
    # sol <- nlminb(c(alphas[k],betas[k],taus[k]), cost.function,
    #              scale = 1, lower=c(0,0,0), upper=c(Inf, Inf, 1))
    sols <- foreach(k=1:K, .packages=c('dfoptim'), .export=c('likelihood_post', 'Qopt')) %do% {
      nmkb(c(alphas[k], betas[k], taus[k]), Qopt.par, 
           lower = c(0,0,0), upper = c(Inf, Inf, 1), 
           control = list(maximize=TRUE),
           df.trees = df.trees, responsabilities = responsabilities, pis = pis, k=k)
    }
            
    for(k in 1:K){
      sol <- sols[[k]]
      alphas[k] <- sol$par[1]
      betas[k]  <-  sol$par[2]
      taus[k]   <-  sol$par[3]
      traces[iter,k] <- sol$value
    }
    params$alphas <- alphas
    params$betas <- betas
    params$taus <- taus


    ###############################################################
    # EVALUATION OF FULL LIKELIHOOD p(X, Z | \theta)
    # this should be monotonically increasing
    ###############################################################
    like <- likelihood_Lumbreras2016(df.trees, params, responsabilities, pis)
    likes[iter] <- c(t(pis) %*% traces[iter,])
    cat("\n\nalphas: ", alphas)
    cat("\nbetas: ", betas)
    cat("\ntaus: ", taus)
    cat("\n likelihood: ", like)
    cat("\n Q: ", sum(traces[iter,]))
    cat("\n Entropy: ", sum(responsabilities*log(responsabilities)))
    cat("\n***")

    # Update pis
    pis <- colSums(responsabilities)/nrow(responsabilities)
  }

  stopCluster(cl)

  list(alphas=alphas,
       betas=betas,
       taus=taus,
       responsabilities=responsabilities,
       pis = pis,
       traces=traces,
       likes=like,
       users=users)

}
