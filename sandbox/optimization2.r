# Methods to find the best alpha, beta, tau parameters in the 
# likelihood function of the EM
##############################################################
library(doParallel)
library(foreach)

opt.grid <- function(Q, params0, ...){
  source('likelihood.r')
  
  # Grid search
  cl <- makeCluster(detectCores()-3);
  registerDoParallel(cl)
  df.results <- foreach(alpha = seq(0.01, 0.1, by=0.01), .combine = rbind, .packages=c('foreach'), .export=c("likelihood.post")) %dopar% {
                  foreach(beta = seq(0.01, 0.14, by=0.01), .combine = rbind) %do% {
                    foreach(tau = seq(0.01, 0.1, by=0.01), .combine = rbind) %do% {
                      like <- Q(alpha, beta, tau, ...)
                      data.frame(alpha = alpha, beta = beta, tau = tau, like = like)
                    }
                  }
  }
  stopCluster(cl)
  df.results$like <- df.results$like - max(df.results$like)
  df.results#[which.max(df.results$like),]
}

#pis_k <- c(0.5,0.5)
if(FALSE){
  df.results <- opt.grid(Qopt, params0=c(1,2,3), 
                         resp_k = responsabilities_k,
                         df.trees = df.trees)
}

########################################################
########################################################
if(FALSE){
outfun <- function(x, optimValues, state){
  cat('\n', x)
}
opt <- optimset(OutputFcn=outfun, MaxFunEvals=10)
opt.fminnd <- function(Q){
  
  fminbnd(fun = cost.function, x0=c(alphas[k],betas[k],taus[k]), 
          xmin=c(0.09,0.09,0.09), xmax=c(10,200,0.99), optimset(OutputFcn=outfun.bar))
  xopt <- neldermead.get(sol, "xopt")
}
}