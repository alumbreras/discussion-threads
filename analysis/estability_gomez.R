# Run gomez many times under the current dataset and see how many posts it needs
# to stabilise the estimations
library(dplyr)
library(tidyr)
ncores <- detectCores() - 2

df.params.total <- list()
#N
for(nposts in c(100000,200000, 300000, 400000, 500000)){

for(nposts in c(1000,2000, 3000, 4000, 5000)){
  cl <- makeCluster(ncores, outfile="", port=11439)
  registerDoParallel(cl)
  cat("\nposts:", nposts)
  
  # boostraps
  df.params <- foreach(repetition=1:5, 
      .combine = rbind,
      .packages=c('dfoptim', 'dplyr'), 
      .export=c('nposts', 'estimation_Gomez2013')) %dopar% 
        {
          #set.seed(n)
          idx <- sample(1: nrow(df.trees.allusers), nposts)
          params <- list(alpha = runif(1,0,2),
                         beta = runif(1,0,10),
                         tau = runif(1))
          res <- estimation_Gomez2013(df.trees.allusers[idx,], params)
          like <- likelihood_Gomez2013(df.trees.allusers[idx,], params)
          data.frame(nposts=nposts, 
                     repetition=repetition,
                     alpha = res$alpha,
                     beta = res$beta,
                     tau = res$tau,
                     like = like/nposts)
        }
  stopCluster(cl)
  
  df.params.total <- rbind(df.params.total, df.params)
}


df.params  <- df.params %>% gather(variable, value, alpha:tau)
ggplot(df.params, aes(x=variable, y=value)) + 
  geom_boxplot(aes(group=variable)) + 
  theme_bw()


alphas <- sapply(params.gomez.list, function(x) x$alpha)
betas <- sapply(params.gomez.list, function(x) x$beta)
taus <- sapply(params.gomez.list, function(x) x$tau)

plot(seq(10000, 500000, by=100000), alphas)
plot(seq(10000, 500000, by=100000), betas)
plot(seq(10000, 500000, by=100000), taus)