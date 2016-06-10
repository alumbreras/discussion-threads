# Check that we can properly recover the true parameters
# author: Alberto Lumbreras
#########################################################
library(parallel)
library(data.table)
library(neldermead)
library(dfoptim)
library(dplyr)
library(ggplot2)
source('R/thread_generators.R')
source('R/likelihood.R')

df.errors <- data.frame(alpha=rep(NA,100*4), 
                        beta=rep(NA,100*4),
                        tau=rep(NA,100*4),
                        alpha_hat=rep(NA,100*4),
                        beta_hat=rep(NA,100*4),
                        tau_hat=rep(NA,100*4),
                        N = rep(NA,100*4),
                        like = rep(NA,100*4))

# Repeat the experiment with different parameters to see error mean and variance
ncores <- detectCores() - 2
cl<-makeCluster(ncores, outfile="", port=11439)
clusterEvalQ(cl, {library(igraph); library(dplyr)})

N.list <- c(50,500,5000,50000)

# Length of simulated threads will be based on real thread lengths
load('dfposts_gameofthrones.Rda')
thread.lengths <- as.numeric(table(df.posts$thread))

# Too many short threads make it very difficult to estimate the parameters
# We therefore impose a minimum length
thread.lengths <- thread.lengths[thread.lengths>10]

row <- 1
for (j in 1:length(N.list)){
  N <- N.list[j]

  for(i in 1:100){
    cat("\n****************************************** ", i)
    cat("\n******************************************\n")
    
    ###############
    # generate N trees with fixed parameters
    ###############
    alpha <- runif(1)*5
    beta <- runif(1)*10
    tau <- runif(1)
    thread.size <- thread.lengths[sample(1:length(thread.lengths), 1)]
   
    # TODO: parallelize this as well
    trees <- replicate(N, gen.thread.Gomez2013(n=thread.size, alpha=alpha, beta=beta, tau=tau), simplify = FALSE)
    
    # Create one dataframe from each tree
    data.parlapply <- parLapply(cl, trees, tree.to.data)
    
    # join dataframes adding thread id
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
    
    
    ########################################
    # Estimation
    ########################################
    Qopt <- function(params, df.trees){
      sum(apply(df.trees, 1, function(x) likelihood.post(x, params[1], params[2], params[3])))
    }
    
    sol <- nmkb(c(0.5, 0.5, 0.5), Qopt, 
                lower = c(0,0,0), upper = c(Inf, Inf, 1), 
                control = list(maximize=TRUE),
                df.trees = df.trees)
    cat('\n', alpha, " ", beta, " ", tau)
    cat('\n', sol$par)
    
    # Store results
    #################
    df.errors[row,] <- c(alpha, beta, tau, sol$par[1], sol$par[2], sol$par[3], N, sol$value)
    row <- row + 1
  }
}

stopCluster(cl)
df.errors <- df.errors[complete.cases(df.errors),]
save("df.errors", "df.errors.Rda")

#########################
# Analize errors
#########################
if(FALSE){
  df.errors <- mutate(df.errors, alpha.error = alpha_hat - alpha,
                                 beta.error = beta_hat - beta,
                                 tau.error = tau_hat - tau)
  
  df.errors <- mutate(df.errors, alpha.error.perc = (alpha_hat - alpha)/alpha,
                      beta.error.perc = (beta_hat - beta)/beta,
                      tau.error.perc = (tau_hat - tau)/tau)
  
  
  df.errors.g <- melt(df.errors, measure.vars=c("alpha.error", "beta.error", "tau.error",
                                                "alpha.error.perc", "beta.error.perc", "tau.error.perc"))
  
  ggplot(filter(df.errors.g, N==50), aes(x=variable, y=value)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4) + 
    ylim(c(-1,1)) + 
    theme_bw()
}