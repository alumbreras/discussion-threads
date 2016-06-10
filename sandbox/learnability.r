# With realistic parameters, how many posts do I need for user to
# propery re-esmite their cluster given that I know the real parameters for each cluster?
source('thread_generator.r')

# In Gomez 2013, they generate 100 different sets of parameters and estimate then in N=50, 500, 5000, 50000 threads
# we can also do this for nusers = 2, 10, 100, 1000?

set.seed(1)
alphas <- c(0.1, 0.5)
betas <- c(0.1, 0.5)
taus <- c(0.1, 0.5)
z <- c(1,2)

# Create dataset
##############################################
# Create trees in parallel
cl <- makeCluster(detectCores()-1)  
clusterEvalQ(cl, {library(igraph); source('thread_generator.r')})
clusterExport(cl, c("alphas", "betas", "taus", "z"))
trees <- parLapply(cl, 1:10000, function(i) gen.thread.Lumbreras2016(n=100, z=z, alphas=alphas, betas=betas, taus=taus) )
stopCluster(cl)


################ SEQUENTIAL
#set.seed(1)
#alphas <- c(0.1,0.5)
#betas <- c(0.1,0.5)
#taus <- c(0.1, 0.5)
#z <- c(1,2)
#trees <- replicate(1000,  gen.thread.Lumbreras2016(n=100, z=z, alphas=alphas, betas=betas, taus=taus), simplify = FALSE)

###############################
################################


# Cast to dataframes in parallel
ncores <- detectCores() - 2
cl<-makeCluster(ncores, outfile="", port=11439)
clusterEvalQ(cl, {library(igraph); library(dplyr)})
data.parlapply <- parLapply(cl, trees, tree.to.data)
stopCluster(cl)


# Join dataframes adding thread id
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



## Select a subset of N threads
N <- 50
id.trees <- sample(1:10000, N)
df.trees.subset <- filter(df.trees, thread %in% id.trees)


### Compute p(Z | X, alpha, beta, tau)
K <- length(alphas)
users <- unique(df.trees.subset$user)
U <- length(users)
responsabilities <- matrix(nrow = U, ncol = K)
pis <- rep(1/ncol(responsabilities), ncol(responsabilities))

for (u in 1:U){
  responsabilities[u,] <- update_responsabilities(df.trees.subset, u, pis, alphas, betas, taus)  
}

print(responsabilities)
