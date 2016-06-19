compare_link_prediction <- function(trees, params.lumbreras, params.gomez){
  #TODO: We can't use Barabasi and Kumar without using their own estimated parameters!
  # We can compare with barabasi because the choice is independent of the alpha
  nposts <- sum(sapply(trees, vcount))
  tmp <- rep(0, nposts)
  DT <- data.table(pred.lumbreras=tmp,
                   like.lumbreras=tmp,
                   pred.gomez=tmp,
                   like.gomez=tmp,
                   pred.kumar=tmp,
                   like.kumar=tmp,
                   pred.barabasi=tmp,
                   like.barabasi=tmp,
                   thread.size = tmp,
                   chosen = tmp)
  
  z <- params.lumbreras$z 
 
 like.gomez <- vector()
 like.lumbreras <- vector()
 hit.gomez <- vector()
 hit.lumbreras <- vector()
 p.random.hit <- vector()
 diffs <- 0 # different predictions Gomez and Lumbreras
 chosen <- vector()
 ntrees <- length(trees)
 result <- data.frame()
 
 j <- 1
 for (i in 1:ntrees){
     if(i%%10==0){ 
      cat("\ntrees:", i, "/", ntrees)
     }
     g <- trees[[i]]
     parents <- get.edgelist(g, names=FALSE)[,2] # parents vector
     # replace names by order
     
     # skip root and first post
     for(t in 2:length(parents)){
       
       # Data common two all models
       b <- rep(0,t)
       lags <- t:1
       popularities <- 1 + tabulate(parents[1:(t-1)], nbins=t)
       popularities[1] <- popularities[1] - 1 # root has no parent
       
       chosen <- parents[t]
       
       # Lumbreras 2016
       z.user <- z[V(g)$userint[t]]
       alpha <- params.lumbreras$alphas[z.user]
       beta <- params.lumbreras$betas[z.user]
       tau <- params.lumbreras$taus[z.user]
       b[1] <- beta
       probs.lumbreras <- alpha*popularities + b + tau^lags
       predicted.lumbreras_ <- which.max(probs.lumbreras)
       like.lumbreras_ <- probs.lumbreras[chosen]/sum(probs.lumbreras)
       
       # Gomez 2013
       alpha <- params.gomez[1]
       beta <- params.gomez[2]
       tau <- params.gomez[3]
       b[1] <- beta
       probs.gomez <- alpha*popularities + b + tau^lags
       predicted.gomez_ <- which.max(probs.gomez)
       like.gomez_ <- probs.gomez[chosen]/sum(probs.gomez)
       
       # Kumar (beta = 0)
       probs.kumar <- alpha*popularities + tau^lags
       predicted.kumar_ <- which.max(probs.kumar)
       like.kumar_ <- probs.kumar[chosen]/sum(probs.kumar)
       
       # Barabasi
       probs.barabasi <- alpha*popularities
       predicted.barabasi_ <- which.max(alpha*popularities)
       like.barabasi_ <- probs.barabasi[chosen]/sum(probs.barabasi)
       
       chosen_ <- chosen # avoid names of datatable columns
       DT[j, pred.lumbreras := predicted.lumbreras_]
       DT[j, like.lumbreras := like.lumbreras_]
       DT[j, pred.gomez := predicted.gomez_]
       DT[j, like.gomez := like.gomez_]
       DT[j, pred.kumar := predicted.kumar_]
       DT[j, like.kumar := like.kumar_]
       DT[j, pred.barabasi := predicted.barabasi_]
       DT[j, like.barabasi := like.barabasi_]
       DT[j, thread.size := t]
       DT[j, chosen := chosen_]
       j <- j+1
       
    } # end post
  } # end thread
 df <- as.data.frame(DT)
 df <- df[1:j,]
 df
}

res <- compare_link_prediction(trees_, params.lumbreras, params.gomez)
cum.hits.gomez <- cumsum(res$hit.gomez)  
cum.hits.lumbreras <- cumsum(res$hit.lumbreras)  
cum.random.hits <- cumsum(res$p.random.hit)

cum.likes.gomez <- cumsum(res$like.gomez)
cum.likes.lumbreras <- cumsum(res$like.lumbreras)

cat("Different predictions: ", res$diffs)
par(mfrow=c(1,1))
vmax <- max(c(cum.random.hits, cum.hits.gomez))
plot(1:length(cum.random.hits), cum.random.hits, ylim=c(0,vmax), type='l', col='black', xlab="posts", ylab='hits')
lines(1:length(cum.random.hits), cum.hits.gomez, col='blue')
lines(1:length(cum.random.hits), cum.hits.lumbreras, col='red')

par(mfrow=c(1,1))
vmax <- max(c(cum.random.hits, cum.hits.gomez))
plot(1:length(cum.random.hits), cum.random.hits, ylim=c(0,vmax), type='l', col='black', xlab="posts", ylab='likelihood')
lines(1:length(cum.random.hits), cum.likes.gomez, col='blue')
lines(1:length(cum.random.hits), cum.likes.lumbreras, col='re
d')