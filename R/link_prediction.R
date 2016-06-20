library(dplyr)

#' Creates a dataframe with predictions and real choices
compare_link_prediction <- function(tree, params.lumbreras, params.gomez){
  # We can compare with barabasi because the choice is independent of the alpha
  parents <- get.edgelist(tree, names=FALSE)[,2] # parents vector
  
  z <- params.lumbreras$z 
  
  df.preds <- data.frame()
  # skip root and first post
  for(t in 2:length(parents)){
   
    # Data common two all models
    b <- rep(0,t)
    lags <- t:1
    popularities <- 1 + tabulate(parents[1:(t-1)], nbins=t)
    popularities[1] <- popularities[1] - 1 # root has no parent
    
    chosen <- parents[t]
    
    # Lumbreras 2016
    z.user <- z[V(tree)$userint[t]]
    alpha <- params.lumbreras$alphas[z.user]
    beta <- params.lumbreras$betas[z.user]
    tau <- params.lumbreras$taus[z.user]
    b[1] <- beta
    probs.lumbreras <- alpha*popularities + b + tau^lags
    predicted.lumbreras <- which.max(probs.lumbreras)
    like.lumbreras <- log(probs.lumbreras[chosen]) - log(sum(probs.lumbreras))
    ranking.lumbreras <- rank(-probs.lumbreras)[chosen] 
      
      
    # Gomez 2013
    alpha <- params.gomez[1]
    beta <- params.gomez[2]
    tau <- params.gomez[3]
    b[1] <- beta
    probs.gomez <- alpha*popularities + b + tau^lags
    predicted.gomez <- which.max(probs.gomez)
    like.gomez <- log(probs.gomez[chosen]) - log(sum(probs.gomez))
    ranking.gomez <- rank(-probs.gomez)[chosen] 
    
    # Barabasi
    probs.barabasi <- alpha*popularities
    predicted.barabasi <- which.max(alpha*popularities)
    ranking.barabasi <- rank(-probs.barabasi)[chosen] 
    
    df.preds <- rbind(df.preds, 
                      c(predicted.lumbreras, like.lumbreras, ranking.lumbreras,
                        predicted.gomez, like.gomez, ranking.gomez,
                        predicted.barabasi, ranking.barabasi,
                        t, chosen))
  }
  names(df.preds) <- c('predicted.lumbreras', 'like.lumbreras', 'ranking.lumbreras',
                       'predicted.gomez', 'like.gomez', 'ranking.gomez',
                       'predicted.barabasi', 'ranking.barabasi',
                       'tree.size', 'chosen')
  df.preds
}

cl <- makeCluster(detectCores()-2)
clusterExport(cl, c("compare_link_prediction", "params.lumbreras", "params.gomez"))
clusterEvalQ(cl, {library(igraph)})

df.preds <- parLapply(cl, trees_, function(tree) compare_link_prediction(tree, params.lumbreras, params.gomez)) %>% 
            rbindlist %>% 
            as.data.frame

stopCluster(cl)

# clean the few NA rows (maybe due to taus near 1)
df.preds <- df.preds[complete.cases(df.preds),] 

# Add baseline predictions
df.preds <- mutate(df.preds, like.baseline=log(1/tree.size))
df.preds <- mutate(df.preds, predicted.baseline=1)

# Compute cummulative hits
df.preds <- mutate(df.preds, hit.gomez = cumsum(as.numeric(predicted.gomez==chosen)))
df.preds <- mutate(df.preds, hit.lumbreras = cumsum(as.numeric(predicted.lumbreras==chosen)))
df.preds <- mutate(df.preds, hit.baseline = cumsum(as.numeric(predicted.baseline==chosen)))


#save(df.preds, file='data/df.preds.rda')
#load('data/df.preds.rda')

cum.hits.gomez <- cumsum(df.preds$hit.gomez)  
cum.hits.lumbreras <- cumsum(df.preds$hit.lumbreras)  
cum.hits.baseline <- cumsum(df.preds$hit.baseline)

par(mfrow=c(1,1))
vmax <- max(c(cum.hits.baseline, cum.hits.gomez, cum.hits.lumbreras))
vmin <- min(c(cum.hits.baseline, cum.hits.gomez, cum.hits.lumbreras))
plot(1:length(cum.hits.baseline), cum.hits.baseline, ylim=c(vmin,vmax), type='l', col='black', xlab="posts", ylab='hits')
lines(1:length(cum.hits.baseline), cum.hits.gomez, col='blue')
lines(1:length(cum.hits.baseline), cum.hits.lumbreras, col='red')
title('Hits')

cum.likes.gomez <- cumsum(df.preds$like.gomez)
cum.likes.lumbreras <- cumsum(df.preds$like.lumbreras)
cum.likes.baseline <- cumsum(df.preds$like.baseline)

par(mfrow=c(1,1))
vmax <- max(c(cum.likes.baseline, cum.likes.gomez, cum.likes.lumbreras))
vmin <- min(c(cum.likes.baseline, cum.likes.gomez, cum.likes.lumbreras))

plot(1:length(cum.likes.baseline), cum.likes.baseline, type='l', col='black', xlab="posts", ylab='likelihood')
lines(1:length(cum.likes.baseline), cum.likes.gomez, col='blue')
lines(1:length(cum.likes.baseline), cum.likes.lumbreras, col='red')
legend("topright", c('baseline', 'lumbreras', 'gomez'), col=c('black', 'red', 'blue'), pch=19, inset = 0.05)
title('Likelihood')

# Recommender systems
###############################'
# see grouped operations
#https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html

df.rankings <- select(df.preds, ranking.lumbreras, 
                                ranking.gomez, 
                                ranking.barabasi, 
                                tree.size) %>% 
               filter(tree.size<50) %>%
               melt(id.vars='tree.size', variable.name='model', value.name='rank')

by_model <- group_by(df.rankings, model, tree.size) %>%
            summarize(rank.mean=mean(rank), rank.sd = sd(rank))

ggplot(by_model, aes(x = tree.size, y=rank.mean, group=model, color=model)) +
  geom_point() +
  scale_size_area() + 
  theme_bw()





