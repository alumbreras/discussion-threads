library(dplyr)
library(ggplot2)
#' Creates a dataframe with predictions from different models 
#' and real choices
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
    lags.inv <- 1:t
    popularities <- 1 + tabulate(parents[1:(t-1)], nbins=t)
    popularities[1] <- popularities[1] - 1 # root has no parent
    
    chosen <- parents[t]
    
    # Lumbreras 2016
    k <- z[V(tree)$userint[t+1]] # Attention to the t+1!
    alpha <- params.lumbreras$alphas[k]
    beta <- params.lumbreras$betas[k]
    tau <- params.lumbreras$taus[k]
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
    
    # Kumar 2010 (TODO. Need params.kumar)
    
    #Time ellapsed between post and parent (or post and root)
    time.to.parent <- V(tree)$date[t+1] - V(tree)$date[chosen]
    time.to.root <- V(tree)$date[t+1] - V(tree)$date[1]
    
    # Barabasi
    probs.barabasi <- alpha*popularities
    predicted.barabasi <- which.max(alpha*popularities)
    ranking.barabasi <- rank(-probs.barabasi)[chosen] 
    
    # Naive sorted
    probs.tau <- tau^lags
    predicted.tau <-  which.max(probs.tau)
    ranking.tau <- rank(-probs.tau)[chosen]
    
    # Naive sorted inverse
    probs.tau.inv <- tau^lags.inv
    predicted.tau.inv <-  which.max(probs.tau.inv) 
    ranking.tau.inv <- rank(-probs.tau.inv)[chosen]
    
    df.preds <- rbind(df.preds, 
                      c(predicted.lumbreras, like.lumbreras, ranking.lumbreras,
                        predicted.gomez, like.gomez, ranking.gomez,
                        predicted.barabasi, ranking.barabasi,
                        predicted.tau, ranking.tau,
                        predicted.tau.inv, ranking.tau.inv,
                        time.to.parent, time.to.root,
                        t, chosen))
  }
  names(df.preds) <- c('predicted.lumbreras', 'like.lumbreras', 'ranking.lumbreras',
                       'predicted.gomez', 'like.gomez', 'ranking.gomez',
                       'predicted.barabasi', 'ranking.barabasi',
                       'predicted.tau', 'ranking.tau',
                       'predicted.tau.inv', 'ranking.tau.inv',
                       'time.to.parent', 'time.to.root',
                       'tree.size', 'chosen')
  df.preds
}



plot_ranking_benchmarks <- function(df.preds){
  
  # clean the few NA rows (maybe due to taus near 1)
  df.preds <- df.preds[complete.cases(df.preds),] 
  
  # Add baseline predictions
  df.preds <- mutate(df.preds, like.baseline=log(1/tree.size))
  df.preds <- mutate(df.preds, predicted.baseline=1)
  
  # Compute cummulative hits
  df.preds <- mutate(df.preds, hit.gomez = cumsum(as.numeric(predicted.gomez==chosen)))
  df.preds <- mutate(df.preds, hit.lumbreras = cumsum(as.numeric(predicted.lumbreras==chosen)))
  df.preds <- mutate(df.preds, hit.baseline = cumsum(as.numeric(predicted.baseline==chosen)))

  # Seconds to minutes
  df.preds <- mutate(df.preds, mins.to.parent = floor(time.to.parent/60))
  df.preds <- mutate(df.preds, mins.to.root = floor(time.to.root/60))
  
  cum.hits.gomez <- cumsum(df.preds$hit.gomez)  
  cum.hits.lumbreras <- cumsum(df.preds$hit.lumbreras)  
  cum.hits.baseline <- cumsum(df.preds$hit.baseline)
  
  par(mfrow=c(1,1))
  vmax <- max(c(cum.hits.baseline, cum.hits.gomez, cum.hits.lumbreras))
  vmin <- min(c(cum.hits.baseline, cum.hits.gomez, cum.hits.lumbreras))
  plot(1:length(cum.hits.baseline), cum.hits.baseline, ylim=c(vmin,vmax), type='l', col='black', xlab="posts", ylab='hits')
  lines(1:length(cum.hits.baseline), cum.hits.gomez, col='blue')
  lines(1:length(cum.hits.baseline), cum.hits.lumbreras, col='red')
  legend("bottomright", c('baseline', 'lumbreras', 'gomez'), col=c('black', 'red', 'blue'), pch=19, inset = 0.05)
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
  df.preds <- select(df.preds, -hit.gomez, -hit.baseline, -hit.lumbreras)
  df.rankings <- select(df.preds, ranking.lumbreras, 
                        ranking.gomez, 
                        ranking.barabasi,
                        ranking.tau,
                        ranking.tau.inv,
                        tree.size) %>% 
    filter(tree.size<1000) %>%
    melt(id.vars='tree.size', variable.name='model', value.name='rank')
  
  by_model <- group_by(df.rankings, model, tree.size) %>%
    summarize(rank=mean(rank), rank.sd = sd(rank))
  
  g <-ggplot(by_model, aes(x = tree.size, y=rank, group=model, color=model)) +
      geom_point(alpha=0.1) +
      scale_size_area() + 
      geom_smooth() +
      xlab("thread size") +
      theme_bw()+
      theme(text = element_text(size = 13),
            legend.key = element_blank(),
            aspect.ratio = 1)
  
  print(g)
  
  # Plot by position of chosen parent (depth and/or order)
  df.rankings <- select(df.preds, ranking.lumbreras, 
                        ranking.gomez, 
                        ranking.barabasi, 
                        ranking.tau,
                        ranking.tau.inv,
                        tree.size, chosen) %>% 
    filter(tree.size<1000) %>%
    melt(id.vars='chosen', variable.name='model', value.name='rank')
  
  by_model <- group_by(df.rankings, model, chosen) %>%
    summarize(rank=mean(rank), rank.sd = sd(rank))
  
  g <- ggplot(by_model, aes(x = chosen, y=rank, group=model, color=model)) +
      geom_point(alpha=0.1) +
      scale_size_area() + 
      geom_smooth() +
      xlab("chosen") +
      theme_bw()+
      theme(text = element_text(size = 13),
            aspect.ratio = 1)
  print(g)
  
  # Plot by minutes to parent
  df.rankings <- select(df.preds, ranking.lumbreras, 
                        ranking.gomez, 
                        ranking.barabasi, 
                        ranking.tau,
                        ranking.tau.inv,
                        tree.size,
                        mins.to.parent) %>% 
    filter(mins.to.parent<60*24*30) %>%
    melt(id.vars='mins.to.parent', variable.name='model', value.name='rank')
  
  by_model <- group_by(df.rankings, model, mins.to.parent) %>%
    summarize(rank=mean(rank), rank.sd = sd(rank))
  
  g <- ggplot(by_model, aes(x = mins.to.parent, y=rank, group=model, color=model)) +
    geom_point(alpha=0.1) +
    scale_size_area() + 
    geom_smooth() +
    xlab("mins to parent") +
    theme_bw()+
    theme(text = element_text(size = 13),
          aspect.ratio = 1)
  print(g)
  
  # Plot by minutes to root (time of thread)
  df.rankings <- select(df.preds, ranking.lumbreras, 
                        ranking.gomez, 
                        ranking.barabasi, 
                        ranking.tau,
                        ranking.tau.inv,
                        tree.size,
                        mins.to.root) %>% 
    filter(mins.to.root<60*24*30) %>%
    melt(id.vars='mins.to.root', variable.name='model', value.name='rank')
  
  by_model <- group_by(df.rankings, model, mins.to.root) %>%
    summarize(rank=mean(rank), rank.sd = sd(rank))
  
  g <- ggplot(by_model, aes(x = mins.to.root, y=rank, group=model, color=model)) +
    geom_point(alpha=0.1) +
    scale_size_area() + 
    geom_smooth() +
    xlab("mins to root") +
    theme_bw()+
    theme(text = element_text(size = 13),
          aspect.ratio = 1)
  print(g)
  
}

