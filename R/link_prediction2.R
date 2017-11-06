library(ggplot2)
library(dplyr)
library(data.table)
#' Creates a dataframe with predictions from different models 
#' and real choices
#' params tree dataframe 
compare_link_prediction2 <- function(df.tree, params.lumbreras, params.gomez, params.gomezplus){
  # We can compare with barabasi becaus the choice is independent of the alpha
  #parents <- get.edgelist(tree, names=FALSE)[,2] # parents vector
  
  subforum <- df.tree$subforum[1]
  parents <- df.tree$parent
  parents.users <- df.tree$parent.user
  users <- df.tree$user
  z <- df.tree$z
  
  # Results container
  df.preds <- data.frame()
  
  # skip root and first post
  # Recall: t has already t posts before (because root t=0)
  # Thus, post at t has t posts to choose from.
  #for(t in 2:length(parents)){
  #for(t in which(df.tree$split=='test')){
  idx.tr.te <- c(which(df.tree$split=='test'), which(df.tree$split=='train'))
  for(t in idx.tr.te){
    # skip if the post is not marked for training
    #if(df.tree$split[t] != 'test') next
<<<<<<< HEAD
    
=======
  
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
    # Data common too all models
    b <- rep(0,t)
    lags <- t:1
    lags.inv <- 1:t
    popularities <- 1 + tabulate(parents[1:(t-1)], nbins=t) #  we follow Gomez. root also starts with 1
    grandparents <- c(FALSE, (df.tree$parent.user[1:(t-1)]==users[t])) # root is always false
    chosen <- parents[t]
    
    # Gomez 2013
    #######################
    alpha <- params.gomez$alpha
    beta <- params.gomez$beta
    tau <- params.gomez$tau
    b[1] <- beta
    probs.gomez <- alpha * popularities + b + tau^lags
    
    predicted.gomez <- which.max(probs.gomez)
    like.gomez <- log(probs.gomez[chosen]) - log(sum(probs.gomez))
    ranking.gomez <- rank(-probs.gomez)[chosen] 
    
    # Gomez 2013 Plus
    #######################
    alpha <- params.gomezplus$alpha
    beta <- params.gomezplus$beta
    tau <- params.gomezplus$tau
    gamma <- params.gomezplus$gamma
    b[1] <- beta
    probs.gomezplus <- alpha * popularities + gamma*grandparents + b + tau^lags
    
    predicted.gomezplus <- which.max(probs.gomezplus)
    like.gomezplus <- log(probs.gomezplus[chosen]) - log(sum(probs.gomezplus))
    ranking.gomezplus <- rank(-probs.gomezplus)[chosen] 
    
    # Lumbreras 2016
    ############################
    if(TRUE){
<<<<<<< HEAD
      #k <- z[V(tree)$userint[t+1]] # Attention to the t+1!
      k <- z[t]
      if (is.na(k)){
        stop("A user has no group. This user shouldnt be in the tests")
      }
      alpha <- params.lumbreras$alphas[k]
      beta <- params.lumbreras$betas[k]
      tau <- params.lumbreras$taus[k]
      b[1] <- beta
      probs.lumbreras <- alpha*popularities + b + tau^lags
      
      predicted.lumbreras <- which.max(probs.lumbreras)
      like.lumbreras <- log(probs.lumbreras[chosen]) - log(sum(probs.lumbreras))
      ranking.lumbreras <- rank(-probs.lumbreras)[chosen] 
=======
        #k <- z[V(tree)$userint[t+1]] # Attention to the t+1!
        k <- z[t]
        if (is.na(k)){
          stop("A user has no group. This user shouldnt be in the tests")
        }
        alpha <- params.lumbreras$alphas[k]
        beta <- params.lumbreras$betas[k]
        tau <- params.lumbreras$taus[k]
        b[1] <- beta
        probs.lumbreras <- alpha*popularities + b + tau^lags
        
        predicted.lumbreras <- which.max(probs.lumbreras)
        like.lumbreras <- log(probs.lumbreras[chosen]) - log(sum(probs.lumbreras))
        ranking.lumbreras <- rank(-probs.lumbreras)[chosen] 
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
    }
    if(FALSE){
      k <- z[t] 
      alpha <- params.lumbreras$alphas[k]
      beta <- params.lumbreras$betas[k]
      tau <- params.lumbreras$taus[k]
      gamma <- params.lumbreras$gammas[k]
      b[1] <- beta
      probs.lumbreras <- alpha*popularities + b + gamma*grandparents + tau^lags
      
      predicted.lumbreras <- which.max(probs.lumbreras)
      like.lumbreras <- log(probs.lumbreras[chosen]) - log(sum(probs.lumbreras))
      ranking.lumbreras <- rank(-probs.lumbreras)[chosen] 
    }
    
    time.to.root <- df.tree$date[t] 
    
    # Barabasi (only alpha)
    ######################################
    probs.barabasi <- alpha*popularities
    
    predicted.barabasi <- which.max(alpha*popularities)
    like.barabasi <- log(probs.barabasi[chosen]) - log(sum(probs.barabasi))
    ranking.barabasi <- rank(-probs.barabasi)[chosen] 
    
    # Naive sorted (the more recent, the more likely)
    #################################################
    probs.tau <- tau^lags
    
    predicted.tau <-  which.max(probs.tau)
    like.tau <-  log(probs.tau[chosen]) - log(sum(probs.tau))
    ranking.tau <- rank(-probs.tau)[chosen]
    
    # Naive sorted inverse (the older, the more likely)
    ###################################################
    probs.tau.inv <- tau^lags.inv
    
    predicted.tau.inv <-  which.max(probs.tau.inv) 
    like.tau.inv <-  log(probs.tau.inv[chosen]) - log(sum(probs.tau.inv))
    ranking.tau.inv <- rank(-probs.tau.inv)[chosen]
    
    # predicted: the most likely post
    # like: likelihood of the choosen post (the real observed one)
    # ranking: position in which the real choice was placed. Best is 1. Worst is current thread size
    
    df.preds <- rbindlist(list(df.preds,
<<<<<<< HEAD
                               data.frame(predicted.lumbreras = predicted.lumbreras,
                                          like.lumbreras = like.lumbreras,
                                          ranking.lumbreras =  ranking.lumbreras,
                                          predicted.gomez =  predicted.gomez,
                                          like.gomez =   like.gomez,
                                          ranking.gomez = ranking.gomez,
                                          predicted.gomezplus =  predicted.gomezplus,
                                          like.gomezplus =   like.gomezplus,
                                          ranking.gomezplus = ranking.gomezplus,
                                          predicted.barabasi = predicted.barabasi,
                                          like.barabasi = like.barabasi,
                                          ranking.barabasi = ranking.barabasi,
                                          predicted.tau =  predicted.tau,
                                          like.tau = like.tau,
                                          ranking.tau = ranking.tau,
                                          predicted.tau.inv = predicted.tau.inv,
                                          like.tau.inv = like.tau.inv,
                                          ranking.tau.inv = ranking.tau.inv,
                                          time.to.root = time.to.root,
                                          tree.size = t,
                                          chosen = chosen,
                                          user = df.tree$user[t],
                                          z = df.tree$z[t],
                                          split = df.tree$split[t],
                                          subforum = subforum)))
=======
                          data.frame(predicted.lumbreras = predicted.lumbreras,
                                     like.lumbreras = like.lumbreras,
                                     ranking.lumbreras =  ranking.lumbreras,
                                     predicted.gomez =  predicted.gomez,
                                     like.gomez =   like.gomez,
                                     ranking.gomez = ranking.gomez,
                                     predicted.gomezplus =  predicted.gomezplus,
                                     like.gomezplus =   like.gomezplus,
                                     ranking.gomezplus = ranking.gomezplus,
                                     predicted.barabasi = predicted.barabasi,
                                     like.barabasi = like.barabasi,
                                     ranking.barabasi = ranking.barabasi,
                                     predicted.tau =  predicted.tau,
                                     like.tau = like.tau,
                                     ranking.tau = ranking.tau,
                                     predicted.tau.inv = predicted.tau.inv,
                                     like.tau.inv = like.tau.inv,
                                     ranking.tau.inv = ranking.tau.inv,
                                     time.to.root = time.to.root,
                                     tree.size = t,
                                     chosen = chosen,
                                     user = df.tree$user[t],
                                     z = df.tree$z[t],
                                     split = df.tree$split[t],
                                     subforum = subforum)))
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
    
  }
  df.preds
}


##############################################################################
# Link predictions (e.g.: for recsys)
##############################################################################
plot_ranking_benchmarks2 <- function(df.preds){
  
  # clean the few NA rows (maybe due to taus near 1)
  df.preds <- df.preds[complete.cases(df.preds),] 
  #df.preds$subforum <- as.character(df.preds$subforum)
  
<<<<<<< HEAD
  ##############################################################################
  # LIKELIHOOODS BY GROUP
  ##############################################################################
=======
  ### LIKELIHOOODS
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  # Sum of scores (interested on like.model)
  df.likes <- df.preds %>% group_by(split, subforum,z) %>% 
    summarise(lumbreras = sum(like.lumbreras),
              gomez = sum(like.gomez),
              #barabasi = sum(like.barabasi),
              #tau = sum(like.tau),
              sample= n())
  
  df.likes <- df.likes %>% 
    filter(split == 'test') %>%
    gather(key = 'model', value = 'like', 
<<<<<<< HEAD
           -subforum, -split, -z, -sample) 
=======
     -subforum, -split, -z, -sample) 
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  
  g1 <- ggplot(df.likes, aes(x=as.factor(z), y=-like/sample, group=model, fill=model)) +
    geom_bar(stat='identity', colour = 'black', position= 'dodge', width=.50) +
    #scale_fill_brewer(palette="OrRd") +
    scale_fill_grey()+
    #scale_fill_manual(values=c("grey", "white"))+
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'),
          text = element_text(size=12),
          legend.position = 'top',
          legend.title = element_blank(),
          legend.key.size = unit(0.4, "cm"),
          legend.key = element_blank()) +
    #guides(fill = guide_legend(override.aes = list(size=0.5)))+
<<<<<<< HEAD
   # guides(colour = "none",
  #         linetype = guide_legend(override.aes = list(linetype = 0))) +
    xlab('cluster') + ylab("negative loglikelihood")
  print(g1)
  ggsave(file=paste0('snam_linkprediction_likelihoods_', subforum_, '_k',k,'.eps'),
         width=100, height=75, units='mm')

   # HITS
=======
    # guides(colour = "none",
    #         linetype = guide_legend(override.aes = list(linetype = 0))) +
    xlab('cluster') + ylab("negative loglikelihood")
  print(g1)
  ggsave(file=paste0('ch4_linkprediction_likelihoods_', subforum_, '_k5.eps'),
         width=100, height=75, units='mm')
  
  
  
  # HITS
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  df.hits <- df.preds %>% group_by(split, subforum, z) %>% 
    summarise(lumbreras = sum(predicted.lumbreras==chosen)/n(),
              gomez = sum(predicted.gomez==chosen)/n(),
              barabasi = sum(predicted.barabasi==chosen)/n(),
              tau = sum(predicted.tau==chosen)/n(),
              sample = n())
  
  df.hits <- df.hits %>% 
    filter(split == 'test') %>%
    gather(key = 'model', value = 'hit', 
           -subforum, -split, -z, -sample) 
  df.hits$model <- factor(df.hits$model, levels = c('barabasi', 'tau', 'gomez', 'lumbreras'))
  
  g.hits <- ggplot(df.hits, aes(x=as.factor(z), y=hit, group=model, fill=model)) +
<<<<<<< HEAD
    geom_bar(stat='identity', colour = 'black', position= 'dodge') +
=======
    geom_bar(stat='identity', colour = 'black', position= 'dodge', width=.50) +
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
    scale_fill_brewer(palette="OrRd")+
    #scale_fill_grey()+
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'), 
          text = element_text(size=12),
          legend.title = element_blank(),
          legend.position = 'top',
          legend.key.size = unit(0.4, "cm"),
          legend.key = element_blank()) +
    guides(colour = "none",
           linetype = guide_legend(override.aes = list(linetype = 0))) +
    xlab('cluster') + ylab("hits")
  print(g.hits)
<<<<<<< HEAD
  ggsave(file=paste0('snam_linkprediction_hits_', subforum_, '_k', k, '.eps'), 
         width=210, height=75, units='mm')

=======
  ggsave(file=paste0('ch4_linkprediction_hits_', subforum_, '_k15.eps'), 
         width=210, height=75, units='mm')
  save(g2, file = "gplot_podemos_k5_linkprediction_hits.rda")
  
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  # Add baseline predictions
  df.preds <- mutate(df.preds, like.baseline=log(1/tree.size))
  df.preds <- mutate(df.preds, predicted.baseline=1)
  
<<<<<<< HEAD

=======
  # Seconds to minutes
  df.preds <- mutate(df.preds, mins.to.root = floor(time.to.root/60))
  df.preds <- mutate(df.preds, hours.to.root = floor(time.to.root/(3600)))
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  
  ########################### PLOTS 2 ##########################################
  ##############################################################################
  # RANKING BY SIZE
  ##############################################################################
<<<<<<< HEAD
  df.rankings <- df.preds %>% filter(split == 'test') %>%
=======
  df.rankings <- df.preds %>% filter(split == 'train') %>%
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
    select(size = tree.size,
           user, 
           z,
           chosen, 
           minutes = time.to.root,
           barabasi = ranking.barabasi, 
           tau = ranking.tau, 
           gomez = ranking.gomez, 
<<<<<<< HEAD
=======
           #gomez.plus = ranking.gomezplus, 
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
           lumbreras = ranking.lumbreras,
           subforum) %>% 
    gather(key = 'model', value = 'rank', 
           -size, -subforum, -chosen, -minutes, -user, -z) %>%
    mutate(ranking.error.abs = rank-1,
           ranking.error = (rank-1)/(size-1))
  
<<<<<<< HEAD
  df.rankings$model <- factor(df.rankings$model, levels = c('barabasi', 'tau', 'gomez', 'lumbreras'))
  
  
  # NRE by size (lines)
  g <- ggplot(filter(df.rankings, size<100), aes(x=size, y=ranking.error, group=model, 
                                                 color=model, shape=model, linetype=model)) +
    #geom_point(alpha=0.2) + 
    stat_summary(fun.y= mean, aes(group=model), geom='point', alpha=1, size=1.5) +
    stat_summary(fun.y= mean, aes(group=model), geom='line', alpha=1, size=0.5) +
    theme_bw() + theme(strip.background = element_rect(fill = 'white'), 
                       text = element_text(size=14),
                       legend.title = element_blank(),
                       legend.key = element_blank()) +
    guides(colour = guide_legend(override.aes = list(size=2)),
           linetype = guide_legend(override.aes = list(linetype = 0))) +
    ylab('Normalised Ranking Error')
  print(g) 
  ggsave(file=paste0('snam_linkprediction_rankerror_by_size_', subforum_, '_k', k, '.eps'),
         width=175, height=100, units='mm')
  save(g, file = "gplot_podemos_k10_linkprediction__rankerror_by_size.rda")
  
  ##############################################################################
  # RANK BY GROUP
  ##############################################################################
  # Only boxes, means and medians
  df.rankings_ <- df.rankings
  df.rankings_$z <- as.factor(df.rankings_$z)
  df.rankings_ <- filter(df.rankings_, size<1000)
  
  g.boxplots <- ggplot(df.rankings_, aes(x=z,y = ranking.error, fill=model)) +
    geom_boxplot(outlier.size=0, outlier.shape = NA) +
    #geom_boxplot(aes(x=z, y = ranking.error, fill=model, position = 'dodge'), 
    #             outlier.size=0.5, outlier.shape=3) +
    stat_summary(fun.y=mean, geom = 'point', aes(group=model), color = 'black',  size=2, position=position_dodge(width=0.75)) +
    scale_fill_brewer(palette="OrRd") + 
    theme_bw() + theme(strip.background = element_rect(fill = 'white'), 
                       text = element_text(size=12),
                       legend.position = 'top',
                       legend.title = element_blank(),
                       legend.key = element_blank()) +
    guides(colour = guide_legend(override.aes = list(size=2)),
           linetype = guide_legend(override.aes = list(linetype = 0))) +
    xlab("cluster") + ylab('Normalised Ranking Error')
  print(g.boxplots)
  ggsave(file=paste0('snam_linkprediction_NRE_whiskers_', subforum_, '_k', k,'.eps'), 
         width=210, height=75, units='mm')
  
  if(FALSE){
    
  ## Boxplots with outliers
  df.rankings_ <- df.rankings
  df.rankings_$z <- as.factor(df.rankings_$z)
  
  g.boxplots <- ggplot(filter(df.rankings_, size<1000), 
                       aes(x=z, y = ranking.error)) +
    geom_boxplot(aes(x=z, y = ranking.error, fill=model, position = 'dodge'), 
                 outlier.size=0.5, outlier.shape=3) +
    stat_summary(fun.y=mean, aes(x=z, fill=model), geom = 'point', color = 'red') +
    scale_fill_brewer(palette="OrRd") + 
    theme_bw() + theme(strip.background = element_rect(fill = 'white'), 
                       text = element_text(size=14),
                       legend.title = element_blank(),
                       legend.key = element_blank()) +
    guides(colour = guide_legend(override.aes = list(size=2)),
           linetype = guide_legend(override.aes = list(linetype = 0))) +
    xlab("cluster") + ylab('Normalised Ranking Error')
  print(g.boxplots)
  ggsave(file=paste0('snam_linkprediction_error_boxplots', subforum_, '_k', k, '.eps'), 
         width=150, height=70, units='mm')
  
  
  
  source('R/hadley.R')
  combined <- grid_arrange_shared_legend(g.hits, g.boxplots, ncol = 2, nrow = 1)
  ggsave(file=paste0('snam_linkprediction_hits_whispers', subforum_, '_k', k, '.eps'), combined, width=210, height=100, units='mm')
  
  gg <-arrangeGrob(g.hits,g.boxplots)
  grid.newpage()
  grid.draw(gg)
  
  
  # alternative 2 (means)
  df.rankings_ <- df.rankings
  df.rankings_$z <- as.factor(df.rankings_$z)
  g <- ggplot(filter(df.rankings, size<20000), aes(x=as.factor(z), 
                                                   y=ranking.error,
                                                   group=model,
                                                   color=model, 
                                                   shape= model, 
                                                   linetype=model)) +
    stat_summary_bin(fun.y= median, geom='point', size=3) +
    stat_summary_bin(fun.y= median, geom='line', size=1) +
    theme_bw() + theme(strip.background = element_rect(fill = 'white'), 
                       legend.position = 'top',
                       legend.title = element_blank(),
                       legend.key = element_blank()) +
    #axis.text.x = element_blank(),) +
    guides(colour = guide_legend(override.aes = list(size=3)),
           linetype = guide_legend(override.aes = list(linetype = 0))) +
    xlab("cluster") + ylab('Normalized Ranking Error')
  print(g)
  ggsave(file=paste0('snam_linkprediction_NRE_meanlines_', subforum_, '_k', k, '.eps'),
         width=100, height=75, units='mm')
  
  
  
=======
  
  df.rankings$model <- factor(df.rankings$model, levels = c('barabasi', 'tau', 'gomez', 'lumbreras'))
  
  ### RANKING ERRORS BARS (with SE errors)
  df.rankings_ <- df.rankings %>% group_by(subforum, model, z) %>%
    summarise(mean = mean(ranking.error), 
              sd = sd(ranking.error), 
              se = sd(ranking.error)/n(),
              samples = n()) %>%
    filter(model != 'gomez.plus')
  
  # Bars
  ggplot(df.rankings_, aes(x=as.factor(z), y = mean, group=model, fill=model)) +
    geom_bar(stat='identity', colour = 'black', position = 'dodge') + 
    geom_errorbar(position = 'dodge', aes(ymin=mean, ymax = mean))+
    scale_fill_brewer(palette="OrRd") + 
    theme_bw() + 
    theme(strip.background = element_rect(fill = 'white'), 
          legend.title = element_blank(),
          legend.key = element_blank())
    ggsave(file=paste0('ch4_linkprediction_errors', subforum_, '_k10.eps'))
  
  
  # Lines
  g <- ggplot(filter(df.rankings, size<100), aes(x=size, y=ranking.error, group=model, 
                               color=model, shape=model, linetype=model)) +
        #geom_point(alpha=0.2) + 
        stat_summary(fun.y= mean, aes(group=model), geom='point', alpha=1, size=1.5) +
        stat_summary(fun.y= mean, aes(group=model), geom='line', alpha=1, size=0.5) +
        #geom_abline(intercept = 0, slope=1, linetype='dotted') +
        #geom_abline(intercept = 0, slope=0.5, linetype='dashed') +
        theme_bw() + theme(strip.background = element_rect(fill = 'white'),
                           legend.position = 'top',
                           text = element_text(size=12),
                           legend.title = element_blank(),
                           legend.key = element_blank()) +
        guides(colour = guide_legend(override.aes = list(size=2)),
               linetype = guide_legend(override.aes = list(linetype = 0))) +
        ylab('Normalized Ranking Error')
        print(g) 
        ggsave(file=paste0('ch4_linkprediction_rankerror_by_size_abs', subforum_, 'k5.eps'),
               width=210, height=75, units='mm')
        save(g, file = "gplot_podemos_k5_linkprediction__rankerror_by_size.rda")
        
        # RANK BY GROUP
        ##############################################################################
        ## Boxplots
        df.rankings_ <- df.rankings
        df.rankings_$z <- as.factor(df.rankings_$z)
        
        g.boxplots <- ggplot(filter(df.rankings_, size<1000), 
                    aes(x=z, y = ranking.error)) +
          geom_boxplot(aes(x=z, y = ranking.error, fill=model, position = 'dodge'), 
                       outlier.size=0.5, outlier.shape=3, width=0.5) +
          scale_fill_brewer(palette="OrRd") + 
          theme_bw() + theme(strip.background = element_rect(fill = 'white'), 
                             text = element_text(size=12),
                             legend.title = element_blank(),
                             legend.key = element_blank()) +
          guides(colour = guide_legend(override.aes = list(size=2)),
                 linetype = guide_legend(override.aes = list(linetype = 0))) +
          xlab("cluster") + ylab('Normalised Ranking Error')
        print(g.boxplots)
        ggsave(file=paste0('ch4_linkprediction_error_boxplots', subforum_, '_k5.eps'), 
               width=150, height=70, units='mm')
        
        
        # Only boxes, means and medians
        df.rankings_ <- filter(df.rankings_, size<1000)
        g.boxplots <- ggplot(df.rankings_, aes(x=z,y = ranking.error, fill=model)) +
          geom_boxplot(outlier.size=0, outlier.shape = NA, width=0.5) +
          #geom_boxplot(aes(x=z, y = ranking.error, fill=model, position = 'dodge'), 
          #             outlier.size=0.5, outlier.shape=3) +
          stat_summary(fun.y=mean, geom = 'point', aes(group=model), color = 'black',  
                       size=2, 
                       position=position_dodge(width=0.38)) +
          scale_fill_brewer(palette="OrRd") + 
          theme_bw() + theme(strip.background = element_rect(fill = 'white'), 
                             text = element_text(size=12),
                             legend.position = 'top',
                             legend.title = element_blank(),
                             legend.key = element_blank()) +
          guides(colour = guide_legend(override.aes = list(size=2)),
                 linetype = guide_legend(override.aes = list(linetype = 0))) +
          xlab("cluster") + ylab('Normalized Ranking Error')
        print(g.boxplots)
        ggsave(file=paste0('ch4_linkprediction_NRE_whiskers_', subforum_, '_k5.eps'), 
               width=210, height=75, units='mm')
        
        
        
        
        
        combined <- grid_arrange_shared_legend(g.hits, g.boxplots, ncol = 2, nrow = 1)
        ggsave(file=paste0('ch4_linkprediction_hits_whispers', subforum_, '_k5.eps'), combined, width=210, height=100, units='mm')
        
        gg <-arrangeGrob(g.hits,g.boxplots)
        grid.newpage()
        grid.draw(gg)
        
        
        # alternative 2 (means)
        df.rankings_ <- df.rankings
        df.rankings_$z <- as.factor(df.rankings_$z)
        g <- ggplot(filter(df.rankings, size<20000), aes(x=as.factor(z), 
                                                         y=ranking.error,
                                                         group=model,
                                                         color=model, 
                                                         shape= model, 
                                                         linetype=model)) +
          stat_summary_bin(fun.y= mean, geom='point', size=2) +
          stat_summary_bin(fun.y= mean, geom='line') +
          theme_bw() + theme(strip.background = element_rect(fill = 'white'), 
                             legend.position = 'top',
                             legend.title = element_blank(),
                             legend.key = element_blank()) +
                             #axis.text.x = element_blank(),) +
          guides(colour = guide_legend(override.aes = list(size=2)),
                 linetype = guide_legend(override.aes = list(linetype = 0))) +
          xlab("cluster") + ylab('Normalized Ranking Error')
        print(g)
        ggsave(file=paste0('ch4_linkprediction_NRE_meanlines_', subforum_, 'k5.eps'),
               width=100, height=75, units='mm')
        
        

>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  # RANK BY CHOSEN
  #############################################################################
  # This might be insteresting for Vicenc and Andreas
  # This shows that the tau model is the best when threads are long
  # That is that the tau factor is underestimated by the other models
  # NOT TRUE, the former graph contradicts this hypotesis:
  # tau is worse for long threads. Its just better when the choice is the
  # last post, of course. But this does not happen too often.
<<<<<<< HEAD
  
  # memory consuming
  g <- ggplot(filter(df.rankings, size<10), aes(x=chosen, y=ranking.error, group=model, 
                                                color=model, shape=model, linetype=model)) +
=======
        
  # memory consuming
  g <- ggplot(filter(df.rankings, size<10), aes(x=chosen, y=ranking.error, group=model, 
                               color=model, shape=model, linetype=model)) +
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
    stat_smooth(method='loess', geom='point')+
    #stat_summary(fun.y= mean, aes(group=model), geom='point', alpha=1, size=1.5) +
    #stat_summary(fun.y= mean, aes(group=model), geom='line', alpha=1, size=0.5) +
    geom_abline(intercept = 0, slope=1, linetype='dotted') +
    coord_fixed(ratio = 1, expand=FALSE) + 
    #facet_grid(. ~ subforum, scales = 'free') + 
    theme_bw() + theme(strip.background = element_rect(fill = 'white'), 
                       legend.title = element_blank(),
                       legend.key = element_blank(),
                       aspect.ratio=1) +
    guides(colour = guide_legend(override.aes = list(size=2)),
           linetype = guide_legend(override.aes = list(linetype = 0)))
  print(g)
<<<<<<< HEAD
  ggsave(file=paste0('snam_linkprediction_rank_by_chosen.png'), 
         width=200, height=70, units='mm')        
  
  }
=======
  ggsave(file=paste0('ch4_linkprediction_rank_by_chosen.png'), 
         width=200, height=70, units='mm')        
        
      

  
  # RANK BY TIME TO ROOT
  ##############################################################################
  g <- ggplot(filter(df.rankings, minutes<60), aes(x=minutes, y=rank, group=model, 
                                                   color=model, shape=model, linetype=model)) +
    stat_summary(fun.y= mean, aes(group=model), geom='point', alpha=1, size=1.5) +
    stat_summary(fun.y= mean, aes(group=model), geom='line', alpha=1, size=0.5) +
    facet_grid(. ~ subforum, scales = 'free') + 
    theme_bw() + theme(strip.background = element_rect(fill = 'white'), 
                       legend.title = element_blank(),
                       legend.key = element_blank(),
                       aspect.ratio=1) +
    guides(colour = guide_legend(override.aes = list(size=2)),
           linetype = guide_legend(override.aes = list(linetype = 0)))
  print(g)
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
}

