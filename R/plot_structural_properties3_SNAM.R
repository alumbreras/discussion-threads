library(data.table)
library(igraph)
library(ggplot2)

tree_from_parents_vector <- function(parents){
  size <- length(parents)+1
  g <- graph.empty(n=size)
  edges <- t(rbind(2:size, parents))
  graph_from_edgelist(edges)
}


compare_degrees <- function(df.data){
  
  df.data$thread <- as.character(df.data$thread)
  df.data$subforum <- as.character(df.data$subforum)
  
  ncores <- detectCores() - 2
  cl <- makeCluster(ncores, outfile="", port=11439)
  registerDoParallel(cl)
  
  threads <- unique(df.data$thread)
  nthreads <- length(threads)
  
  i <- 1
  df.degrees.list <- foreach(i=1:nthreads, 
                             .packages=c('igraph', 'dplyr'), 
                             .export = 'tree_from_parents_vector') %do%{
                               th <- threads[i]
                               df.tree <- df.data %>% filter(thread == th)
                               subforum <- df.data$subforum[1]
                               cat('\nsubforum', subforum, i, '/', nthreads) 
                               
                               # Extract real and generated graphs 
                               g.real <- tree_from_parents_vector(df.tree$parent.real)
                               g.lumbreras <- tree_from_parents_vector(df.tree$parent.lumbreras)
                               g.gomez <- tree_from_parents_vector(df.tree$parent.gomez)
                               
                               # Compute metric
                               degrees.real = degree(g.real, mode='all')
                               degrees.lumbreras = degree(g.lumbreras, mode='all')
                               degrees.gomez = degree(g.gomez, mode='all')
                               
                               
                               data.frame(real = degrees.real,
                                          lumbreras = degrees.lumbreras,
                                          gomez = degrees.gomez,
                                          subforum = subforum)
                             }
  stopCluster(cl)
  df.degrees <- rbindlist(df.degrees.list)
  
  # Frequency by forum all with dplyr and withour table()
  df.degrees <- df.degrees %>% gather(model, degree, real, gomez, lumbreras)
  
  df.degrees.freqs <- df.degrees %>% 
    group_by(subforum, model, degree) %>% 
    summarise(freq = n()) %>%
    group_by(subforum, model) %>% 
    mutate(prob = freq / sum(freq)) %>%
    arrange(degree) %>%
    mutate(cumprob = cumsum(prob)) %>%
    ungroup
  
  
  g <- ggplot(df.degrees.freqs, aes(x=degree, y=cumprob, color=model, shape=model)) + 
    #scale_y_log10() + 
    scale_x_log10() +
    geom_point(size=1.5) +
    scale_colour_manual(values=c('blue', 'red', 'black')) +
    scale_shape_manual(values=c(19, 19, 1)) +
    scale_alpha_manual(values=c(1,1,0.1))+
    #geom_line(stat='smooth', method= 'lm', alpha=0.5) +
    guides(colour = guide_legend(override.aes = list(size=2))) +
    theme_bw() +
    theme(#strip.background = element_rect(fill = 'white'), 
      strip.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_blank(),
      #legend.position = "none",
      aspect.ratio=1) + ylab("probability") 
  #ggtitle('podemos')
  print(g) 
  ggsave(file=paste0('snam_degree_cumdistribution_', subforum, '.png'), 
         width=200, height=70, units='mm')
  g
}

#'
#' Compare subtree sizes distribution
#' 
compare_subtrees <- function(df.data){
  
  df.data$thread <- as.character(df.data$thread)
  df.data$subforum <- as.character(df.data$subforum)
  
  ncores <- detectCores() - 2
  cl <- makeCluster(ncores, outfile="", port=11439)
  registerDoParallel(cl)
  
  ####################################
  # Degree distribution
  ####################################
  threads <- unique(df.data$thread)
  nthreads <- length(threads)
  
  i <- 1
  df.sizes.list <- foreach(i=1:nthreads,.packages=c('igraph', 'dplyr'), 
                           .export = 'tree_from_parents_vector') %do%{
                             th <- threads[i]
                             df.tree <- df.data %>% filter(thread == th)
                             subforum <- df.data$subforum[1]
                             cat('\nsubforum', subforum, i, '/', nthreads) 
                             
                             # Extract real and generated graphs 
                             g.real <- tree_from_parents_vector(df.tree$parent.real)
                             g.gomez <- tree_from_parents_vector(df.tree$parent.gomez)
                             g.lumbreras <- tree_from_parents_vector(df.tree$parent.lumbreras)
                             
                             subtrees.real <- ego_size(g.real, order=1000, mode='in', mindist=1)
                             subtrees.gomez <- ego_size(g.gomez, order=1000, mode='in', mindist=1)
                             subtrees.lumbreras <- ego_size(g.lumbreras, order=1000, mode='in', mindist=1)
                             
                             
                             data.frame(real = subtrees.real,
                                        gomez = subtrees.gomez,
                                        lumbreras = subtrees.lumbreras,
                                        subforum = subforum)
                           }
  stopCluster(cl)
  df.sizes <- rbindlist(df.sizes.list)
  df.sizes <- df.sizes %>% gather(model, size, real, gomez, lumbreras)
  
  # Frequency by forum all with dplyr and withour table()
  df.sizes.freqs <- df.sizes %>% 
    group_by(subforum, model, size) %>% 
    summarise(freq = n()) %>%
    group_by(subforum, model) %>% 
    mutate(prob = freq / sum(freq)) %>% 
    arrange(size) %>%
    mutate(cumprob = cumsum(prob)) %>%
    ungroup
  
  g <- ggplot(df.sizes.freqs, aes(x=size, y=cumprob, color=model, shape=model)) + 
    scale_y_log10() + scale_x_log10() +
    geom_point(size=1.5) +
    scale_colour_manual(values=c('blue', 'red', 'black')) +
    scale_shape_manual(values=c(19, 19, 1)) +
    scale_alpha_manual(values=c(1,1,0.1))+
    guides(colour = guide_legend(override.aes = list(size=2))) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'), 
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          aspect.ratio=1) + ylab("probability") + xlab('subtree size')
  print(g)
  ggsave(file=paste0('snam_subtreesize_cumdistribution_', subforum, '.png'), 
         width=200, height=70, units='mm')
  g
}

compare_size_depth <- function(df.data){
  
  df.data$thread <- as.character(df.data$thread)
  df.data$subforum <- as.character(df.data$subforum)
  
  ncores <- detectCores() - 2
  cl <- makeCluster(ncores, outfile="", port=11439)
  registerDoParallel(cl)
  
  ####################################
  # Degree distribution
  ####################################
  threads <- unique(df.data$thread)
  nthreads <- length(threads)
  
  i <- 1
  df.depths.list <- foreach(i=1:nthreads, .packages=c('igraph', 'dplyr'),
                            .export='tree_from_parents_vector') %dopar%{
                              
                              th <- threads[i]
                              df.tree <- df.data %>% filter(thread == th)
                              subforum <- df.data$subforum[1]
                              cat('\nsubforum', subforum, i, '/', nthreads) 
                              
                              # Extract real and generated graphs 
                              g.real <- tree_from_parents_vector(df.tree$parent.real)
                              g.gomez <- tree_from_parents_vector(df.tree$parent.gomez)
                              g.lumbreras <- tree_from_parents_vector(df.tree$parent.lumbreras)
                              
                              depth.real <- diameter(g.real)
                              depth.gomez <- diameter(g.gomez)
                              depth.lumbreras <- diameter(g.lumbreras)
                              
                              
                              data.frame(real = depth.real,
                                         lumbreras = depth.lumbreras,
                                         gomez = depth.gomez,
                                         size = vcount(g.real), # all trees have the same size
                                         thread=th,
                                         subforum = subforum)
                            }
  stopCluster(cl)
  df.depths <- rbindlist(df.depths.list)
  
  df.depths <- df.depths %>% 
    gather(model, depth, real, gomez, lumbreras)
  
  g <- ggplot(df.depths, aes(x=size, y=depth, color=model, shape=model)) + 
    scale_y_log10() + 
    scale_x_log10() +
    stat_summary(fun.y= mean, aes(group=model), geom='point', alpha=1, size=1.5) +
    scale_colour_manual(values=c('blue', 'red', 'black')) +
    scale_shape_manual(values=c(19, 19, 1)) +
    #geom_line(stat='smooth', method= 'lm', alpha=0.5) +
    guides(colour = guide_legend(override.aes = list(size=2))) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'), 
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          aspect.ratio=1) + ylab("mean depth")
  print(g)
  ggsave(file=paste0('snam_depth_by_size_', subforum, '.png'), 
         width=200, height=70, units='mm')
  g
}
