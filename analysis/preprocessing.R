################################################################################
# Creates a single big dataframe that can be directly used by the functions of 
# parameter estimation. The dataframe contains properties of the parent to which 
# each post replied. Use it once, save the results to a file and load that file 
# for the analysis instead of running this file each time
################################################################################
library(dplyr)
library(igraph)
library(doParallel)
library(foreach)
library(data.table)
source('R/datasets.R')

#### Set up clusters ###
if(detectCores() > 20)
{
  ncores <- 22 # mediamining server
} else {
  ncores <- 6 # local computer
}  


########################

subforums <- c("MachineLearning",
               "TwoXChromosomes",
               "france",  
               "podemos",
               "gameofthrones")

data.list <- list()
for(subforum in subforums){
  
  load(paste0('data/df.posts.', subforum, '.rda'))
  cat('\nsubforum:', subforum )
  
  # Add date in seconds from the root
  df.posts <- df.posts %>% 
    mutate(date=as.numeric(date)) %>% 
    mutate(date=date - min(date))
  
  # Add parent user
  parent.users <- df.posts[match(df.posts$parent, df.posts$postid),]$user
  df.posts <- df.posts %>% mutate(parent.user=parent.users)
  
  # Add thread size, user posts, user threads (in each post)
  df.posts <- df.posts %>% 
    group_by(thread) %>% mutate(thread.size = n()) %>% ungroup
  
  # User infos: number of post, threads, and ranking by activity
  df.users <- df.posts %>% 
    group_by(user) %>% 
    summarise(user.posts = n(), 
              user.threads = length(unique(thread))) %>%
    arrange(desc(user.posts), desc(user.threads), user) %>%
    mutate(user.rank = match(user, unique(user)))
  
  # Add user info to posts
  df.posts <- df.posts %>% left_join(df.users)
  
  # Create trees and then the final dataframes for each tree
  df.posts <- filter(df.posts, thread %in% unique(df.posts$thread))
  
  # Remove threads with only the root
  df.posts <- df.posts %>% filter(thread.size>1)
  
  threads <- unique(df.posts$thread)#[1:10]
  nthreads <- length(threads)
  
  cl = makeCluster(ncores, outfile="",  port=11439)
  registerDoParallel(cl)
  
  cat('\nprocessing trees')
  df.trees.list <- foreach(i = 1:length(threads),
                           .inorder = FALSE,
                           .packages = c('dplyr', 'igraph')) %dopar% {
     cat('\n', subforum, ':', i, ' / ', nthreads)
     th <- threads[i]
     df.thread <- df.posts %>% filter(thread==th)
     df.edges <- df.thread %>% filter(!is.na(parent.user)) %>% select(postid, parent)
     df.vertices <- df.thread %>% select(postid, user, date)
     gtree <- graph.data.frame(df.edges, vertices = df.vertices)
     tree_to_data(gtree, thread=th)
                           }
  df.trees <- rbindlist(df.trees.list)
  
  stopCluster(cl)
  
  
  # Add extra information to the post (about the user and subforum)
  # df.trees <- df.posts %>% rename(parentid = parent) %>% left_join(as.tbl(df.trees), .)
  
  df.posts <- df.posts %>% rename(parentid = parent)
  df.trees <- merge(df.trees, df.posts, by=c('postid', 'thread', 'user'), all.x=TRUE)
  df.trees <- df.trees %>% mutate(subforum=subforum)
  
  data.list[[ length(data.list) + 1 ]] <- df.trees
}

df.trees <- rbindlist(data.list)

# http://www.fromthebottomoftheheap.net/2012/04/01/saving-and-loading-r-objects/
saveRDS(df.trees, 'data/df.trees.rds')

if(FALSE){
  df.trees <- readRDS("data/df.trees.rds")
}
