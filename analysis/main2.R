# Main script (newest, end August)
# Compute parameters of all the models,
# compute their likelihood and their raking precission
# Compares Gomez 2013 and Lumbreras 2016 on a reddit forum
# author: Alberto Lumbreras
################################################################################
<<<<<<< HEAD
#set.seed(1) # Remove this in the final experiments

# remove deleted posts and bots
deleted <- c('AutoModerator', '[deleted]', 'piedbot', 'arXiv_landing_bot')
=======
source('R/datasets.R')
source('R/estimators.R')
source('R/extract_from_db.R')
source('R/likelihood.R')
source('R/plot_structural_properties2.R')
source('R/plotting.R')
source('R/thread_generators.R')
source('R/link_prediction2.R')
library(data.table)

#set.seed(1) # Remove this in the final experiments


# remove deleted posts and bots
deleted <- c('AutoModerator', '[deleted]', 'piedbot', 'arXiv_landing_bot')

>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
# or remove just deleted posts, and have fun with robots
# first results show that piedbot (france bot) is has alpha, beta, tau --> 0
deleted <- c('[deleted]') 

################################################################################
# Prepare the data
################################################################################
df.trees <- readRDS("data/df.trees.rds") %>%
  group_by(user) %>% mutate(user.posts.ok = n()) %>% 
  filter(user.rank <= 1000, user.posts.ok > 100) %>%  # just in case
  filter(t>1) %>%                   # the first post (after the root) is trivial
  filter(! user %in% deleted) %>%   # remove deleted and bots
  mutate(userint = user.rank)       # identify each user by its rank

# Mark training, validation, test 50:25
df.trees <- df.trees[sample(nrow(df.trees)), ]
df.trees <- df.trees %>% 
  group_by(user) %>% 
<<<<<<< HEAD
  mutate(split = ifelse(row_number() <= 0.5*n(), 
                        "train", 
                        ifelse(row_number() <= 0.75*n(), "validation", "test"))) %>%
=======
    mutate(split = ifelse(row_number() <= 0.5*n(), 
                    "train", 
                    ifelse(row_number() <= 0.75*n(), "validation", "test"))) %>%
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  ungroup

# Users and their sizes of training, validation and test for each user
df.users <- df.trees %>% 
  group_by(user) %>% 
<<<<<<< HEAD
  summarise(
    ntrain = sum(split=='train'), 
    nvalidation = sum(split=='validation'),
    ntest = sum(split=='test'), 
    total=n()) %>% 
=======
    summarise(
      ntrain = sum(split=='train'), 
      nvalidation = sum(split=='validation'),
      ntest = sum(split=='test'), 
      total=n()) %>% 
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  arrange(desc(total), desc(ntrain), desc(ntest)) %>%
  as.data.frame

# Info: how many users in each subforum
df.trees %>% group_by(subforum) %>% summarise(nusers = length(unique(user)))

################################################################################
# SEPARATED ANALYSIS FOR EVERY FORUM
################################################################################
<<<<<<< HEAD
=======

>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
subforums <- "MachineLearning"
subforum <- "TwoXChromosomes"
subforum <- "france"
subforum <- "podemos"
<<<<<<< HEAD

# pick one
subforum <- "gameofthrones"
subforum_ <- "gameofthrones"

subforum_ <- "podemos"
subforum <- "podemos"


df.trees.all <- filter(df.trees, subforum==subforum_)
=======
subforum <- "gameofthrones"

# pick one
subforum_ <- "podemos"
df.trees.all <- filter(df.trees, subforum==subforum_)
                #select(user, t, popularity, parent, lag, 
                #  grandparent, grandparents.candidates, 
                #  postid, thread, subforum)
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
df.trees.train      <- df.trees.all %>% filter(split == 'train')
df.trees.validation <- df.trees.all %>% filter(split == 'validation')
df.trees.test       <- df.trees.all %>% filter(split == 'test')

# a data frame with all but those posts marked for validation and test
# note: ideally, validation should be setect from all users, not only from the most active
# to avoid bias towards the active

################################################################################
# Estimate parameters
################################################################################

# For Gomez, do not filter by active user, use them all!
# df.trees.allusers <- readRDS("data/df.trees.rds") %>% filter(t>1) %>%
#                       select(user, t, popularity, parent, lag, 
#                              grandparent, grandparents.candidates, 
#                              postid, thread, subforum) %>%
#                       filter(subforum == subforum_)
df.trees.allusers <- readRDS("data/df.trees.rds") %>% 
  filter(t>1) %>% 
  filter(subforum == subforum_)

# GOMEZ
################################################################################
# Initialisation doesn't affect too much
# The most critical is the sample size
# With 100,000 it is quite stable

#df.trees.allusers.sample <- df.trees.allusers[sample(500000),]
#params.gomez <- estimation_Gomez2013(df.trees.allusers.sample, params)

# Run for different initialisations and get the best
packages <- c('dplyr', 'dfoptim')

cl <- makeCluster(ncores, outfile="", port=11439)
registerDoParallel(cl)
res <- foreach(i=1:5, .packages = packages, .inorder=FALSE) %dopar% {
  params <- list(alpha = runif(1,0,10),
                 beta = runif(1,0,20),
                 tau = runif(1))
<<<<<<< HEAD
=======
  #estimation_Gomez2013(df.trees.allusers, params)
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  estimation_Gomez2013(df.trees.train, params)
}
stopCluster(cl)
params.gomez <- res[[which.max(sapply(res, function(x) x$likelihood))]]


# GOMEZ PLUS
# It has a new factor (is a post its own grandchild?)
################################################################################
cl <- makeCluster(ncores, outfile="", port=11439)
registerDoParallel(cl)
res <- foreach(i=1:5, .packages = packages, .inorder=FALSE) %dopar% {
  params <- list(alpha = runif(1,0,2),
                 beta = runif(1,0,10),
                 tau = runif(1),
                 gamma = runif(1,0,10))
<<<<<<< HEAD
  estimation_Gomez2013plus(df.trees.train, params)
=======
  estimation_Gomez2013plus(df.trees.allusers, params)
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
}
stopCluster(cl)
params.gomezplus <- res[[which.max(sapply(res, function(x) x$likelihood))]]

# LUMBRERAS (k=1,...N)
################################################################################
params.lumbreras.list <- list()
<<<<<<< HEAD

for (k in 1:50){
    
=======
for (k in 10){
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  alphas <- runif(k)*2
  betas <- runif(k)*2 #*100
  taus <- runif(k)
  #gammas <- runif(k)
  params <- list(alpha=alphas, beta=betas, tau=taus)
  res <- estimation_Lumbreras2016(df.trees.train, params, niters=10)
  params.lumbreras.list[[k]] <- res
}

# debug
# res <- estimation_Lumbreras2016(head(df.trees.train,100), params, niters=3)

# Save / Load parameters
# We also store the data because it has the marked train and test posts
#######################################################################
if(FALSE){
  saveRDS(params.gomez, 
          file = paste0('data/params.gomez.', subforum_, '.rds'))
  saveRDS(params.gomezplus, 
          file = paste0('data/params.gomezplus.', subforum_, '.rds'))
  saveRDS(params.lumbreras.list, 
          file=paste0('data/params.lumbreras.list.', subforum_, '.rds'))
  
  saveRDS(df.trees.all, 
          file=paste0('data/df.tree.all.', subforum_, '.data.rds')) 
  
  params.gomez          <- readRDS(paste0('data/params.gomez.', subforum_, '.rds'))
  params.gomezplus      <- readRDS(paste0('data/params.gomezplus.', subforum_, '.rds'))
  params.lumbreras.list <- readRDS(paste0('data/params.lumbreras.list.', subforum_, '.rds'))
  df.trees.all <- readRDS(paste0('data/df.tree.all.', subforum_, '.data.rds'))
<<<<<<< HEAD
  
=======

>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  # Get ready to work !
  df.trees.train      <- df.trees.all %>% filter(split == 'train')
  df.trees.validation <- df.trees.all %>% filter(split == 'validation')
  df.trees.test       <- df.trees.all %>% filter(split == 'test')
  
}

# Choose number of clusters (in validation set)
##################################################
# Select number of clusters in validation set (estimation of predictive power)
likelihoods.lumbreras.k <- vector()
for(k in 1:length(params.lumbreras.list)){
  params <-  params.lumbreras.list[[k]]
<<<<<<< HEAD
  if(is.null(params)) next
  likelihoods.lumbreras.k[k] <- likelihood_Lumbreras2016(df.trees.validation, 
=======
  likelihoods.lumbreras.k[k] <- likelihood_Lumbreras2016(df.trees.train, 
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
                                                         params, 
                                                         params$responsibilities, 
                                                         params$pis)
  
}
#If the ratio / n_points / n_parameters in < 40 then use AICc

#AIC =  - 2loglike + 2k
AIC.lumbreras.k <- sapply(1:length(likelihoods.lumbreras.k), 
<<<<<<< HEAD
                          function(k) {
                            -2*likelihoods.lumbreras.k[k] + 2*(3*k +(k-1)) 
                          })
=======
                            function(k) {
                              -2*likelihoods.lumbreras.k[k] + 2*(3*k +(k-1)) 
                            })
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
#BIC = -2loglike + log(n)k
n <- nrow(df.trees.train)
BIC.lumbreras.k <- sapply(1:length(likelihoods.lumbreras.k), 
                          function(k) {
                            -2*likelihoods.lumbreras.k[k] + log(n)*(3*k +(k-1))
                          })

plot(likelihoods.lumbreras.k)
plot(AIC.lumbreras.k) # Use this because penalises complexity
plot(BIC.lumbreras.k) # Use this because penalises complexity

# And with the stored probabilities in training
# like every EM library does
likelihoods <- sapply(params.lumbreras.list, function(x) max(x$likelihoods,na.rm=TRUE))
likelihoods.Q <- sapply(params.lumbreras.list, function(x) max(x$likelihoods.Q, na.rm=TRUE))

AIC.lumbreras.k <- sapply(1:length(likelihoods.Q), 
                          function(k) {
                            -2*likelihoods.Q[k] + 2*(3*k +(k-1)) 
                          })
BIC.lumbreras.k <- sapply(1:length(likelihoods.Q), 
                          function(k) {
                            -2*likelihoods.Q[k] + log(n)*(3*k +(k-1))
                          })
plot(AIC.lumbreras.k) # Use this because penalises complexity
<<<<<<< HEAD
plot(BIC.lumbreras.k, xlab="k", ylab='BIC') # Use this because penalises complexity

df.BIC <- data.frame(k=1:length(BIC.lumbreras.k), BIC = BIC.lumbreras.k)

df.BIC <- filter(df.BIC, BIC!=Inf)
df.BIC$delta <- df.BIC$BIC - min(df.BIC$BIC)
order(rank(df.BIC$delta)) # check it without the plot
ggplot(filter(df.BIC, !is.na(BIC)), aes(x=k, y=BIC)) +
  geom_point(size=1) +
  geom_line() + 
  theme_bw() + theme(text = element_text(size=14)) #+ ylim(0)
ggsave(file=paste0('ch4_linkprediction_BIC', subforum_, '.eps'), 
       width=125, height=70, units='mm')
k <- which.min(AIC.lumbreras.k)
k <- which.min(BIC.lumbreras.k)

k <- 15 # Choose it from visual inspection on the plot


=======
plot(BIC.lumbreras.k) # Use this because penalises complexity

k <- which.min(AIC.lumbreras.k)
k <- which.min(BIC.lumbreras.k)

k <- 5 # Choose it from visual inspection on the plot
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad


# Visualise likelihoods in training and test with k=1,...N
################################################################################
# Plot them, and plot also Gomez likelihood
likelihoods.train <- sapply(1:length(params.lumbreras.list), 
                            function(i) {
                              likelihood_Lumbreras2016(df.trees.train, 
                                                       params.lumbreras.list[[i]], 
                                                       params.lumbreras.list[[i]]$responsibilities, 
                                                       params.lumbreras.list[[i]]$pis)
                            }
)


likelihoods.validation <- sapply(1:length(params.lumbreras.list), 
<<<<<<< HEAD
                                 function(i) {
                                   likelihood_Lumbreras2016(df.trees.validation, 
                                                            params.lumbreras.list[[i]], 
                                                            params.lumbreras.list[[i]]$responsibilities, 
                                                            params.lumbreras.list[[i]]$pis)
                                 }
)


likelihoods.test <- sapply(1:length(params.lumbreras.list), 
                           function(i) {
                             likelihood_Lumbreras2016(df.trees.test, 
=======
                           function(i) {
                             likelihood_Lumbreras2016(df.trees.validation, 
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
                                                      params.lumbreras.list[[i]], 
                                                      params.lumbreras.list[[i]]$responsibilities, 
                                                      params.lumbreras.list[[i]]$pis)
                           }
)

<<<<<<< HEAD
=======

likelihoods.test <- sapply(1:length(params.lumbreras.list), 
                           function(i) {
                              likelihood_Lumbreras2016(df.trees.test, 
                                params.lumbreras.list[[i]], 
                                params.lumbreras.list[[i]]$responsibilities, 
                                params.lumbreras.list[[i]]$pis)
                            }
)

>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
# just to compare as baseline
likelihood.gomez.train <- likelihood_Gomez2013(df.trees.train, params.gomez)
likelihood.gomez.validation <- likelihood_Gomez2013(df.trees.validation, params.gomez)
likelihood.gomez.test <- likelihood_Gomez2013(df.trees.test, params.gomez)

df.likelihoods <- data.frame(k= 1:length(params.lumbreras.list), 
                             validation = likelihoods.validation/nrow(df.trees.validation),
                             train = likelihoods.train/nrow(df.trees.train),
                             test = likelihoods.test/nrow(df.trees.test))

df.likelihoods <- gather(df.likelihoods, split, like, -k)

ggplot(df.likelihoods, aes(x=k, y=like, group=split, color=split)) + 
  geom_point() + geom_line() + 
  theme_bw()

# AND THE WINNER IS....
<<<<<<< HEAD
k <- 20
k <- 15
params.lumbreras <- params.lumbreras.list[[k]]
params.lumbreras$z <- as.vector(apply(params.lumbreras$responsibilities, 1, which.max))


nusers <- as.vector(table(apply(params.lumbreras$responsibilities, 1, function(x) which.max(x))))
cbind(params.lumbreras$alphas, params.lumbreras$betas, params.lumbreras$taus, params.lumbreras$pis, nusers)

=======
k <- 5
k <- 10
params.lumbreras <- params.lumbreras.list[[k]]
params.lumbreras$z <- as.vector(apply(params.lumbreras$responsibilities, 1, which.max))

>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
# Info clusters
df.clusters <- data.frame(user = rownames(params.lumbreras$responsibilities), z = params.lumbreras$z)
df.clusters <- merge(df.clusters, df.users)
df.clusters <- df.clusters %>% select(z, total)
# How many posts in training test for each group?
ggplot(df.clusters, aes(x=as.factor(z), y=total)) + 
  geom_point(size=0.5) +
  stat_summary(fun.y=mean, colour="red", geom="point", size=3, shape=19)+
  theme_bw() + 
  theme(text = element_text(size=14)) +
  xlab("cluster") + ylab('posts') 
<<<<<<< HEAD
ggsave(file=paste0('ch4_', subforum, '_cluster_posts.eps'), width=100, height=75, units='mm')

=======
  ggsave(file=paste0('ch4_podemos_cluster_posts.eps'), width=100, height=70, units='mm')
  
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
# If you use a subset of the dataset, do this again
#users <- names(sort(table(df.trees$user), decreasing = TRUE))
#df.trees$userint <- match(df.trees$user, users)
#trees <- lapply(trees, function(t) {V(t)$userint <- match(V(t)$user, users);t})

cat('\n', params.gomez)
cat('\n alphas: ', params.lumbreras$alphas)
cat('\n betas: ', params.lumbreras$betas)
cat('\n taus: ', params.lumbreras$taus)
plot(rowSums(params.lumbreras$traces), type='b')
title('\n Likelihood')


################################################################################
# Generate synthetic trees
################################################################################
# Ideally, trees in test and not in training to avoid pollution
# Instead, I'm using all trees where active users participated

# Get the full trees where our users participated
df.trees_ <- readRDS("data/df.trees.rds")
df.trees.active <-  df.trees_ %>% 
<<<<<<< HEAD
  filter(subforum == subforum_) %>%
  group_by(thread) %>% 
  filter(any(user %in% df.users$user)) %>%
  ungroup
=======
                    filter(subforum == subforum_) %>%
                    group_by(thread) %>% 
                      filter(any(user %in% df.users$user)) %>%
                    ungroup
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad

# Recover the line of the responsability matrix for every user
# NA if user has not been clustered (most of them!)
# store it in each post row for later
df.trees.active$userint <- match(df.trees.active$user,rownames(params.lumbreras$responsibilities))
z <- as.vector(apply(params.lumbreras$responsibilities, 1, which.max))
df.trees.active$z <- z[df.trees.active$userint]

threads <- df.trees.active %>% distinct(thread) %>% unlist %>% as.vector
threads <- sample(threads,10000)
nthreads <- length(threads)

cl <- makeCluster(ncores, outfile="", port=11439)
registerDoParallel(cl)

data.list <- foreach(i = 1:nthreads, .packages = c('igraph','dplyr')) %dopar% {
  th <- threads[i]
  cat('\nsubforum', i, '/', nthreads) 
  df.tree <- df.trees.active %>%  filter(thread == th)
  #prof <- profvis({
<<<<<<< HEAD
  
  
  # Avoid threads of size < 2 since there is no randomness there
  if (nrow(df.tree) < 2) {
    res <- data.frame(thread=th,
                      t = NA,
                      parent.real = NA,
                      parent.gomez = NA,
                      parent.lumbreras = NA,
                      parent.gomezplus = NA,
                      subforum=subforum_)  
    return(res)
  }
=======
    
    
  # Avoid threads of size < 2 since there is no randomness there
  if (nrow(df.tree) < 2) {
    res <- data.frame(thread=th,
                       t = NA,
                       parent.real = NA,
                       parent.gomez = NA,
                       parent.lumbreras = NA,
                       parent.gomezplus = NA,
                       subforum=subforum_)  
    return(res)
    }
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  users <- df.tree$user
  # hack to add the root user, the grandpas factor needs it
  users.all <- c(df.tree$parent.user[1], users) 
  
  if(TRUE){
<<<<<<< HEAD
    # For Lumbreras, pre-compute the parameters of each user accoring to its cluster
    alphas <- params.lumbreras$alpha[df.tree$z]
    alphas[is.na(alphas)] <- params.gomez$alpha # if no cluster, use Gomez
    
    betas <- params.lumbreras$beta[df.tree$z]
    betas[is.na(betas)] <- params.gomez$beta # if no cluster, use Gomez
    
    taus <- params.lumbreras$tau[df.tree$z]
    taus[is.na(taus)] <- params.gomez$tau # if no cluster, use Gomez
    
    #gammas <- params.lumbreras$gamma[df.tree$z]
    #gammas[is.na(gammas)] <- params.gomezplus$gamma # if no cluster, use Gomez
=======
  # For Lumbreras, pre-compute the parameters of each user accoring to its cluster
  alphas <- params.lumbreras$alpha[df.tree$z]
  alphas[is.na(alphas)] <- params.gomez$alpha # if no cluster, use Gomez
  
  betas <- params.lumbreras$beta[df.tree$z]
  betas[is.na(betas)] <- params.gomez$beta # if no cluster, use Gomez
  
  taus <- params.lumbreras$tau[df.tree$z]
  taus[is.na(taus)] <- params.gomez$tau # if no cluster, use Gomez
  
  #gammas <- params.lumbreras$gamma[df.tree$z]
  #gammas[is.na(gammas)] <- params.gomezplus$gamma # if no cluster, use Gomez
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  }
  
  # Simulate the trees and store only their parent tree 
  # (occupies less memory and it is exavtmy the same structural info)
  
  # Real graph
  parents <- df.tree %>% arrange(t) %>% select(parent) %>% unlist %>% as.vector
  parents.real <- parents
  
  # Gomez (everyone the same parameters)
  g.gomez <- gen.thread.Gomez2013(nrow(df.tree)+1, alpha =params.gomez$alpha, 
                                  beta= params.gomez$beta, 
                                  tau = params.gomez$tau)
  parents.gomez <- get.edgelist(g.gomez, names=FALSE)[,2]
  
  
  
  # Lumbreras
  g.lumbreras <- gen.thread.Lumbreras2016(users.all, alphas, betas, taus)
  parents.lumbreras <- get.edgelist(g.lumbreras, names=FALSE)[,2]
  
  # Gomez plus
  g.gomezplus <- gen.thread.Gomez2013plus(users.all, 
<<<<<<< HEAD
                                          alpha =params.gomezplus$alpha, 
                                          beta= params.gomezplus$beta, 
                                          tau = params.gomezplus$tau,
                                          gamma = params.gomezplus$gamma)
=======
                                  alpha =params.gomezplus$alpha, 
                                  beta= params.gomezplus$beta, 
                                  tau = params.gomezplus$tau,
                                  gamma = params.gomezplus$gamma)
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
  parents.gomezplus <- get.edgelist(g.gomezplus, names=FALSE)[,2]
  
  
  data.frame(thread=th,
             t = 1:length(parents),
             parent.real = parents.real,
             parent.gomez = parents.gomez,
             parent.lumbreras = parents.lumbreras,
             parent.gomezplus = parents.gomezplus,
             subforum=subforum_)
  
  #}) # end profvis
}
stopCluster(cl)
df.synthetic.trees <- rbindlist(data.list, fill=TRUE)
df.synthetic.trees <- df.synthetic.trees %>% filter(complete.cases(.))



if(FALSE){
<<<<<<< HEAD
  k <- length(params.lumbreras$alpha)
  filename <- paste0('data/generated.trees.', subforum_, '_k_', k, '.rda')
  saveRDS(df.synthetic.trees, file=filename)
  df.synthetic.trees <- readRDS(filename)
  
  # We can merge all the trees in all the forums and work in the same dataframe
  # df.synthetic.trees <- rbindlist(list(df.gen.podemos, df.gen.gameofthrones, df.gen.france))
=======
k <- length(params.lumbreras$alpha)
filename <- paste0('data/generated.trees.', subforum_, '_k_', k, '.rda')
saveRDS(df.synthetic.trees, file=filename)
df.synthetic.trees <- readRDS(filename)

# We can merge all the trees in all the forums and work in the same dataframe
# df.synthetic.trees <- rbindlist(list(df.gen.podemos, df.gen.gameofthrones, df.gen.france))
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
}

# Compare genetared threads (plot degree distributions, etc)
################################################################################
source('R/hadley.R')

g1 <- compare_degrees(df.synthetic.trees)
g2 <- compare_subtrees(df.synthetic.trees)
g3 <- compare_size_depth(df.synthetic.trees)

g11 <- g1 + theme(aspect.ratio=1.15)  + scale_shape_manual(values=c(10, 19, 1)) #plot.margin=unit(c(0,0,0,0), "lines"))
g22 <- g2 + theme(aspect.ratio=1.15)  + scale_shape_manual(values=c(10, 19, 1)) 
g33 <- g3 + theme(aspect.ratio=1.07)  + scale_shape_manual(values=c(10, 19, 1)) 
# plot together using one legend
combined <- grid_arrange_shared_legend(g11, g22, g33, ncol = 3, nrow = 1)
ggsave(file=paste0('ch4_structures_', subforum_, '_k5.png'), combined, width=210, height=100, units='mm')

gg <-arrangeGrob(g11,g22,g33)
grid.newpage()
grid.draw(gg)
ggsave(file=paste0('ch4_structures_degrees_', subforum_, '_k_', k, '.png'), g11, 
       width=200, height=70, units='mm', dpi=450)


################################################################################
# Compare post recommendations
################################################################################
df.trees_ <- readRDS("data/df.trees.rds")
df.trees.active <-  df.trees_ %>% 
  filter(subforum == subforum_) %>%
  group_by(thread) %>% 
  filter(any(user %in% df.users$user)) %>%
  ungroup

# Recover the line of the responsability matrix for every user
# NA if user has not been clustered (most of them!)
# store it in each post row for later
df.trees.active$userint <- match(df.trees.active$user,rownames(params.lumbreras$responsibilities))
z <- as.vector(apply(params.lumbreras$responsibilities, 1, which.max))
df.trees.active$z <- z[df.trees.active$userint]


# Threads with some test posts
# get their training/tets labels, if any (just to check)
df.trees.active.test <- df.trees.active %>% filter(thread %in% df.trees.test$thread)
df.trees.active.test$split <- ifelse(df.trees.active.test$postid %in% 
<<<<<<< HEAD
                                       df.trees.test$postid, 
                                     'test', 'none')
df.trees.active.test$split <- ifelse(df.trees.active.test$postid %in% 
                                       df.trees.train$postid, 
                                     'train', df.trees.active.test$split)
=======
                                     df.trees.test$postid, 
                                        'test', 'none')
df.trees.active.test$split <- ifelse(df.trees.active.test$postid %in% 
                                     df.trees.train$postid, 
                                        'train', df.trees.active.test$split)
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad

threads <- unique(df.trees.active.test$thread)
nthreads <- length(threads)
# Podemos: 42996 threads

cl <- makeCluster(ncores, outfile="", port=11439)
registerDoParallel(cl)
df.preds.list <- foreach(i = 1:nthreads, 
                         .packages = c('igraph','dplyr', 'data.table'),
                         .export = 'compare_link_prediction2') %dopar% {
<<<<<<< HEAD
                           th <- threads[i]
                           cat('\nsubforum', i, '/', nthreads) 
                           df.tree <- df.trees.active.test %>%  filter(thread == th)
                           
                           # Avoid threads of size < 2 since there is no randomness there
                           if (nrow(df.tree) < 2) {
                             res <- data.frame(thread=th,
                                               t = NA,
                                               parent.real = NA,
                                               parent.gomez = NA,
                                               parent.lumbreras = NA,
                                               parent.gomezplus = NA,
                                               subforum=subforum_)  
                             return(res)
                           }
                           
                           df.tree <- df.tree %>% select(t, post, parent, date, user, parent.user, z, split, subforum)
                           compare_link_prediction2(df.tree, params.lumbreras, params.gomez, params.gomezplus)
                         }
=======
  th <- threads[i]
  cat('\nsubforum', i, '/', nthreads) 
  df.tree <- df.trees.active.test %>%  filter(thread == th)
  
  # Avoid threads of size < 2 since there is no randomness there
  if (nrow(df.tree) < 2) {
    res <- data.frame(thread=th,
                      t = NA,
                      parent.real = NA,
                      parent.gomez = NA,
                      parent.lumbreras = NA,
                      parent.gomezplus = NA,
                      subforum=subforum_)  
    return(res)
  }

  df.tree <- df.tree %>% select(t, post, parent, date, user, parent.user, z, split, subforum)
  compare_link_prediction2(df.tree, params.lumbreras, params.gomez, params.gomezplus)
}
>>>>>>> 77f2dccc581a2e305d374991d500b386bf49b3ad
stopCluster(cl)
df.preds <- rbindlist(df.preds.list)

if(FALSE){
  filename <- paste0('data/df.preds.', subforum_, '_k_', k, '.rda')
  save(df.preds, file=filename)
  load(filename)
}

plot_ranking_benchmarks2(df.preds)

