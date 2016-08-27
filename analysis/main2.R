# Main script (newest, end August)
# Compute parameters of all the models,
# compute their likelihood and their raking precission

# Compares Gomez 2013 and Lumbreras 2016 on a reddit forum
# author: Alberto Lumbreras
################################################################################
source('R/datasets.R')
source('R/estimators.R')
source('R/extract_from_db.R')
source('R/likelihood.R')
source('R/plot_structural_properties.R')
source('R/plotting.R')
source('R/thread_generators.R')
source('R/link_prediction.R')

# Prepare the data
#######################################
df.trees <- readRDS("data/df.trees.rds")
df.trees <- df.trees %>% filter(user.rank <= 1000)
df.trees <- df.trees %>% filter(t>1) # t==1 is trivial, is always a reply to the root

# mark as training,  validation and test 60/20/20
# or mark fold 1, fold 2...
df.trees <- df.trees[sample(nrow(df.trees)), ]
df.trees <- df.trees %>% 
  group_by(user) %>% 
    mutate(split = ifelse(row_number() <= 0.5*n(), "train", "test")) %>%
  ungroup

# We'll identify each user by its rank
df.trees <- df.trees %>% mutate(userint = user.rank)
df.trees.train <- df.trees %>% filter(split == 'train') %>% select(userint, t, popularity, parent, lag)
df.trees.test <- df.trees %>% filter(split == 'test') %>% select(userint, t, popularity, parent, lag)

# Remove the non-numeric rows to avoid issues when using apply
# over rows

# Estimate 
#####################################
# Estimate parameters for Gomez 2013
params <- list(alpha = runif(1),
                    beta = runif(1),
                    tau = runif(1))
params.gomez <- estimation_Gomez2013(head(df.trees.train,10000), params)


# Estimate parameters for Lumbreras for k=2,..,10
params.lumbreras.list <- list()
for (k in 1:5){
  alphas <- runif(k)*2
  betas <- runif(k)*2 #*100
  taus <- runif(k)
  params <- list(alpha=alphas, beta=betas, tau=taus)
  #params.lumbreras.list[[k]] <- estimation_Lumbreras2016(head(df.trees.train,1000), params)
  params.lumbreras.list[[k]] <- estimation_Lumbreras2016(df.trees.train, params)
}

# Select number of clusters in validation set (estimation of predictive power)
likelihoods.lumbreras.k <- vector()
df.trees.test <- filter(df.trees.test, !is.na(userint))
for(k in 1:5){
  params.lumbreras <-  params.lumbreras.list[[k]]
  likelihoods.lumbreras.k[k] <- likelihood_Lumbreras2016(df.trees.test, params.lumbreras, params.lumbreras$responsabilities, params.lumbreras$pis)
}
plot(likelihoods.lumbreras.k)

k <- 3
params.lumbreras$z <- as.vector(apply(params.lumbreras$responsabilities, 1, which.max))

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

if(FALSE){
  save(params.gomez, file=paste0('data/params.gomez.', subforum, '.rda'))
  save(params.lumbreras, file=paste0('data/params.lumbreras.', subforum, '.rda'))
  load(paste0('data/params.gomez.', subforum, '.rda'))
  load(paste0('data/params.lumbreras.', subforum, '.rda'))
}

################################################################################
# Compare likelihood Gomez2013 and Lumbreras2016 over the trees
################################################################################
likelihood.gomez <- likelihood_Gomez2013(df.trees.test, params.gomez)
likelihood.lumbreras <- likelihood_Lumbreras2016(df.trees.test, params.lumbreras, params.lumbreras$responsabilities, params.lumbreras$pis)
likelihood.lumbreras.hard <- likelihood_Lumbreras2016_hard(df.trees.test, params.lumbreras, params.lumbreras$responsabilities)
cat("\nlikelihood Gomez: ", likelihood.gomez)
cat("\nlikelihood Lumbreras:", likelihood.lumbreras)
cat("\nlikelihood Lumbreras.hard:", likelihood.lumbreras.hard[1,1])


# Generate artificial trees under  Gomez 2013 and Lumbreras2016 reproducing the real sizes
# in Lumbreras2016, authors are chosen uniformly
sizes <- sapply(trees, vcount)
trees.gomez <- generate_trees(model='Gomez2013', params=params.gomez, sizes=sizes)
trees.lumbreras <- generate_trees(model='Lumbreras2016', params=params.lumbreras, 
                                  sizes=sizes, user.sample=df.trees.train$userint)

if(FALSE){
  save(trees.gomez, file = paste0('data/trees.gomez.',subforum, '.rda'))
  save(trees.lumbreras, file = paste0('data/trees.lumbreras.',subforum, '.rda'))
  load(paste0('data/trees.gomez.',subforum, '.rda'))
  load(paste0('data/trees.lumbreras.',subforum, '.rda'))
}

# Compare genetared threads (plot degree distributions, etc)
compare_trees(trees, trees.lumbreras, trees.gomez)


# Compare how good are both models to make posts recommendations
# (if we recommend the most likely parent)
cl <- makeCluster(detectCores()-2)
clusterExport(cl, c("compare_link_prediction", "params.lumbreras", "params.gomez"))
clusterEvalQ(cl, {library(igraph)})
df.preds <- parLapply(cl, trees.test, function(tree) compare_link_prediction(tree, params.lumbreras, params.gomez)) %>% 
  rbindlist %>% 
  as.data.frame
stopCluster(cl)


if(FALSE){
  save(df.preds, file=paste0('data/df.preds.', subforum, '.rda'))
  load(paste0('data/df.preds.', subforum, '.rda'))
}

plot_ranking_benchmarks(df.preds)

