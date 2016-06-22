# Compares Gomez 2013 and Lumbreras 2016 on a reddit forum
# author: Alberto Lumbreras
###########################################################
# Load dataframe representation of trees
#subforum <- 'podemos'
subforum <- 'gameofthrones'

trees <- load_trees(forum="reddit", subforum=subforum)[1:1000]
df.trees <- trees_to_dataframe(trees)

# In case users are string names, we give each user a unique integer id
# ids are given according to the user frequency (though not necessary)
users <- names(sort(table(df.trees$user), decreasing = TRUE))
df.trees$userint <- match(df.trees$user, users)
trees <- lapply(trees, function(t) {V(t)$userint <- match(V(t)$user, users);t})

if(FALSE){
save(trees, file=paste0('data/trees.', subforum, '.rda'))
save(df.trees, file=paste0('data/df.trees.', subforum, '.rda'))
load(paste0('data/trees.', subforum, '.rda'))
load(paste0('data/df.trees.', subforum, '.rda'))
}

# Estimate parameters for Gomez 2013 and Lumbreras 2016:
alphas <- c(0.1,0.7, 0.5)
betas <- c(0.1,0.7,3)
taus <- c(0.1,0.7,0.1)
params <- list(alphas=alphas, betas=betas, taus=taus)
params.gomez <- estimation_Gomez2013(df.trees, c(0.5, 0.5, 0.5))
params.lumbreras <- estimation_Lumbreras2016(df.trees, params)

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

# Compare likelihood Gomez2013 and Lumbreras2016 over the trees
likelihood.gomez <- likelihood_Gomez2013(df.trees, params.gomez)
likelihood.lumbreras <- likelihood_Lumbreras2016(df.trees, params.lumbreras, params.lumbreras$responsabilities, params.lumbreras$pis)
likelihood.lumbreras.hard <- likelihood_Lumbreras2016_hard(df.trees, params.lumbreras, params.lumbreras$responsabilities)
cat("\nlikelihood Gomez: ", likelihood.gomez)
cat("\nlikelihood Lumbreras:", likelihood.lumbreras)
cat("\nlikelihood Lumbreras.hard:", likelihood.lumbreras.hard[1,1])


# Generate artificial trees under  Gomez 2013 and Lumbreras2016 reproducing the real sizes
# in Lumbreras2016, authors are chosen uniformly
sizes <- sapply(trees, vcount)
trees.gomez <- generate_trees(model='Gomez2013', params=params.gomez, sizes=sizes)
trees.lumbreras <- generate_trees(model='Lumbreras2016', params=params.lumbreras, 
                                  sizes=sizes, user.sample=df.trees$userint)

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
compare_link_prediction(trees, param.lumbreras, params.gomez)
