# Compares Gomez 2013 and Lumbreras 2016 on a reddit forum
# author: Alberto Lumbreras
###########################################################
# Load dataframe representation of trees

#trees <- load_trees(forum="reddit", subforum="gameofthrones")
trees <- load_trees(forum="reddit", subforum="podemos")
df.trees <- trees_to_dataframe(trees)

save(trees, file='data/trees.podemos.rda')
save(df.trees, file='data/df.trees.podemos.rda')
#load('data/trees.gof.rda')
#load('data/df.trees.gof.rda')

# In case users are string names, we give each user a unique integer id
# besides, ids are given according to the user frequency (though not necessary)
users <- names(sort(table(df.trees$user), decreasing = TRUE))
df.trees$userint <- match(df.trees$user, users)

# Estimate parameters for Gomez 2013 and Lumbreras 2016:
alphas <- c(0.1,0.7, 0.5)
betas <- c(0.1,0.7,3)
taus <- c(0.1,0.7,0.1)
params <- list(alphas=alphas, betas=betas, taus=taus)
params.gomez <- estimation_Gomez2013(df.trees, c(0.5, 0.5, 0.5))
params.lumbreras <- estimation_Lumbreras2016(df.trees, params)

# If you use a subset of the dataset, do this again
#users <- names(sort(table(df.trees$user), decreasing = TRUE))
#df.trees$userint <- match(df.trees$user, users)


cat('\n', params.gomez)
cat('\n alphas: ', params.lumbreras$alphas)
cat('\n betas: ', params.lumbreras$betas)
cat('\n taus: ', params.lumbreras$taus)
plot(rowSums(params.lumbreras$traces), type='b')
title('\n Likelihood')

save(params.gomez, file='data/params.gomez.podemos.rda')
save(params.lumbreras, file='data/params.lumbreras.podemos.rda')
#load('data/params.gomez.rda')
#load('data/params.lumbreras.rda')

# Compare likelihood Gomez2013 and Lumbreras2016 over the trees
likelihood.gomez <- likelihood_Gomez2013(df.trees, params.gomez)
likelihood.lumbreras <- likelihood_Lumbreras2016(df.trees, params.lumbreras, params.lumbreras$responsabilities, params.lumbreras$pis)
likelihood.lumbreras.hard <- likelihood_Lumbreras2016.hard(df.trees, params.lumbreras, params.lumbreras$responsabilities)
cat("\nlikelihood Gomez: ", likelihood.gomez)
cat("\nlikelihood Lumbreras:", likelihood.lumbreras)
cat("\nlikelihood Lumbreras.hard:", likelihood.lumbreras.hard[1,1])

params.lumbreras$z <- as.vector(apply(params.lumbreras$responsabilities, 1, which.max))


# Generate artificial trees under  Gomez 2013 and Lumbreras2016 reproducing the real sizes
# in Lumbreras2016, authors are chosen uniformly
sizes <- sapply(trees, vcount)
users <- df.trees$userint
trees.gomez <- generate_trees(model='Gomez2013', params=params.gomez, sizes=sizes)
trees.lumbreras <- generate_trees(model='Lumbreras2016', params=params.lumbreras, sizes=sizes, user.sample=users)

save(trees.gomez, file = 'data/trees.gomez.podemos.rda')
save(trees.lumbreras, file = 'data/trees.lumbreras.podemos.rda')

# Compare genetared threads (plot degree distributions, etc)
compare_trees(trees, trees.lumbreras, trees.gomez)


# Compare how good are both models to make posts recommendations
# (if we recommend the most likely parent)
