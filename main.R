# Compares Gomez 2013 and Lumbreras 2016 on a reddit forum
# author: Alberto Lumbreras
###########################################################
source('R/datasets.R')
source('R/likelihood.R')
source('R/extract_from_db.R')
source('R/estimators.R')

# Load dataframe representation of trees
trees <- load_trees(forum="reddit", subforum="gameofthrones")
df.trees <- trees_to_dataframe(trees)
#save(trees, file='trees.gof.rda')
#save(df.trees, file='df.trees.gof.rda')
#load('data/trees.gof.rda')
#load('data/df.trees.gof.rda')

# Subset for testing purposes
trees <- trees[1:1000]
df.trees <- trees_to_dataframe(trees)

# Estimate parameters
# Gomez 2013: 
# alpha, beta, tau
# Lumbreras 2016:
# alpha_1, beta_1, tau_1
# ...
# alpha_1, beta_1, tau_1
# z_1,...z_U
params.gomez <- estimation_Gomez2013(df.trees, c(0.5, 0.5, 0.5))

alphas <- 0.5
betas <- 0.5
taus <- 0.5
params <- list(alphas=alphas, betas=betas, taus=taus)
params.lumbreras <- estimation_Lumbreras2016(df.trees, params)
cat('\n', params.gomez)
cat('\n', params.lumbreras)

alphas <- c(0.5,0.5)
betas <- c(0.5,0.5)
taus <- c(0.5,0.5)
params <- list(alphas=alphas, betas=betas, taus=taus)
params.lumbreras <- estimation_Lumbreras2016(df.trees, params)
cat('\n', params.gomez)
cat('\n', params.lumbreras)




# Compare likelihood Gomez2013 and Lumbreras2016 over the trees
likelihood.gomez <- likelihood_Gomez2013(df.trees, params.gomez)
likelihood.lumbreras <- likelihood_Lumbreras2016(df.trees, params.lumbreras, params.lumbreras$responsabilities)

# Generate artificial trees under  Gomez 2013 and Lumbreras2016
trees.gomez <- generate_trees(model='Gomez2013', params=params.gomez)
trees.lumbreras <- generate_trees(model='Lumbreras2016', params=params.lumbreras)
