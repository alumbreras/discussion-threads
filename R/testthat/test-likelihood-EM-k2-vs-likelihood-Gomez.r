context("likelihood-EM")
# Test that the solution of two components with the same parameters
# has the same likelihood than one component with that parameters

source('R/datasets.R')
source('R/likelihood.R')
source('R/extract_from_db.R')
source('R/estimators.R')

# Load dataframe representation of trees
load('data/trees.gof.rda')
load('data/df.trees.gof.rda')

# Subset for testing purposes
trees <- trees[1:10]
df.trees <- trees_to_dataframe(trees)

# In case users are string names, we give each user a unique integer id
# besides, ids are given according to the user frequency (though not necessary)
users <- names(sort(table(df.trees$user), decreasing = TRUE))
df.trees$userint <- match(df.trees$user, users)
U <- length(unique(df.trees$userint))

params.gomez <- c(0.5, 0.5, 0.5)
params.lumbreras <- list(alphas=rep(params.gomez[1],2), 
                         betas=rep(params.gomez[2],2), 
                         taus=rep(params.gomez[3],2))
responsabilities <- matrix(c(0.5,0.5), nrow=U, ncol=2)
pis <- c(0.5,0.5)

##############################################################

# Compare likelihoods
likelihood.gomez <- likelihood_Gomez2013(df.trees, params.gomez)
likelihood.lumbreras <- likelihood_Lumbreras2016(df.trees, params.lumbreras, responsabilities, pis)
likelihood.lumbreras.hard <- likelihood_Lumbreras2016.hard(df.trees, params.lumbreras, responsabilities)

expect_that(likelihood.gomez, equals(likelihood.lumbreras))