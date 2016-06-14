# Tree illustrated in Gomez 2013

context("thread-generators")

tree.toy <- make_empty_graph(n=9)
tree.toy <- add_edges(tree.toy, c(2,1,
                                3,1,
                                4,2,
                                5,1,
                                6,5,
                                7,2,
                                8,1,
                                9,6)) 
V(tree.toy)$user <- 1

###################################################
# Generates trees of correct size?
tree.synthetic <- gen.thread.Lumbreras2016(n=100, 
                                          z=c(1,2,3), 
                                          alphas = c(1,2,3), 
                                          betas = c(1,2,3), 
                                          taus = c(0.25, 0.5, 0.75))
  
expect_that(vcount(tree.synthetic), equals(100))


#################################################
# Creates the dataframe correctly
df.tree <- tree.to.data(tree.toy)
expect_that(nrow(df.tree), equals(vcount(tree.toy)-1))
expect_that(df.tree$post, equals(df.tree$t+1)) # post with k=t+1 added at time t
expect_that(df.tree$lag, equals(df.tree$t-df.tree$parent+1)) # lag = t - k + 1
expect_that(df.tree$popularity[1], equals(1)) # root has popularity 1 when first comment arrives