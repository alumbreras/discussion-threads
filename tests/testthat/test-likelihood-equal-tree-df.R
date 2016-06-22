# load some tree
# tree
df.tree <- tree_to_data(tree)

l1 <- likelihood_Gomez2013_tree(tree, params.gomez)
l2 <- likelihood_Gomez2013(df.tree, params.gomez)
expect_that(l1, equals(l2))

l3 <- likelihood_Lumbreras2016_tree_hard(tree, params.lumbreras, params.lumbreras$responsabilities)
l4 <- likelihood_Lumbreras2016_hard(df.tree, params.lumbreras, params.lumbreras$responsabilities)[1,1]
expect_that(l3, equals(l4))