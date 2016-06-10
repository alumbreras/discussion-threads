context("realtree-to-dataframe")

df.threads <- plyr::count(df.posts, "thread") %>% filter(freq>10)
threads <- sample(df.threads$thread,10)

for (i in 1:length(threads)){
  tree <- database.to.graph(threads[1], con=con, 'reddit')$gp
  df.tree <- tree.to.data(tree)
  
  # Creates the dataframe correctly ?
  expect_that(nrow(df.tree), equals(vcount(tree)-1))
  expect_that(df.tree$post, equals(df.tree$t+1)) # post with k=t+1 added at time t
  expect_that(df.tree$lag, equals(df.tree$t-df.tree$parent+1)) # lag = t - k + 1
  expect_that(df.tree$popularity[1], equals(1)) # root has popularity 1 when first comment arrives
}