# Test that the graphs built from the database
# are properly built
# - test ascendants are older than descendants
###############################################
library(RSQLite)
library(dplyr)

context("db-to-igraph")

# Load data
data.dir <- file.path(system.file("data", package="threads"))
con <- dbConnect(dbDriver("SQLite"), dbname = paste0(data.dir, "/reddit.db"));
load(paste0(data.dir, '/dfposts_podemos.Rda'))

# Get some thread ids
df.threads <- plyr::count(df.posts, "thread") %>% filter(freq>10)
threads <- sample(df.threads$thread, 10)

for (i in 1:length(threads)){
  g <- database.to.graph(threads[i], con=con, 'reddit')$gp
  for(j in 1:vcount(g)){
    ascendants <- ego(g, 100, nodes=j, mode='out')
    for(k in 1:length(ascendants)){
      expect_gte(as.numeric((V(g)[j]$date)),
                       as.numeric(V(g)[ascendants[[1]][k]]))
    }
  }
   expect_lte(0, prod(diff(V(g)$date)))

}
