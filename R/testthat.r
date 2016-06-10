library('testthat')
source('R/thread_generators.r')
source('R/extract_from_db.r')

# Load data
con <- dbConnect(dbDriver("SQLite"), dbname = paste0("data/reddit.db"));
load('R_objects/dfposts_podemos.Rda')

# Tests
test_dir('R/testthat/')