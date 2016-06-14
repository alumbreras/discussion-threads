library('testthat')
source('R/thread_generators.R')
source('R/extract_from_db.R')

# Load data
con <- dbConnect(dbDriver("SQLite"), dbname = paste0("data/reddit.db"));
load('data/dfposts_podemos.Rda')

# Tests
test_dir('R/testthat/')