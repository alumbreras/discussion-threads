this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source('R/datasets.R')
source('R/estimators.R')
source('R/extract_from_db.R')
source('R/likelihood.R')
source('R/plotting.R')
source('R/thread_generators.R')

library(ggplot2)
library(tidyr)
library(dplyr)
library(doParallel)
library(foreach)

theme_horizontal <- theme_bw() + 
  theme(strip.background = element_rect(fill = 'white'), 
        legend.position = "none",
        aspect.ratio=1)


theme_vertical <- theme_bw() + 
  theme(strip.background = element_rect(fill = 'white'), 
        legend.position = "none")


golden_ratio = 2/(1+sqrt(5))

ncores <- detectCores() - 2