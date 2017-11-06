this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(ggplot2)
library(tidyr)
library(dplyr)
library(doParallel)
library(foreach)
library(data.table)

source('R/datasets.R')
source('R/estimators.R')
source('R/likelihood.R')
source('R/thread_generators.R')
source('R/extract_from_db.R')
source('R/plot_structural_properties3_SNAM.R')
source('R/link_prediction2.R')
source('R/plotting.R')


# Some global options
theme_horizontal <- theme_bw() + 
  theme(strip.background = element_rect(fill = 'white'), 
        legend.position = "none",
        aspect.ratio=1)

theme_vertical <- theme_bw() + 
  theme(strip.background = element_rect(fill = 'white'), 
        legend.position = "none")

golden_ratio = 2/(1+sqrt(5))

ncores <- detectCores() - 2