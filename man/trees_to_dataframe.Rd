% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\name{trees_to_dataframe}
\alias{trees_to_dataframe}
\title{Builds a dataframe from a list of trees where every row is a post and every column 
is some post feature}
\usage{
trees_to_dataframe(trees)
}
\arguments{
\item{trees}{a list of igraph trees}
}
\value{
a dataframe with one row per post
}
\description{
Builds a dataframe from a list of trees where every row is a post and every column 
is some post feature
}
\details{
uses parallelization
}

