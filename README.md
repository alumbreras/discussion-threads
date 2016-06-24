# Overview

I'm trying to follow these ideas to adapt the concept of R packages for reproducible research
https://github.com/ropensci/rrrpkg

#### On how we represent discussion threads
One of our main objects is a tree that represents a discussion thread. A tree is represented by an *igraph* object. We use this representation to plot trees and to compute structural properties such as degree distribution, size vs depth, and so on.

Anothere very useful representation is a dataframe where every row represents a post (a comment) of a discussion thread. For every post, we have a set of attributes describing it such as the degree of the father, the time-steps between the post and its father and whether the fathere is the root of the thread. With this vector representation of a post, the total likelihood is just the sum of the likelihood of each row (given some candidate parameters).

#### Scripts

**main.R**

Main script that call controls the pipeline of subscripts.

**datasets.R**

* load_trees()
* generate_trees()
* trees_to_dataset()

**estimators.R**

* estimation_Gomez2013()
* estimation_Lumbreras2016()

**likelihoods.R**

* likelihood_post()
* likelihood_Gomez2013()
* likelihood_Lumbreras2016()


**plot_structural_properties.R**

Given a set of parameters, generate N artificial threads and plot some 
figures to analyse their properties:
  
  * x: degrees y: propability (log-log)
  * x: subtree sizes y: probabilities (log-log)
  * x: size y: depth (log-log)

**link_prediction.R**


Compare Gomez 2013, Lumbreras 2016 and baselines for the task of predicting the parent.
Specifically, we rank posts according to their likelihood of being the next parent.
  
 * predict()
 * plot_ranking_benchmarks()



#### Other scripts

**estimate_params_Gomez2013.r**:

Reproduces Section 3.1 of Gomez 2013 where they check that the estimates are not biased.

# Implementation notes

The most costly part of the Expectation-Maximization is the maximization step. Since we do not have an analytic solution, we use numerical optimization methods.

The chosen method is the Nelder-Mead. It allows non-linear opimization, $n$ dimensions (though it has been reported to perform badly for high dimensions) and some implementations also deal with boundaries.

Strangely, the `neldermead::fminbnd` produces some errors with our model and our data. `stats::nlminb` and `dfoptim::nmkb` produce the same results (as they should!) and the speed is similar, `nmkb` being a bit faster.

`vectoptim::nmkb.vec` seems to use paralellization, but the package is no longer mantained as has been removed from the CRAN repositories.

`mcGlobaloptim::multiStartoptim` combines local searches with, for instance, Nelder Mead, and Monte Carlo sampling to add (I guess) some randomisation in order to find the global optimum. However, I guess the price to pay is a slowing down of speed. Anyway, I didn't get it to work.


`stats::optim`(..., method='L-BFGS-B',...)
 
Given all this, we use `dfoptim::nmkb` for the optimizations.