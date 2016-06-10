# toy optimization tests 

library(neldermead)

foo <- function(x){
  -exp(-x**2)
}

bar <- function(x){
  -1 * (0.3*exp(-(x-5)**2)  +  0.3*exp(-(x+5)**2)  +  0.3*exp(-x**2))
}

outfun.foo <- function(x, optimValues, state){
  outfun.foobar(x, foo)
}
outfun.bar <- function(x, optimValues, state){
  outfun.foobar(x, bar)
}

outfun.foobar <- function(x, foobar){
  xx <- seq(-10, 10, by=0.1)
  plot(xx, foobar(xx), type='l', col = 'red')
  points(x[1], foobar(x[1]))
  cat('\n', x)
  par(new=TRUE) # all plots in the same plot
}

x <- seq(-10, 10, by=0.1)
plot(x, foo(x), type='l', col = 'red')
lines(x, bar(x), col='blue')

opt <- optimset(OutputFcn=outfun)
par(new=FALSE) 
sol <- fminsearch(fun = foo, x0 = -10, verbose=FALSE, optimset(OutputFcn=outfun.foo))
par(new=FALSE) 
sol <- fminsearch(fun = bar, x0 = -10, verbose=FALSE, optimset(OutputFcn=outfun.bar))

# With constrained space
fminbnd(fun=bar, x0=-3.5, xmin=-4, xmax=6, optimset(OutputFcn=outfun.bar))