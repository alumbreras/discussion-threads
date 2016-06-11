Game of Thrones:

* likelihood.gomez 
[1] -4239344




##### with subset of trees
estimation takes < 10 mins

* params.gomez
[1] 0.06934608 3.62409834 0.91054571
* likelihood.gomez 
[1] -205646.2

* params.lumbreras

[1] 0.06934608 3.62409834 0.91054571
> sol$value
[1] -205646.2

* likelihood.lumbreras
alphas:  0.06934608
betas:  3.624098
taus:  0.9105457
 likelihood:  -205646.2

##### k=3 init 3 clusters with same parameters

(only 2 iterations)
Cluster distribution:
 5201.52 5651.96 5578.52

alphas:  0.07897529 0.0421966 0.05748605
betas:  2.910903 1.292057 19.00572
taus:  0.8813849 0.8628861 0.9815242
 likelihood:  -207749.7

Cluster distribution:
 6013.597 4444.553 5973.851

alphas:  0.09542442 0.03303165 0.01244823
betas:  3.575304 0.4384018 23.30167
taus:  0.8519218 0.8561448 0.9885746
 likelihood:  -210523.4

> rowSums(params.lumbreras$traces)
 [1] -204839.2 -203469.3 -201907.1 -201092.7 -200744.3 -200586.6 -200510.4 -200471.0 -200448.4
[10] -200433.8 -200422.5 -200412.1 -200400.9 -200387.3 -200369.2