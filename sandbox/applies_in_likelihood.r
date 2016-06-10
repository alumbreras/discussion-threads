df.test <- data.frame(a=1:10, b=1:10)
apply(df.test, 1, function(x) x$a + x$b) # ko
apply(df.test, 1, function(x) x['a'] + x['b']) # ok

df.test <- data.frame(a=1:5, b=1:5, c=c('aa', 'bb', 'cc', 'dd', 'ee'))
apply(df.test, 1, function(x) x['a'] + x['b'])
apply(df.test[,-3], 1, function(x) x['a'] + x['b'])

funct <- function(a, b)  {a + b}
mapply(funct, df.test$a, df.test$b)

sum(sapply(1:nrow(df.trees), function(i) likelihood.post(df.trees[i,], params[1], params[2], params[3]))) # ok
sum(apply(df.trees[,-2], 1, function(i) likelihood.post(i, params[1], params[2], params[3]))) # ok


apply(df.trees[,-2], 1, function(i) params[1]*i['popularity']+ params[2]*(i['parent']==1) + params[3]^i['lag'])



likelihood.post <- function(row, alpha, beta, tau){
  c(as.matrix((log(alpha * row['popularity'] + beta*(row['parent']==1) + tau^row['lag']) - 
     log(2*alpha*(row['t']-1/2)   + beta + tau*(tau^row['t']-1)/(tau-1)))))
}

sum(sapply(1:nrow(df.trees), function(i) likelihood.post(df.trees[i,], params[1], params[2], params[3])))
sum(apply(df.trees[,-2], 1, function(i) likelihood.post(i, params[1], params[2], params[3])))

benchmark(sum(sapply(1:nrow(df.trees_), function(i) likelihood.post(df.trees[i,], params[1], params[2], params[3]))),
          sum(apply(df.trees_[,-2], 1, function(i) likelihood.post(i, params[1], params[2], params[3]))))