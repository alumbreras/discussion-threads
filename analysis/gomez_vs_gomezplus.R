# Input: df.trees with all the dataset
# params.gomez and params.gomezplus estimated from the main script


################################################################################
# Generate trees of same size than original ones
################################################################################
cl <- makeCluster(ncores, outfile="", port=11439)
registerDoParallel(cl)

data.list <- foreach(i = 1:nthreads, .packages = c('igraph','dplyr')) %dopar% {
  th <- threads[i]
  df.tree <- df.trees.subforum %>%  filter(thread == th)

  # Avoid threads of size < 2 since there is no randomness there
  if (nrow(df.tree) < 2) {
    res <- data.frame(thread=th,
                      t = NA,
                      parent.real = NA,
                      parent.gomez = NA,
                      parent.lumbreras = NA,
                      parent.gomezplus = NA,
                      subforum=subforum_)  
    return(res)
  }
  users <- df.tree$user

  # hack to add the root user, the grandpas factor needs it
  users.all <- c(df.tree$parent.user[1], users) 
  
  # Simulate the trees and store only their parent tree 
  # (occupies less memory and it is exavtmy the same structural info)
  
  # Real graph
  parents <- df.tree %>% arrange(t) %>% select(parent) %>% unlist %>% as.vector
  parents.real <- parents
  
  # Gomez (everyone the same parameters)
  g.gomez <- gen.thread.Gomez2013(nrow(df.tree)+1, alpha =params.gomez$alpha, 
                                  beta= params.gomez$beta, 
                                  tau = params.gomez$tau)
  parents.gomez <- get.edgelist(g.gomez, names=FALSE)[,2]

  # Gomez plus
  g.gomezplus <- gen.thread.Gomez2013plus(users.all, 
                                          alpha =params.gomezplus$alpha, 
                                          beta= params.gomezplus$beta, 
                                          tau = params.gomezplus$tau,
                                          gamma = params.gomezplus$gamma)
  parents.gomezplus <- get.edgelist(g.gomezplus, names=FALSE)[,2]
  
  
  data.frame(thread=th,
             t = 1:length(parents),
             parent.real = parents.real,
             parent.gomez = parents.gomez,
             parent.lumbreras = parents.lumbreras,
             parent.gomezplus = parents.gomezplus,
             subforum=subforum_)
  
  #}) # end profvis
}
stopCluster(cl)
df.synthetic.trees <- rbindlist(data.list, fill=TRUE)
df.synthetic.trees <- df.synthetic.trees %>% filter(complete.cases(.))


# Compare structures