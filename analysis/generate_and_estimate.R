# Generates N threads with K types of user
# and infers the clusters and original paramaters
# 
# This is a test to see whether we are able to estimate clusters
# if they are clear
active <-  readRDS("data/df.trees.rds") %>%
            filter(subforum == 'podemos') %>%
            group_by(user) %>% 
              mutate(user.posts = n()) %>% 
            ungroup %>%
            arrange(desc(user.posts), user) %>%
            mutate(rank)
            filter(user.)
  



# Recover the line of the responsability matrix for every user
# NA if user has not been clustered (most of them!)
# store it in each post row for later
df.trees.active$userint <- match(df.trees.active$user,rownames(params.lumbreras$responsibilities))
z <- as.vector(apply(params.lumbreras$responsibilities, 1, which.max))
df.trees.active$z <- z[df.trees.active$userint]

threads <- df.trees.active %>% distinct(thread) %>% unlist %>% as.vector
threads <- sample(threads,10000)
nthreads <- length(threads)

cl <- makeCluster(ncores, outfile="", port=11439)
registerDoParallel(cl)

data.list <- foreach(i = 1:nthreads, .packages = c('igraph','dplyr')) %dopar% {
  th <- threads[i]
  cat('\nsubforum', i, '/', nthreads) 
  df.tree <- df.trees.active %>%  filter(thread == th)
  #prof <- profvis({
  
  
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
  
  if(TRUE){
    # For Lumbreras, pre-compute the parameters of each user accoring to its cluster
    alphas <- params.lumbreras$alpha[df.tree$z]
    alphas[is.na(alphas)] <- params.gomez$alpha # if no cluster, use Gomez
    
    betas <- params.lumbreras$beta[df.tree$z]
    betas[is.na(betas)] <- params.gomez$beta # if no cluster, use Gomez
    
    taus <- params.lumbreras$tau[df.tree$z]
    taus[is.na(taus)] <- params.gomez$tau # if no cluster, use Gomez
    
    #gammas <- params.lumbreras$gamma[df.tree$z]
    #gammas[is.na(gammas)] <- params.gomezplus$gamma # if no cluster, use Gomez
  }
  
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
  
  
  
  # Lumbreras
  g.lumbreras <- gen.thread.Lumbreras2016(users.all, alphas, betas, taus)
  parents.lumbreras <- get.edgelist(g.lumbreras, names=FALSE)[,2]
  
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