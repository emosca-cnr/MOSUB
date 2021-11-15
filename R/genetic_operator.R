genetic_operator <- function(parents=NULL, interactions=NULL, mn.size.max=30, crossover.prob=0.1, mutation.fraction=0.1, mutation.fraction.del=0.75, mn.size.min=3, max.new.nodes=NULL, parallel=FALSE, mc.cores=mc.cores){
  
  N <- length(parents)
  
  temp <- list()
  for(i in 1:N)
    temp[[i]] <- parents
  
  if(parallel){
    
    children <- parallel::mclapply(temp, crossover.mutation, N=N, interactions=interactions, crossover.prob=crossover.prob, mn.size.min=mn.size.min, mn.size.max=mn.size.max, mutation.fraction=mutation.fraction, mutation.fraction.del=mutation.fraction.del, max.new.nodes=max.new.nodes, mc.cores=mc.cores)
    
  }else{
    
    children <- lapply(temp, crossover.mutation, N=N, interactions=interactions, crossover.prob=crossover.prob, mn.size.min=mn.size.min, mn.size.max=mn.size.max, mutation.fraction=mutation.fraction, mutation.fraction.del=mutation.fraction.del, max.new.nodes=max.new.nodes)
    
  }
  
  children <- unique(lapply(children, function(x) x[[1]])) #from list of lists to list
  children <- unique(lapply(children, sort))
  nulls <- which(unlist(lapply(children, is.null)))
  if(length(nulls)>0)
    children <- children[-nulls]
  
  return(children)
  
}
