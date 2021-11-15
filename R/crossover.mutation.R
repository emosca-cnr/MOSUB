crossover.mutation <- function(parents, N, interactions, crossover.prob=0.1, mn.size.min=10, mn.size.max=50, mutation.fraction=0.1, mutation.fraction.del=0.75, max.new.nodes=3){
  
  parents.idx <- sample(N, 2)
  parent_1 <- parents[[parents.idx[1]]]
  parent_2 <- parents[[parents.idx[2]]]
  child_3 <- parent_1 #this is for mutation
  
  #nodes in common between the two parents for crossover
  common <- parent_1[parent_1 %in% parent_2]
  common.N <- length(common)
  
  was_crossover <- 0
  was_mutation <- 0
  
  # With crossover.prob# probability perform crossover if there is at least one node in common
  if((runif(1) <= crossover.prob) & common.N > 0){
    
    children <- crossover(parent_1, parent_2, common, interactions, mn.size.max, mn.size.min, max.new.nodes)
    was_crossover <- 1;
        
  }else{
      
    #repeat the mutation according to the specified percentage
    for(j in 1:ceiling(length(child_3) * mutation.fraction))
      child_3 <- mutation(child_3, interactions, mn.size.min, mn.size.max, mutation.fraction.del, max.new.nodes)
    was_mutation <- 1
  }
  
  if(was_crossover){
    return(children)
  }else{
    return(list(child_3))
  }
  
}