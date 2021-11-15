mutation <- function(child_3, interactions, mn.size.min=10, mn.size.max=50, mutation.fraction.del, max.new.nodes=3){
  
  #random selection of a node index
  node <- sample(1:length(child_3), 1)
  
  u <- runif(1)
  
  #updates the indexes of nodes == 0
  n.empty <- mn.size.max - length(child_3)
  
  if((n.empty == 0) | ((u < mutation.fraction.del) & length(child_3) > mn.size.min)){
    
    #deletion if there are not empty nodes or if u < mutation.fraction.del and there are enough nodes
    child_3 <- child_3[-node]          
    child_3 <- mn.check(child_3, interactions, mn.size.max, mn.size.min, max.new.nodes)
    
  }else if(n.empty > 0){
    #addition of the  nodes connected to child_3[node]
    child_3 <- mn.add.nodes(child_3, mn.size.max, child_3[node], interactions, max.new.nodes)
    child_3 <- mn.check(child_3, interactions, mn.size.max, mn.size.min, max.new.nodes)
  }
  
  return(child_3)
  
}

