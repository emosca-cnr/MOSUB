mn.extend <- function(x=NULL, mn.size.min=3, mn.size.max=30, interactions=NULL, max.new.nodes=NULL){
  
  nodes <- x
  nodes.bkp <- nodes
  
  #try for a maximum of 100 cycles to add nodes
  i <- 0
  while(i < 100){
    
    #select a node from nodes
    if(length(nodes) > 1){
      selected <- sample(nodes, 1)
    }else{
      selected <- nodes
    }
    
    #add to nodes the neighbours of selected
    nodes <- mn.add.nodes(nodes, mn.size.max, selected, interactions, max.new.nodes)
    
    if(length(nodes) > mn.size.min)
      return(nodes)
    
    i <- i + 1
  }
  
  return(nodes)
}