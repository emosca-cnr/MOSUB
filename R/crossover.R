crossover <- function(parent_1, parent_2, common, interactions, mn.size.max=50, mn.size.min=10, max.new.nodes=3){
  
  child_1 <- parent_1
  child_2 <- parent_2
  
  # select one of the nodes in common
  if(length(common) > 1){
    selected <- sample(common, 1)
  }else{
    selected <- common
  }
  
  # select the interacting nodes: all the 1-neighbours of the selected node
  # remove one of the selected nodes to ensure the operations will not break the network
  #selected_children_1 <- unique(interactions$b[ (interactions$a == selected) & (interactions$b %in% parent_1)])
  selected_children_1 <- interactions[ J(selected, parent_1)]
  selected_children_1 <- unique(selected_children_1$b[!is.na(selected_children_1$score>0)])
  selected_children_1.N <- length(selected_children_1)
  if(length(selected_children_1) > 1){
    selected_children_1 <- selected_children_1[-sample(1:length(selected_children_1), 1)]
    selected_children_1.N <- selected_children_1.N - 1
  }
  
  selected_children_2 <- interactions[J(selected, parent_2)]
  selected_children_2 <- unique(selected_children_2$b[!is.na(selected_children_2$score>0)])
  selected_children_2.N <- length(selected_children_2)
  if(length(selected_children_2) > 1){
    selected_children_2 <- selected_children_2[-sample(1:length(selected_children_2), 1)]
    selected_children_2.N <- selected_children_2.N - 1
  }
  
  # remove the nodes from the parent 1
  if(selected_children_1.N > 0){
    temp <- which(parent_1 %in% selected_children_1)
    if(length(temp) > 0)
      child_1 <- child_1[-temp]
  }
  #check the available places and add the nodes
  empty.child_1 <- mn.size.max - length(child_1)
  if((selected_children_2.N > 0) & (empty.child_1 > 0)){
    N.replace <- min(empty.child_1, selected_children_2.N)
    child_1 <- c(child_1, selected_children_2[1 : N.replace])
  }
  
  # remove the nodes from the parent 2
  if(selected_children_2.N > 0){
    temp <- which(parent_2 %in% selected_children_2)
    if(length(temp) > 0)
      child_2 <- child_2[-temp]
  }
  #check the available places and add the nodes
  empty.child_2 <- mn.size.max-length(child_2)
  if(selected_children_1.N > 0 & empty.child_2 > 0){
    N.replace <- min(empty.child_2, selected_children_1.N)
    child_2 <- c(child_2, selected_children_1[1 : N.replace])
  }
  
  # remove duplicated nodes
  child_1 <- unique(sort(child_1))
  child_2 <- unique(sort(child_2))
  
  #check networks
  child_1 <- mn.check(child_1, interactions, mn.size.max, mn.size.min, max.new.nodes)
  child_2 <- mn.check(child_2, interactions, mn.size.max, mn.size.min, max.new.nodes)
  
  return(list(child_1, child_2))
  
}