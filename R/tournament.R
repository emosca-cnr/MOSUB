tournament <- function(pop.irc, tour=tour){
  
  candidate <- sample(pop.irc$idx, tour)
  
  c_obj_rank <- pop.irc$rank[candidate]
  c_obj_distance <- pop.irc$cd[candidate]
  candidate <- candidate[c_obj_rank == min(c_obj_rank)] #lowest rank
  if(length(candidate) > 1){
    c_obj_rank <- pop.irc$rank[candidate]
    c_obj_distance <- pop.irc$cd[candidate]
    candidate <- candidate[c_obj_distance == max(c_obj_distance)] #highest cd
    if(length(candidate) > 1)
      candidate <- sample(candidate, 1)
  }
  
  return(candidate)
  
}