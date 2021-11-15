front.sort <- function(input.list){
    
    N <- dim(input.list$objectives)[1]
    if(!is.null(N)){
      M <- dim(input.list$objectives)[2]
    }else{
      N <- 1
      M <- length(input.list$objectives)
    }
    
    #set the cd to Inf for all the individuals of the *current front*
    crowding.dist <- matrix(Inf, nrow=N, ncol=M)
     
    # if there are more than two individuals sort on the basis of the cd
    if (N > 2) {
      
      for (i in 1:M) {
        
        #index of current front ranked by the i-th objective
        index_of_objectives <- order(input.list$objectives[, i])
        
        # current max and min values
        f_max <- max(input.list$objectives[, i])
        f_min <- min(input.list$objectives[, i])
        
        if (length(index_of_objectives) > 2 & ((f_max - f_min) != 0)) {
          
          #calculate the crowding distance
          #difference between objective next and previous j dividev by difference in objectives
          for (j in 2:(length(index_of_objectives) - 1)) {
            
            next_obj <- input.list$objectives[index_of_objectives[j + 1], i]
            previous_obj <- input.list$objectives[index_of_objectives[j - 1], i]
            
            crowding.dist[index_of_objectives[j], i] <- (next_obj - previous_obj)/(f_max - f_min)
          }
        }
      }
    }
    
    cd <- apply(crowding.dist, 1, sum)
    
    return(list(idx=input.list$idx, cd=cd))
  }
  