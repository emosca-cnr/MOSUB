tournament_selection <-
  function (population=NULL, pool=NULL, tour=2) 
  {
    pop <- length(population$individuals)
    f <- list()
    
    # this parrallel version is worse than  the sequential :(
    #     if(parallel=='multicore'){
    #       
    #       temp <- list()
    #       for(i in 1:pop)
    #        temp[[i]] <- list(idx=1:pool, rank=population$rank, cd=population$cd)
    #       
    #       f <- unlist(mclapply(temp, tournament, tour=tour))
    #       
    #       return(population$individuals[f])
    #      
    #    }else{
    
    for (i in 1:pool) {
      
      candidate <- sample(pop, tour)
      c_obj_rank <- population$rank[candidate]
      c_obj_distance <- population$cd[candidate]
      candidate <- candidate[c_obj_rank == min(c_obj_rank)] #lowest rank
      if(length(candidate) > 1){
        c_obj_rank <- population$rank[candidate]
        c_obj_distance <- population$cd[candidate]
        candidate <- candidate[c_obj_distance == max(c_obj_distance)] #highest cd
        if(length(candidate) > 1)
          candidate <- sample(candidate, 1)
      }
      
      if((candidate < 1) | (candidate > pop)){
        cat('ERROR, problem with candidate selection\n')
        save.image('error.RData')
      }
      
      f[i] <- population$individuals[candidate]
    }
    
    #    }
    
    return(f)
  }
