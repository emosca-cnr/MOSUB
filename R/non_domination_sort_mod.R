non_domination_sort_mod <-
  function (population=NULL, parallel=FALSE, mc.cores=mc.cores) 
  {
    
    N <- length(population$individuals)
    M <- dim(population$objectives)[2]
    front <- 1
    
    population$rank <- rep(0, N)
    population$cd <- rep(0, N)
    
    F <- array(list(numeric(0)), dim = 1)
    individual.n <- as.vector(rep(0, N), "numeric")
    individual.p <- array(list(numeric(0)), dim = N)
    
    if(any(is.na(population$objectives))){
      print(population)
      stop("NA found\n")
    }
    
    
    if(parallel){
      
      temp <- list()
      for(i in 1:N)
        temp <- c(temp, list(rbind(rep(i, M), population$objectives)))
      
      temp.res <- parallel::mclapply(temp, domination, mc.cores=mc.cores)
      
      individual.idx <- unlist(lapply(temp.res, function(x) x$idx))
      individual.n[individual.idx] <- unlist(lapply(temp.res, function(x) x$n))
      individual.p[individual.idx] <- lapply(temp.res, function(x) x$p)
      F[[front]] <- individual.idx[unlist(lapply(temp.res, function(x) x$front))]
      population$rank[individual.idx %in% F[[front]]] <- 1
      
      rm(temp, temp.res)
      
    }else{
      
      #sequential version
      for (i in 1:N) {
        for (j in 1:N) {
          dom_less <- 0
          dom_equal <- 0
          dom_more <- 0
          for (k in 1:M) {
            if (population$objectives[i, k] < population$objectives[j, k]) {
              dom_less <- dom_less + 1
            }
            else if (population$objectives[i, k] == population$objectives[j, k]) {
              dom_equal <- dom_equal + 1
            }
            else {
              dom_more <- dom_more + 1
            }
          }
          if (dom_less == 0 & dom_equal != M) {
            individual.n[i] <- individual.n[i] + 1
          }
          else if (dom_more == 0 & dom_equal != M) {
            individual.p[[i]] <- c(individual.p[[i]], j)
          }
        }
        
        if (individual.n[i] == 0) {
          population$rank[i] <- 1
          F[[front]] <- c(F[[front]], i)
        }
      }
    }
    
    
    #front assignment
    #for each individual of the current front, consider the individuals dominated by it
    #for each individual dominated, reduce the number of individual that dominate it and moves is to the next front
    #increase the front and assign to it the dominated individuals
    while (length(F[[front]]) > 0) {  
      Q <- numeric(0)
      for (i in 1:length(F[[front]])) { # for each i-element of the current front
        if (length(individual.p[[F[[front]][i]]]) > 0) { # if there are still elements dominated by i
          individuals_p_front_i <- individual.p[[F[[front]][i]]] #define the array of elements dominated by i
          for (j in 1:length(individuals_p_front_i)) { #for each j-element dominated by i
            individual.n[[individuals_p_front_i[j]]] <- individual.n[[individuals_p_front_i[j]]] - 1 #reduce the number of individuals that dominate element j
            if (individual.n[[individuals_p_front_i[j]]] == 0) { #if the number of individuals that dominate element j are zero
              population$rank[individuals_p_front_i[j]] <- front + 1 # set the rank of that individual as current front plus 1
              Q <- c(Q, individuals_p_front_i[j]) #place the element j in array Q
            }
          }
        }
      }
      front <- front + 1 #increase the current front
      F[[front]] <- Q #assign the elements in Q to the current front
    }
    
    
    if(parallel){
      
      temp <- list()
      for(i in 1:(length(F)-1))
        temp[[i]] <- list(idx=F[[i]], objectives=population$objectives[F[[i]], ])
      
      temp.res <- parallel::mclapply(temp, front.sort, mc.cores=mc.cores)

      individuals.idx <- unlist(lapply(temp.res, function(x) x$idx))
      individuals.cd <- unlist(lapply(temp.res, function(x) x$cd))
      
      population.sorted <- list(individuals=population$individuals[individuals.idx], objectives=population$objectives[individuals.idx, ], rank=population$rank[ individuals.idx], cd=individuals.cd)
      
      rm(temp, temp.res)
      
    }else{
      
      # index of individuals sorted on the basis of their ranks
      #sorted_based_on_front <- order(population$rank)
      sorted_based_on_front <- unlist(F)
      current_index <- 0
      population.sorted <- list(individuals=NULL, objectives=NULL, rank=NULL, cd=NULL)
      
      #for each front calcualtes the crowding distance
      for (front in 1:(length(F) - 1)) {
        
        crowding.dist <- 0
        
        #current_index + 1 indicates the first individuals of the current front
        #current_index + length(F[[front]]) indicates the last individuals of the current front
        current.front <- sorted_based_on_front[(current_index + 1):(current_index + length(F[[front]]))]
        
        #set the cd to Inf for all the individuals of the *current front*
        crowding.dist <- matrix(Inf, nrow=length(current.front), ncol=M)
        
        #set the current index to the last individual of the current front
        current_index <- current_index + length(current.front)
        
        # if there are more than two individuals sort on the basis of the cd
        if (length(current.front) > 2) {
          
          for (i in 1:M) {
            
            #index of current front ranked by the i-th objective
            index_of_objectives <- order(population$objectives[current.front, i])
            
            # current max and min values
            f_max <- max(population$objectives[current.front, i])
            f_min <- min(population$objectives[current.front, i])
            
            if (length(index_of_objectives) > 2 & ((f_max - f_min) != 0)) {
              
              #calculate the crowding distance
              #difference between objective next and previous j dividev by difference in objectives
              for (j in 2:(length(index_of_objectives) - 1)) {
                
                next_obj <- population$objectives[current.front[index_of_objectives[j + 1]], i]
                previous_obj <- population$objectives[current.front[index_of_objectives[j - 1]], i]
                
                crowding.dist[index_of_objectives[j], i] <- (next_obj - previous_obj)/(f_max - f_min)
              }
            }
          }
        }
        
        #set the current individuals, rank and cd of the new population
        population.sorted$individuals <- c(population.sorted$individuals, population$individuals[current.front])
        if(is.null(population.sorted$objectives)){
          population.sorted$objectives <- population$objectives[current.front, ]
        }else{
          population.sorted$objectives <- rbind(population.sorted$objectives, population$objectives[current.front, ])
        }
        population.sorted$rank <- c(population.sorted$rank, population$rank[current.front])
        population.sorted$cd <- c(population.sorted$cd, apply(crowding.dist, 1, sum))
      }
    }
    
    return(population.sorted)
  }
