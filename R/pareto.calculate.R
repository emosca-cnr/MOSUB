pareto.calculate <- function(x){

  N <- dim(x)[1]
  d <- dim(x)[2]

  pareto <- NULL

  for (i in 1:N) {
    isDominated <- FALSE
    for (j in 1:N) {
      if(j != i){
        dom_less <- 0 #number of objectives where i is better than j
        dom_equal <- 0 #number of objectives where i is equal than j
        dom_more <- 0 #number of objectives where i is worse than j
        for (k in 1:d) {
          if (x[i, k] < x[j, k]) {
            dom_less <- dom_less + 1
          }else if (x[i, k] == x[j, k]) {
            dom_equal <- dom_equal + 1
          }
        }

        if(dom_less == 0 & dom_equal != d) {
          isDominated <- TRUE
          break
        }
      }
    }

    # if no individual dominates i, i is in the Pareto front
    if (!isDominated)
      pareto <- c(pareto, i)

  }

  return(pareto)

}



