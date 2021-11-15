domination <- function(objectives){
  
  idx <- objectives[1, 1]
  objectives <- objectives[-1,]
  
  N <- dim(objectives)[1]
  M <- dim(objectives)[2]
  
  individual.n <- 0 #number of individuals that dominates idx
  individual.p <- numeric(0) #idx of individuals that are dominated from idx
  
  for (j in 1:N) {
    dom_less <- 0
    dom_equal <- 0
    dom_more <- 0
    for (k in 1:M) {
      if (objectives[idx, k] < objectives[j, k]) { #dominated from idx
        dom_less <- dom_less + 1
      }
      else if (objectives[idx, k] == objectives[j, k]) { #equal
        dom_equal <- dom_equal + 1
      }
      else {
        dom_more <- dom_more + 1 #dominating idx
      }
    }
    if (dom_less == 0 & dom_equal != M) {
      individual.n <- individual.n + 1
    }
    else if (dom_more == 0 & dom_equal != M) {
      individual.p <- c(individual.p, j)
    }
  }
  
  front <- FALSE
  if (individual.n == 0)
    front <- TRUE
  
  return(list(idx=idx, n=individual.n, p=individual.p, front=front))
  
}