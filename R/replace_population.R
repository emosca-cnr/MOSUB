replace_population <- function (x=NULL, pop=NULL) {

    idx <- order(x$rank, -x$cd)[1:pop]
    
    return(list(individuals=x$individuals[idx], objectives=x$objectives[idx, ], rank=x$rank[idx], cd=x$cd[idx]))

}
