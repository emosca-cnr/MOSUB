mn.hyper <- function(drawn=NULL, universe=NULL, hits=NULL){


    q <- mn.hits(drawn, hits) #number of white balls drawn
    m <- length(hits) #number of white balls
    n1 <- length(universe) - length(hits) #number of black balls
    k <- length(drawn) #number of balls drawn
    f <- phyper(q, m, n1, k, lower.tail=0) + dhyper(q, m, n1, k)

    return(f)

}
