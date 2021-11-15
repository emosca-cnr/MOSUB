#' Monte-Carlo Calculation of Pareto hypervolume
#' @param P matrix of objective values \(columns\) for a set of optimal solutions \(rows\)
#' @param r vector of maximum values of optimal solutions
#' @param N number of permutations
#' @return numeric value, the hypervolume
#' @author \packageAuthor{mosub}
#' @export

pareto.hypervolume <- function(P=NULL, r=c(1, 1), N=10000){

    P <- P %*% diag(1/r)
    d <- dim(P)[2]
    n <- dim(P)[1]    
    C <- randtoolbox::sobol(N, d, scrambling=1)
    
    fDominated <- array(F, N)
    lB <- apply(P, 2, min)
    fcheck <- apply(t(apply(C, 1, function(x) x > lB)), 1, all)
    
    for (k in 1:n){
        if(any(fcheck)){
            f <- apply(t(apply(C[fcheck, ], 1, function(x) x >  P[k, ])), 1, all)
            fDominated[fcheck] <- f
            fcheck[fcheck] <- !f
        }
    }
    
    return(sum(fDominated)/N)

}
