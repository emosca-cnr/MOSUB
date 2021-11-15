#' Normalization of a pareto front
#' @details This function scales the Pareto front in [0, 1], according to the formula: y = (x - min(x)) / (max(x) - min(x))
#' @author \packageAuthor{mosub}
#' @param x n-by-m matrix with n points expressed with m coordinates 
#' @return scaled matrix
#' @export
pareto.normalize <- function(x=NULL){

        return((x - min(x)) / (max(x) - min(x)))

}



