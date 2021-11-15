#'Calculation of the pair-wise similarity of a population of individuals
#' @param x population of individuals defined by \code{mosub_find}
#' @return matrix of similarities
#' @author \packageAuthor{mosub}
#' @export
#'  
mn.sim <- function (x=NULL) {

	#calculate the gene set similarity
	x.intersections <- matrix(0, length(x), length(x))
	for(i in 2:length(x)){
		for(j in 1:(i-1)){
			temp <- length(x[[i]] %in% x[[j]])
			x.intersections[i,j] <- temp / (length(x[[i]]) + length(x[[j]]) - temp)
		}
	}

	#fill also the upper tri and calculate the average distance
	temp <- t(x.intersections)
	x.intersections[upper.tri(x.intersections)] <- temp[which(upper.tri(temp), arr.ind=T)]

    	return(x.intersections)
}
