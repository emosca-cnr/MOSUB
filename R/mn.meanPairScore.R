mn.meanPairScore <- function(x=NULL, scores=NULL){

	
	idx <- which(x != 0)

	idx.ss <- which(rownames(scores) %in% x[idx])

	#get the subset of ss matrix with only the pairs 
	f <- scores[idx.ss,idx.ss]

	#sum the values in the lower triangular matrix, i.e. all the possible pairs of genes
	f <- f[lower.tri(f)]
	idx.ss <- which(!(is.na(f)))
	f <- sum(f[idx.ss]) / length(idx.ss)

	return(f)

}
