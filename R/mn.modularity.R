mn.modularity <- function(x=NULL, interactions=NULL){

	##connectivity
	##sum of the number of children of the current nodes not included in the current network

	f <- NA

	#find the interactors
	temp <- interactions$b[which(interactions$a %in% x)]
	if(length(temp) > 0){
		f <- length(which(!(temp %in% x)))
	}

	return(f)
}