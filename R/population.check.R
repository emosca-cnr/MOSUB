population.check <- function(population=NULL, pop=NULL, M=NULL, mn.size.max=30, mn.size.min=3){

	temp <- unlist(lapply(population$individuals, length))

	if(min(temp) < mn.size.min)
		return(FALSE)

	if(max(temp) > mn.size.max)
		return(FALSE)

    if(dim(population$objectives)[1] != pop | dim(population$objectives)[2] != M)
		return(FALSE)

    if(any(is.na(population$objectives)))
        return(FALSE)

	if(length(population$rank) != pop | length(population$cd) != pop)
		return(FALSE)

	return(TRUE)
}