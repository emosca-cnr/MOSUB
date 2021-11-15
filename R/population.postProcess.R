#' Post-processing of optimal subnetworks
#' @param population list of individuals found by \code{mosub_find}
#' @param interactions data.frame with interactions
#' @return a list that contains the population and igraphs objects associated with it
#' @export 
#' @author \packageAuthor{mosub}
population.postProcess <- function(population=NULL, interactions=NULL){

	pathways <- list()
	pathways.igraph <- list()
	
    for(i in 1:length(population$individuals)){

        pathways[[i]] <- interactions[which((interactions$a %in% population$individuals[[i]]) & (interactions$b %in% population$individuals[[i]])), ]
		
		#given ab, eliminates either ab or ba.
		if(dim(pathways[[i]])[1] > 0){

			temp <- pathways[[i]]
			temp.unqEG <- unique(temp$a)
	
			for(ii in 2:length(temp.unqEG)){
				for(jj in 1:(ii-1)){
					temp.idx <- which((temp$a == temp.unqEG[ii]) & (temp$b == temp.unqEG[jj]))
					if(length(temp.idx) > 0)
						temp <- temp[-temp.idx,]
				}
			}
            pathways[[i]] <- temp
		}

		pathways.igraph[[i]] <- igraph::graph.data.frame(pathways[[i]], directed=FALSE)
			
    }
        
	return(c(population, list(igraphs=pathways.igraph)))
	
}



