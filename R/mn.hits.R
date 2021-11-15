mn.hits <- function(x=NULL, hits=NULL){

    	f <- which(x %in% hits)
        f <- length(f)
                
        return(f)

}
