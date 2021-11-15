#' Function for the evaluation of the enrichment of subnetwork x in two ranked lists of differentially expressed genes
#' 
#' @param x a vector of ids corresponding to the nodes of a subnetwork
#' @param eval_obj.p list with elements \code{exprs1} and \code{exprs2}, two sorted and named vectors of gene expression variations
#' @return numeric vector with the log10 of p-values assessing the enrichment of subnetwork x in differentially expressed genes at the top of \code{exprs1} and \code{exprs2} vectors.
#' @export
#' @author \packageAuthor{mosub}
#' 
eval.obj.ee <- function (x=NULL, eval_obj.p=NULL) {

    f <- c(0, 0)
	f[1] <- log10(mn.page(x, eval_obj.p$exprs1))
	f[2] <- log10(mn.page(x, eval_obj.p$exprs2))
               
	return(f)
}
