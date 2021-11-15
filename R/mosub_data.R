#' Protein-protein interaction and gene expression data included in package mosub.
#' @format \code{data.table} containing human protein-protein associations:
#' \describe{
#'   \item{a}{Entrez Gene id;}
#'   \item{b}{Entrez Gene id;}
#'   \item{score}{confidence score.}
#' }
#' @source STRING database
#' @references Szklarczyk, D., et al., 2011. The STRING database in 2011: functional interaction networks of proteins, globally integrated and scored. Nucleic Acids Res. 39: D561-8.
"interactions"

#' Gene expression data included in package mosub 
#' @source The Cancer Genoma Atlas http://tcga-data.nci.nih.gov/tcga/.
#' @format vector of gene log2-fold changes between breast carcinoma and normal samples; \code{names(ge1)} are Entrez Gene ids.
"ge1"

#' Gene expression data included in package mosub 
#' @source The Cancer Genoma Atlas http://tcga-data.nci.nih.gov/tcga/.
#' @format vector of gene log2-fold changes between breast carcinoma and normal samples; \code{names(ge2)} are Entrez Gene ids.
"ge2"

#' Output of mosub_find after 10 iterations
#' @format list with elements \code{individuals}, \code{objectives}, \code{rank} and \code{cd}.
"res.10"

#' Output of mosub_find after 100 iterations
#' @format list with elements \code{individuals}, \code{objectives}, \code{rank} and \code{cd}.
"res.100"

#' Output of mosub_find after 500 iterations
#' @format list with elements \code{individuals}, \code{objectives}, \code{rank}, \code{cd} and \code{igraphs}.
"res.500"
