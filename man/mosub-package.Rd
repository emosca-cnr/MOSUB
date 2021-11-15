\name{mosub-package}
\alias{mosub-package}
\alias{mosub}
\docType{package}
\title{
\packageTitle{mosub}
}
\description{
\packageDescription{mosub}
}
\details{
The package is designed for the identification of multi-optimal subnetworks. The problem is formulated as the multi-objective optimization of two or more functions  that are evaluated on subnetworks. The problem is solved using a graph-based version (Mosca and Milanesi 2013) of the Non-dominated Sorting Genetic Algorithm II (NSGA-II) (Deb, et al. 2002). Each solution of the problem is a set of identifiers that form a connected subnetwork and genetic operators are designed to modify subnetworks within the constraints of the network they belong to.}
\author{
\packageAuthor{mosub}

Maintainer: \packageMaintainer{mosub}
}
\references{
E. Mosca and L. Milanesi (2013) "Network-Based Analysis of Omics with Multi-Objective Optimization.", Molecular BioSystems 9 (12).
K. Deb, A. Pratap, S. Agarwal and T Meyarivan (2002) "A Fast and Elitist Multiobjective Genetic Algorithm: NSGA-II.", Evolutionary Computation, IEEE Transactions on 6 (2). IEEE: 182–97.
}
\keyword{ package }
\examples{
library(mosub)
data(mosub_data)
res <- mosub_find(gen=10, pop=50, M=2, interactions=interactions,
eval_obj=eval.obj.ee, eval_obj.p=list(exprs1=ge1, exprs2=ge2))
res <- population.postProcess(res, interactions)
}
