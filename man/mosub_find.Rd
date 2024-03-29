% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mosub_find.R
\name{mosub_find}
\alias{mosub_find}
\title{Identification of multi-optimal subnetworks}
\usage{
mosub_find(
  gen = NULL,
  pop = NULL,
  M = NULL,
  mn.size.max = 30,
  pool = NULL,
  population = NULL,
  population.seeds = NULL,
  interactions = NULL,
  eval_obj = NULL,
  eval_obj.p = NULL,
  ID2SYMBOL = NULL,
  tour = 2,
  crossover.prob = 0.1,
  mutation.fraction = 0.1,
  mutation.fraction.del = 0.75,
  mn.size.min = 3,
  parallel = FALSE,
  mc.cores = 2,
  INDEX = -1,
  migrants.fraction = 0.05,
  migration.freq = 10,
  mysql.user = NULL,
  mysql.pass = NULL,
  mysql.db = NULL,
  mysql.host = NULL,
  mysql.table = NULL,
  verbose = 1,
  seed = 1234
)
}
\arguments{
\item{gen}{number of generations; if a vector, the population is saved at each given generation}

\item{pop}{number of individuals;}

\item{M}{number of objectives}

\item{mn.size.max}{maximum subnetwork size}

\item{pool}{number of individuals allowed to participate at tournament selection; if not provided it is set to \code{round(pop/2)};}

\item{population}{a list with the elements: \code{individuals}, a list of \code{pop} vectors of identifiers, where each vector represents an individual of the population; \code{objectives}, an empty numeric matrix of size \code{pop}-by-\code{M} for the values of the objectives functions; \code{rank} an empty numeric vector of length \code{pop} for individuals ranks;  \code{cd} an empty numeric vector of length \code{pop} for individuals crowding distance; if not provided, the population is generated using \code{\link{population.rnd}};}

\item{population.seeds}{vector of IDs that are used by \code{\link{population.rnd}} as seeds during the generation of individuals; the length of population.seeds must be >= \code{pop}; if the length of \code{population.seeds} is > \code{pop}, then only the most different individuals are selected for the initial population; in this case, the similarity between two individuals is calculated as the fraction of molecule IDs in common;}

\item{interactions}{a data frame with three columns, \code{a} and \code{b} for ids and \code{score} for the interaction score; a given tuple (a=4, b=7, score=0.5) sould be present also as (a=7, b=4, score=0.5);}

\item{eval_obj}{a function for evaluating the objectives; see \code{\link{eval.obj.ee}};}

\item{eval_obj.p}{a list for collecting the problem specific arguments of \code{eval_obj};}

\item{ID2SYMBOL}{a data frame with columns \code{ID} and \code{SYMBOL} for mapping the molecule IDs to symbols;}

\item{tour}{number of individual that participate in tournament selection;}

\item{crossover.prob}{probability of crossover; mutation probaility will be 1-\code{crossover.prob};}

\item{mutation.fraction}{fraction of the individual (i.e. number of variables) that will be mutated once an individual is selected for mutation;}

\item{mutation.fraction.del}{value in (0,1) used to set \code{max.add.nodes}, the maximum number of nodes that can be attached to an individual selected for mutation;  \code{max.add.nodes} is calculated as \code{mutation.fraction.del} / (1 - \code{mutation.fraction.del});}

\item{mn.size.min}{minimum number of nodes an individual must have; note that the interaction data can not contain any subgraph with a number of node smaller than \code{mn.size.min}}

\item{parallel}{TRUE/FALSE;}

\item{mc.cores}{number of cores;}

\item{INDEX}{unique integer value that identifies the current run of \code{mnnsga2}; if \code{INDEX} != -1 the migration of individuals is enabled;}

\item{migrants.fraction}{fraction of the population that is shared with other instances of \code{mnnsga2}; used only if \code{INDEX} != -1;}

\item{migration.freq}{number of generations defining the frequency of migration of individuals; used only if \code{INDEX} != -1;}

\item{mysql.user}{MySQL database user name; required only if \code{INDEX} != -1;}

\item{mysql.pass}{MySQL database password; required only if \code{INDEX} != -1;}

\item{mysql.db}{MySQL database name; required only if \code{INDEX} != -1;}

\item{mysql.host}{MySQL database host; required only if \code{INDEX} != -1;}

\item{mysql.table}{MySQL database table; required only if \code{INDEX} != -1;}

\item{verbose}{it can be 0, 1 or 2 and regulates the verbosity.}

\item{seed}{for random number generation.}
}
\value{
A list with the follownig elements: individuals, list of \code{pop} vectors representing the individuals; objectives, numeric matrix of size \code{pop}-by-\code{M} with the values of the objectives functions; rank, numeric vector of length \code{pop} for individuals ranks; cd, numeric vector of length \code{pop} for individuals crowding distance;
}
\description{
Identification of multi-optimal subnetworks
}
\examples{
library(mosub)
data(mosub_data)
res <- mosub_find(gen=10, pop=50, M=2, interactions=interactions,
eval_obj=eval.obj.ee, eval_obj.p=list(exprs1=ge1, exprs2=ge2))
res <- population.postProcess(res, interactions)
}
\references{
Mosca, E. and Milanesi, L. 2013. Network-based analysis of omics with multi-objective optimization. Molecular BioSystems 9 (12), 2971-2980.

Deb, K.; Pratap, A.; Agarwal, S.; Meyarivan, T. 2002. A fast and elitist multiobjective genetic algorithm: NSGA-II, Evolutionary Computation, IEEE Transactions on , vol.6, no.2, pp.182-197.
}
\seealso{
\code{\link{eval.obj.ee}}, \code{\link{population.rnd}}.
}
\author{
\packageAuthor{mosub}
}
