#' Identification of multi-optimal subnetworks
#' @param gen number of generations; if a vector, the population is saved at each given generation
#' @param pop number of individuals;
#' @param M number of objectives
#' @param mn.size.max maximum subnetwork size
#' @param pool number of individuals allowed to participate at tournament selection; if not provided it is set to \code{round(pop/2)};
#' @param population a list with the elements: \code{individuals}, a list of \code{pop} vectors of identifiers, where each vector represents an individual of the population; \code{objectives}, an empty numeric matrix of size \code{pop}-by-\code{M} for the values of the objectives functions; \code{rank} an empty numeric vector of length \code{pop} for individuals ranks;  \code{cd} an empty numeric vector of length \code{pop} for individuals crowding distance; if not provided, the population is generated using \code{\link{population.rnd}};
#' @param population.seeds vector of IDs that are used by \code{\link{population.rnd}} as seeds during the generation of individuals; the length of population.seeds must be >= \code{pop}; if the length of \code{population.seeds} is > \code{pop}, then only the most different individuals are selected for the initial population; in this case, the similarity between two individuals is calculated as the fraction of molecule IDs in common;
#' @param interactions a data frame with three columns, \code{a} and \code{b} for ids and \code{score} for the interaction score; a given tuple (a=4, b=7, score=0.5) sould be present also as (a=7, b=4, score=0.5);
#' @param eval_obj a function for evaluating the objectives; see \code{\link{eval.obj.ee}};
#' @param eval_obj.p a list for collecting the problem specific arguments of \code{eval_obj};
#' @param ID2SYMBOL a data frame with columns \code{ID} and \code{SYMBOL} for mapping the molecule IDs to symbols;
#' @param tour number of individual that participate in tournament selection;
#' @param crossover.prob probability of crossover; mutation probaility will be 1-\code{crossover.prob};
#' @param mutation.fraction fraction of the individual (i.e. number of variables) that will be mutated once an individual is selected for mutation;
#' @param mutation.fraction.del value in (0,1) used to set \code{max.add.nodes}, the maximum number of nodes that can be attached to an individual selected for mutation;  \code{max.add.nodes} is calculated as \code{mutation.fraction.del} / (1 - \code{mutation.fraction.del});
#' @param mn.size.min minimum number of nodes an individual must have; note that the interaction data can not contain any subgraph with a number of node smaller than \code{mn.size.min}
#' @param parallel TRUE/FALSE;
#' @param mc.cores number of cores;
#' @param INDEX unique integer value that identifies the current run of \code{mnnsga2}; if \code{INDEX} != -1 the migration of individuals is enabled;
#' @param migrants.fraction fraction of the population that is shared with other instances of \code{mnnsga2}; used only if \code{INDEX} != -1;
#' @param migration.freq  number of generations defining the frequency of migration of individuals; used only if \code{INDEX} != -1;
#' @param mysql.user MySQL database user name; required only if \code{INDEX} != -1;
#' @param mysql.pass MySQL database password; required only if \code{INDEX} != -1;
#' @param mysql.db MySQL database name; required only if \code{INDEX} != -1;
#' @param mysql.host MySQL database host; required only if \code{INDEX} != -1;
#' @param mysql.table MySQL database table; required only if \code{INDEX} != -1;
#' @param verbose it can be 0, 1 or 2 and regulates the verbosity.
#' @param seed for random number generation.

#' @return A list with the follownig elements: individuals, list of \code{pop} vectors representing the individuals; objectives, numeric matrix of size \code{pop}-by-\code{M} with the values of the objectives functions; rank, numeric vector of length \code{pop} for individuals ranks; cd, numeric vector of length \code{pop} for individuals crowding distance;
#' @references
#' Mosca, E. and Milanesi, L. 2013. Network-based analysis of omics with multi-objective optimization. Molecular BioSystems 9 (12), 2971-2980.
#' 
#' Deb, K.; Pratap, A.; Agarwal, S.; Meyarivan, T. 2002. A fast and elitist multiobjective genetic algorithm: NSGA-II, Evolutionary Computation, IEEE Transactions on , vol.6, no.2, pp.182-197.
#' @author \packageAuthor{mosub}
#' @seealso \code{\link{eval.obj.ee}}, \code{\link{population.rnd}}.
#' @examples
#' library(mosub)
#' data(mosub_data)
#' res <- mosub_find(gen=10, pop=50, M=2, interactions=interactions,
#' eval_obj=eval.obj.ee, eval_obj.p=list(exprs1=ge1, exprs2=ge2))
#' res <- population.postProcess(res, interactions)
#' @export
#' @import RMySQL
#' @import igraph
#' @import randtoolbox
#' @import parallel
#' @import data.table
mosub_find <- function (gen=NULL, pop=NULL, M=NULL, mn.size.max=30, pool=NULL, population=NULL, population.seeds=NULL, interactions=NULL, eval_obj=NULL, eval_obj.p=NULL, ID2SYMBOL=NULL, tour=2, crossover.prob=0.1, mutation.fraction=0.1, mutation.fraction.del = 0.75, mn.size.min=3, parallel=FALSE, mc.cores=2, INDEX=-1, migrants.fraction=0.05, migration.freq=10, mysql.user=NULL, mysql.pass=NULL, mysql.db=NULL, mysql.host=NULL, mysql.table=NULL, verbose=1, seed=1234){
  
  
  if (is.null(pop) | is.null(gen) | is.null(M) | is.null(mn.size.max) | is.null(interactions) | is.null(eval_obj))
    stop("ERROR: pop, gen, M,  mn.size.max, interactions and eval_obj are mandatory.")
  if (pop < 5) 
    stop("ERROR: pop < 5.")
  if (is.null(pool))
    pool <- round(pop/2)
  
  if(!any(class(interactions) == 'data.table')){
    print("Defining interactions as data.table")
    interactions <- data.table::data.table(interactions)
    data.table::setkey(interactions, a, b)
  }
  
  
  if(INDEX != -1){
    mysql.con <- RMySQL::dbConnect(RMySQL::MySQL(), user=mysql.user, password=mysql.pass, dbname=mysql.db, host=mysql.host)
    RMySQL::dbDisconnect(mysql.con)
  }
  
  set.seed(seed)
  
  max.new.nodes <- mutation.fraction.del / (1 - mutation.fraction.del)   
  pop <- round(pop)
  last.gen <- gen[length(gen)]
  err.pop <- "problem with population\n"
  
  #INITIALIZATION
  
  cat("initialization\n")
  if(is.null(population)){
    #generate population    
    if(is.null(population.seeds)){
      IDS <- unique(c(interactions$a, interactions$b))
      population.seeds <- sample(IDS, min(2*pop, length(IDS)))    
    }
    population <- population.rnd(pop, M, population.seeds, mn.size.max, interactions, mn.size.min, max.new.nodes, parallel, mc.cores = mc.cores)
  }
  
  if(!population.check(population, pop, M, mn.size.max, mn.size.min)){
    print(population)
    stop(err.pop)
  }
  
  ## INITIAL EVALUATION AND SORTING
  population <- population.evaluate(population, parallel, eval_obj, eval_obj.p, mc.cores = mc.cores)
  population <- non_domination_sort_mod(population, parallel, mc.cores)
  save(population, file = "pop.0.Rdata")
  
  ## MAIN LOOP: SIMULATED EVOLUTION
  cat('evolution\n')
  for (curr.gen in 1:last.gen) {
    
    if(curr.gen %in% gen)
      cat(Sys.time(), ": generation ", curr.gen, "\n")
    
    ##MIGRATION
    if((INDEX != -1) & ((curr.gen %% migration.freq) ==0) ){
      print('migration')
      population <- doMigration(population, INDEX, curr.gen, migrants.fraction, mysql.user, mysql.pass, mysql.db, mysql.host, mysql.table)
      population <- non_domination_sort_mod(population, parallel, mc.cores)
    }
    
    ##TOURNAMENT SELECTION
    parents <- tournament_selection(population, pool, tour)
    
    ##CREATE THE OFFSPRING APPLYING THE GENETIC OPERATOR
    offspring <- genetic_operator(parents, interactions, mn.size.max, crossover.prob, mutation.fraction, mutation.fraction.del, mn.size.min, max.new.nodes, mc.cores=mc.cores)
    offspring_population <- list(individuals=offspring, objectives=matrix(0, length(offspring), M), rank=rep(0, length(offspring)), cd=rep(0, length(offspring)))
    
    ##EVALUATION OF THE OFFSPRING
    offspring_population <- population.evaluate(offspring_population, parallel, eval_obj, eval_obj.p, mc.cores = mc.cores)

    ##CREATION AND SORTING OF THE INTERMEDIATE POPULATION: POPULATION + OFFSPRING
    intermediate_population <- list(individuals=c(population$individuals, offspring_population$individuals), objectives=rbind(population$objectives, offspring_population$objectives), rank=c(population$rank, offspring_population$rank), cd=c(population$cd, offspring_population$cd))
    
    duplicated.idx <- which(duplicated(intermediate_population$individuals))
    if(length(duplicated.idx)>0)
      intermediate_population <- list(individuals=intermediate_population$individuals[-duplicated.idx], objectives=intermediate_population$objectives[-duplicated.idx, ], rank=intermediate_population$rank[-duplicated.idx], cd=intermediate_population$cd[-duplicated.idx])
    
    intermediate_population <- non_domination_sort_mod(intermediate_population, parallel, mc.cores = mc.cores)
    
    
    ##REPLACE POPULATION
    population <- replace_population(intermediate_population, pop)
    if(!population.check(population, pop, M, mn.size.max, mn.size.min)){
      print(population)
      stop(err.pop)
    }
    
    ##SAVE AND PRINT
    
    if(curr.gen %in% gen){
      
      if(verbose == 2){
        cat('individuals in the Pareto front\n')
        if(!is.null(ID2SYMBOL)){
          print(lapply(population$individuals[population$rank == 1], function(xx) ID2SYMBOL$SYMBOL[ID2SYMBOL$ID %in% xx]))
        }else{
          print(population$individuals[population$rank == 1])
        }
      }
      
      if(verbose > 1){
        cat('objectives in the Pareto front\n')
        print(population$objectives[population$rank == 1, ])
      }
      
      if(verbose > 0){
        temp <- population$objectives[population$rank == 1, ]
        if(!is.null(dim(temp)[1])){
          cat('summary of objectives in the Pareto front\n')      
          print(apply(temp, 2, summary))
        }
      }
      
      save(population, file = paste("pop.", curr.gen, ".RData", sep = ""))
    }
    
  }
  
  return(population)
}