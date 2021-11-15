#' Random Generation of an Initial Population of Connected Subnetworks
#' @param pop number of individuals
#' @param M number of objectives
#' @param seeds set of ids to be used as seeds in the generation of connected subnetworks
#' @param mn.size.min minimum size of subnetwork
#' @param mn.size.max maximum size of subnetwork
#' @param interactions data.frame with interactions
#' @param max.new.nodes maximum number of neighbors that will be added at each iteration of the algorithm for the generation of connected subnetworks
#' @param parallel TRUE/FALSE value that specifies wether to use parallel computations
#' @param mc.cores number of cores
#' @return a population of individuals
#' @author Ettore Mosca

population.rnd <- function(pop=NULL, M=NULL, seeds=NULL,  mn.size.max=30, interactions=NULL, mn.size.min=3, max.new.nodes=NULL, parallel=FALSE, mc.cores = mc.cores){
  
  geneClusters <- list()
  
  #for each seed extend a molecular network
  
  if(parallel){
    
    geneClusters <- parallel::mclapply(seeds, mn.extend, mn.size.min=mn.size.min, mn.size.max=mn.size.max, interactions=interactions, max.new.nodes=max.new.nodes, mc.cores = mc.cores)
    
    
  }else{
    
    for(i in 1:length(seeds)) {
      geneClusters[[i]]  <- mn.extend(seeds[i], round(mn.size.max/2), mn.size.max, interactions, max.new.nodes)
    }
  }
  
  geneClusters <- geneClusters[unlist(lapply(geneClusters, length )) > mn.size.min]
  
  #calculate the similarity among the gene sets using the GP BP terms
  similarity <- mn.sim(geneClusters)
  
  #take the least similar p$pop
  temp <- order(apply(similarity, 1, median))[1:pop]
  population <- list(individuals=geneClusters[temp], objectives=matrix(0, pop, M), rank=rep(0, pop), cd=rep(0, pop))
  
  return(population)
  
}

