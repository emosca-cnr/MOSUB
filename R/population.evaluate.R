population.evaluate <- function(population=NULL, parallel=FALSE, eval_obj=NULL, eval_obj.p=NULL, mc.cores = mc.cores){
  
  
 if(parallel){

       output <- parallel::mclapply(population$individuals, eval_obj, eval_obj.p=eval_obj.p, mc.cores = mc.cores)
    population$objectives <- do.call(rbind, output)
    
  }else{
    
    population$objectives <- do.call(rbind, lapply(population$individuals, eval_obj, eval_obj.p))
    
  }
  
  return(population)
}