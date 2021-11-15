mn.check <- function(x=NULL, interactions=NULL, mn.size.max=30, mn.size.min=3, max.new.nodes=NULL){
  
  x.old <- x
  
  temp <- expand.grid(x, x)
  g.df <- interactions[ J(temp[,1], temp[,2])]
  g.df <- g.df[!is.na(g.df$score), ]
  data.table::setkey(g.df, a, b, score)
  
  g <- igraph::graph.data.frame(g.df)
  
  #remove singletons
  singletons <- which(!(x %in% g.df$a) & !(x %in% g.df$b))
  if(length(singletons) > 0){
    x <- x[-singletons]
  }
  
  if(length(igraph::V(g)$name) > 0){
    
    #if there is a graph
    #if the graph is not connected
    #keeps the largest subgraph
    #if it is too small try to extend
    #if it is not possible to extend get a rnd.graph
    
    if(!igraph::is.connected(g)){
      
      gg.cl <- igraph::clusters(g)
      
      #find the largest subgraph (random if equal)
      max.size.cl.idx <- which(gg.cl$csize == max(gg.cl$csize))
      
      if(length(max.size.cl.idx) > 1)
        max.size.cl.idx <- max.size.cl.idx[sample(1:length(max.size.cl.idx), 1)]
      
      if(min(gg.cl$membership)==0)
        max.size.cl.idx <- max.size.cl.idx-1
      
      x <- igraph::V(g)$name[which(gg.cl$membership == max.size.cl.idx)]
      
      if(length(x) >=  mn.size.min){       
        return(x)
      }else{
        x <- mn.extend(x, mn.size.min, mn.size.max, interactions, max.new.nodes)
        return(x)
      }
    }else{
      #if the graph is connected but the num of nodes < than min fixes the graph
      if(length(x) <  mn.size.min){       
        x <- mn.extend(x, mn.size.min, mn.size.max, interactions, max.new.nodes)
        return(x)
      }else{
        return(x)
      }
    }
  }else{

    #if there is no graph
    #creates a new graph from one of the initial nodes
    #if there are many initial nodes try them

    x <- list()
    if(length(x.old) > 1){
      i <- 0
      while((length(x) <  mn.size.min) & (i < length(x.old))) {
        x <- mn.extend(sample(x.old, 1), mn.size.min, mn.size.max, interactions, max.new.nodes)
        i <- i + 1
      }
    }else{
      x <- mn.extend(x.old, mn.size.min, mn.size.max, interactions, max.new.nodes)
    }
    
    #it is still possible that a graph is generated less than mn.size.min
    if(length(x) <  mn.size.min){
      print(x)
      cat("WARNING: in mn.check(), individual generated with less than ", mn.size.min," nodes\n")
    }
    return(x)
  }
}
