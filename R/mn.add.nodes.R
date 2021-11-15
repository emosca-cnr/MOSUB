mn.add.nodes <- function(x=NULL, mn.size.max=30, selected=NULL, interactions=NULL, max.new.nodes=NULL){
  
  #data.table version
  #if the molecular network x is smaller than the max size try to add at most a  number max.new.nodes of interactors
  n.empty <- mn.size.max - length(x)
  
  if(n.empty > 0){
    
    #addition of the best nodes connected to 'node'
    newnodes <- interactions[selected, ]
    newnodes <- newnodes[!(newnodes$b %in% x), with=TRUE] #or: newnodes[!(b %in% x), mult='All']
    
    n.newnodes <- dim(newnodes)[1]
    if(n.newnodes > 0){
      
      n.add <- min(n.empty, max.new.nodes, n.newnodes)
      
      if(n.newnodes > 1){
        x <- c(x, sample(newnodes$b, n.add))
      }else{
        x <- c(x, newnodes$b)
      }
    }
  }
  
  return(x)
}
