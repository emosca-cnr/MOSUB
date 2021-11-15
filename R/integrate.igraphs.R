integrate.igraphs <- function(x){
  
  all.edges <- do.call(rbind, lapply(x, igraph::get.edgelist))
  all.edges <- t(apply(all.edges, 1, sort))
  all.edges.N <- dim(all.edges)[1]
  all.edges.array <- apply(all.edges, 1, paste, collapse=':::')
  edges.duplicated <- unique(all.edges.array[duplicated(all.edges.array)])
  all.edges.freq <- data.frame(edge=all.edges.array[!duplicated(all.edges.array)], freq=1/all.edges.N, stringsAsFactors=FALSE)
  for(i in 1:length(edges.duplicated))
    all.edges.freq$freq[all.edges.freq$edge == edges.duplicated[i]] <- length(which(all.edges.array==edges.duplicated[i])) / all.edges.N
  all.edges.freq <- data.frame(do.call(rbind, sapply(all.edges.freq$edge, strsplit, '\\:::')), freq=all.edges.freq$freq, stringsAsFactors=FALSE)
  y <- igraph::graph.data.frame(all.edges.freq, directed=FALSE)
}

