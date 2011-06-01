## function to export a network to a Cytoscape importable gml file
## object: adjacency matrix
## returns a cytoscape importable object
adjM2gml <- function(adjMatrix=NULL, edgecolor=NULL, vertexcolor=NULL, nodelabels=NULL, nodedescription=NULL, filename=NULL) {
	## adjacency matrix representing the topology; parents in rows, children in columns
	net.topo <- adjMatrix
	if(any(dim(net.topo) <= 0)) { stop("network should contain at least one node!") }
	
	## create igraph object
	net.igraph <- graph.adjacency(adjmatrix=net.topo, mode="directed")
	
	## convert colors into hex
	convCol <- function(x){
    ss <- col2rgb(x)
    return(sprintf("#%02X%02X%02X",ss[1],ss[2],ss[3]))
  }
	
  ## colors for the edges
	if(!is.null(edgecolor)){
    ccSelect <- regexpr("#",edgecolor) < 0
    ccData <- edgecolor[ccSelect]
    if(length(ccData) != 0){
      ccData <- as.character(sapply(ccData,function(x)convCol(x)))
    }
    edgecolor[ccSelect] <- ccData
    E(net.igraph)$color <- edgecolor
  } else {
    E(net.igraph)$color <- rep("#000000", length(E(net.igraph)))
  }
    
  ## node labels
  if(!is.null(nodelabels)){
    V(net.igraph)$name <- sprintf('"%s"',nodelabels)
  } else {
    V(net.igraph)$name <- sprintf('"%s"',V(net.igraph)$name)
  }
  
  ## node description
  if(!is.null(nodedescription)){
    V(net.igraph)$description <- sprintf('"%s"',nodedescription)
  }
  
  ## colors for the vertexes
  if(!is.null(vertexcolor)){
    ccSelect <- regexpr("#",vertexcolor) < 0
    ccData <- vertexcolor[ccSelect]
    if(length(ccData) != 0){
      ccData <- as.character(sapply(ccData,function(x)convCol(x)))
    }
    vertexcolor[ccSelect] <- ccData
    V(net.igraph)$color <- vertexcolor
  } else {
    V(net.igraph)$color <- rep("#0099ff", length(V(net.igraph)))
  }
	exportCytoGML(graph=net.igraph, filename=sprintf("%s.gml", filename))
}
