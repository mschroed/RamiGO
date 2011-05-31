read.amigo.dot <- function(filename=NULL, ...) {
  if(is.null(filename)){ stop("read.amigo.dot: No file specified!") }
  
  ## read dot file
  inputData <- readLines(filename);
  
  ## split data
  metaData <- inputData[grep("\tnode[\\d]+\\s\\[",inputData,perl=TRUE)]
  nodeData <- inputData[grep("\tnode[\\d]+\\s->\\snode[\\d]+\\s\\[",inputData,perl=TRUE)]
  
  ## get meta data
  metaDataValues <- sapply(t(metaData),function(x)strapply(x, "(node[\\d]+).*(GO:[\\d]+)<br/>(.+)</TD>.*color=.(#.+)..\\sfill", c, backref = -4))
  annot <- NULL
  for(i in 1:4){
    annot <- cbind(annot,as.character(sapply(metaDataValues,function(x)x[i])))
  }
  annot <- as.data.frame(annot,stringsAsFactors=FALSE)
  names(annot) <- c("node","GO_ID","description","color")
   
  ## get relations between nodes
  nodeDataValues <-  sapply(nodeData,function(x)strapply(x, "\t(node[\\d]+) -> (node[\\d]+) \\[arrowhead=(.+), arrowtail=(.+), color=(.+), style=(.+)\\];", c, backref = -6))
  relations <- NULL
  for(i in 1:6){
    relations <- cbind(relations,as.character(sapply(nodeDataValues,function(x)x[i])))
  }
  relations <- as.data.frame(relations,stringsAsFactors=FALSE)
  names(relations) <- c("parent","child","arrowhead","arrowtail","color","style")
  
  ## create adjacency matrix
  graph <- matrix(0,nrow(annot),nrow(annot),dimnames=list(annot[,"node"],annot[,"node"]))
  for(i in 1:nrow(relations)){
    graph[relations[i,"parent"],relations[i,"child"]] <- 1
  }
  return(list("graph"=graph,"annot"=annot,"relations"=relations);
}
