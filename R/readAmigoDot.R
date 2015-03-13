readAmigoDot <- function(object,filename) {
  if(missing(object) && missing(filename)){
    stop("readAmigoDot: You have to provide something!")
  }
  
  if(!missing(filename)){
    ## read dot file
    inputData <- readLines(filename);
  } else {
    ## split readAmigoTree output string
    inputData <- strsplit(object,"\n")[[1]]
  }
  
  ## split data
  metaData <- inputData[grep("\tnode[\\d]+\\s\\[",inputData,perl=TRUE)]
  nodeData <- inputData[grep("\tnode[\\d]+\\s->\\snode[\\d]+\\s\\[",inputData,perl=TRUE)]
  
  ## get meta data
  metaDataValues <- sapply(t(metaData),function(x)strapply(x, '(node[\\d]+).*(GO:[\\d]+)<br/>(.+)</TD>.*,\\scolor=["]?(#.+[^"]|.+[^"])["]?,\\sfillcolor=["]?(#.+[^"]|.+[^"])["]?,\\sfontcolor=["]?(#.+[^"]|.+[^"])["]?];', c, backref = -6))
  annot <- NULL
  for(i in 1:6){
    annot <- cbind(annot,as.character(sapply(metaDataValues,function(x)x[i])))
  }
  annot <- as.data.frame(annot,stringsAsFactors=FALSE)
  names(annot) <- c("node","GO_ID","description","color","fillcolor","fontcolor")
  annot[,"description"] <- as.character(sapply(annot[,"description"],function(x)paste(strsplit(x,"\\s*\\<br \\/\\>\\s*",perl=TRUE)[[1]],collapse=" ")))
  
  ## get relations between nodes
  nodeDataValues <-  sapply(nodeData,function(x)strapply(x, "\t(node[\\d]+) -> (node[\\d]+) \\[arrowhead=(.+), arrowtail=(.+), color=(.+), style=(.+)\\];", c, backref = -6))
  relations <- NULL
  for(i in 1:6){
    relations <- cbind(relations,as.character(sapply(nodeDataValues,function(x)x[i])))
  }
  relations <- as.data.frame(relations,stringsAsFactors=FALSE)
  names(relations) <- c("parent","child","arrowhead","arrowtail","color","style")
  
  ## sort edges
  parentList <- as.numeric(sapply(relations[,"parent"],function(x)substr(x,5,nchar(x))))
  childList <- as.numeric(sapply(relations[,"child"],function(x)substr(x,5,nchar(x))))
  relationsOrder <- order(parentList,childList)
  relations <- relations[relationsOrder,]
  
  ## create adjacency matrix
  graph <- matrix(0,nrow(annot),nrow(annot),dimnames=list(annot[,"node"],annot[,"node"]))
  for(i in 1:nrow(relations)){
    graph[relations[i,"parent"],relations[i,"child"]] <- 1
  }
  adjMatrix <- graph
  graph <- graph.adjacency(graph,mode="directed")
  
  ## get leaves
  leaves <- annot[!annot[,"node"]%in%unique(relations[,"parent"]),]
  
  return(new("AmigoDot", agraph=graph, adjMatrix=adjMatrix, annot=annot, relations=relations, leaves=leaves))
}
