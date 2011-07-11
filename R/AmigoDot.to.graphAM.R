AmigoDot.to.graphAM <- function(object){
  adjMat <- adjMatrix(object)
  gAM <- new("graphAM", adjMat=adjMat, "directed")

  return("gAM" = gAM)
}
