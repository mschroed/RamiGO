AmigoDot.to.graphNEL <- function(object){
  gg <- adjM2gml(adjMatrix=adjMatrix(object),edgecolor=relations(object)$color,
    vertexcolor=annot(object)$fillcolor,nodelabels=annot(object)$GO_ID,
    nodedescription=annot(object)$description)

  gNEL <- igraph.to.graphNEL(gg)

  attr(edgeDataDefaults(gNEL, attr="weight"), "class") = "INTEGER"
  attr(edgeDataDefaults(gNEL, attr="color"), "class") = "STRING"
  attr(nodeDataDefaults(gNEL, attr="description"), "class") = "STRING"
  attr(nodeDataDefaults(gNEL, attr="color"), "class") = "STRING"

  return("gNEL" = gNEL)
}
