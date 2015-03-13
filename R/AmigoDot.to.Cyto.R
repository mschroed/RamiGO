AmigoDot.to.Cyto <- function(object){
  gg <- adjM2gml(adjMatrix=adjMatrix(object),edgecolor=relations(object)$color,
    vertexcolor=annot(object)$fillcolor,nodelabels=annot(object)$GO_ID,
    nodedescription=annot(object)$description)

  ggCyto <- igraph.to.graphNEL(gg)

  attr(edgeDataDefaults(ggCyto, attr="weight"), "class") = "INTEGER"
  attr(edgeDataDefaults(ggCyto, attr="color"), "class") = "STRING"
  attr(nodeDataDefaults(ggCyto, attr="description"), "class") = "STRING"
  attr(nodeDataDefaults(ggCyto, attr="color"), "class") = "STRING"

  cw <- CytoscapeWindow('RamiGO', ggCyto)
  displayGraph(cw)
  layoutNetwork(cw, layout.name = "hierarchical")
}
