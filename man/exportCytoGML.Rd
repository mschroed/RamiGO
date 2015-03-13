\name{exportCytoGML}
\alias{exportCytoGML}
\title{
Writes out an igraph graph to a Cytoscape readable GML file.
}
\description{
Takes the igraph object edited in adjM2gml() and writes it to a GML file that is readable by Cytoscape.
}
\usage{
exportCytoGML(graph, filename)
}
\arguments{
  \item{graph}{
	igraph graph (for example from adjM2gml()).
}
  \item{filename}{
	output filename.
}
}
\author{
Markus Schroeder <mschroed@jimmy.harvard.edu>

Benjamin Haibe-Kains <bhaibeka@jimmy.harvard.edu>
}
\examples{
## set GO ID's and color
goIDs <- c("GO:0051130","GO:0019912","GO:0005783")
color <- c("lightblue","red","yellow")

dd <- getAmigoTree(goIDs=goIDs,color=color,
  filename="example",picType="dot",saveResult=FALSE)
tt <- readAmigoDot(object=dd)

## exportCytoGML is called inside adjM2gml
adjM2gml(adjMatrix(tt),relations(tt)$color,
  annot(tt)$fillcolor,annot(tt)$GO_ID,
  annot(tt)$description,"example")
}
\keyword{ igraph }
\keyword{ graph }
\keyword{ Cytoscape }
\keyword{ network }
