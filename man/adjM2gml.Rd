\name{adjM2gml}
\alias{adjM2gml}
\title{
Create GML file from readAmigoDot output.
}
\description{
Takes a part of the output of the readAmigoDot function and creates a GML file by calling exportCytoGML().
}
\usage{
adjM2gml(adjMatrix, edgecolor, vertexcolor,
  nodelabels, nodedescription, filename)
}
\arguments{
  \item{adjMatrix}{
	adjacency matrix, parents in rows, children in cols.
}
  \item{edgecolor}{
	character vector of the length of existing edges.
}
  \item{vertexcolor}{
	character vector of the length of existing nodes.
}
  \item{nodelabels}{
	character vector of the length of existing nodes.
}
  \item{nodedescription}{
	character vector of the length of existing nodes.
}
  \item{filename}{
	output filenames.
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

adjM2gml(adjMatrix(tt),relations(tt)$color,
  annot(tt)$fillcolor,annot(tt)$GO_ID,
  annot(tt)$description,"example")
}
\keyword{ igraph }
\keyword{ graph }
\keyword{ network }
\keyword{ Cytoscape }
