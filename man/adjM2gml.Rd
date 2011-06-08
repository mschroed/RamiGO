\name{adjM2gml}
\alias{adjM2gml}
\title{
Create GML file from readAmigoDot output.
}
\description{
Takes a part of the output of the readAmigoDot function and creates a GML file by calling exportCytoGML().
}
\usage{
adjM2gml(adjMatrix = NULL, edgecolor = NULL,
  vertexcolor = NULL, nodelabels = NULL,
  nodedescription = NULL, filename = NULL)
}
\arguments{
  \item{adjMatrix}{
	adjacency matrix, partens in rows, children in cols.
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
library(RamiGO)
library(RCurl)
library(igraph)
## set GO ID's and color
goIDs <- c("GO:0051130","GO:0019912","GO:0005783")
color <- c("lightblue","red","yellow")

dd <- getAmigoTree(goIDs=goIDs,color=color,
  filename="example",picType="dot",saveResult=FALSE)
tt <- readAmigoDot(object=dd)

adjM2gml(tt$adjMatrix,tt$relations$color,
  tt$annot$fillcolor,tt$annot$GO_ID,
  tt$annot$description,"example")
}
\keyword{ igraph }
\keyword{ graph }
\keyword{ network }
\keyword{ Cytoscape }
