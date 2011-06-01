\name{adjM2gml}
\alias{adjM2gml}
\title{
Create GML file from readAmigoDot output.
}
\description{
Takes part of the output of the readAmigoDot function and creates a GML file by calling exportCytoGML().
}
\usage{
adjM2gml(adjMatrix = NULL, edgecolor = NULL, vertexcolor = NULL, nodelabels = NULL, nodedescription = NULL, filename = NULL)
}
\arguments{
  \item{adjMatrix}{
	\code{adjMatrix} adjacency matrix, partens in rows, children in cols
}
  \item{edgecolor}{
	\code{edgecolor} character vector of the length of existing edges
}
  \item{vertexcolor}{
	\code{vertexcolor} character vector of the length of existing nodes
}
  \item{nodelabels}{
	\code{nodelabels} character vector of the length of existing nodes
}
  \item{nodedescription}{
	\code{nodedescription} character vector of the length of existing nodes
}
  \item{filename}{
	\code{filename} output filenames
}
}
\author{
Markus Schroeder <mschroed@jimmy.harvard.edu>

Benjamin Haibe-Kains <bhaibeka@jimmy.harvard.edu>
}
\examples{
## TODO
}
\keyword{ igraph }
\keyword{ graph }
\keyword{ network }
\keyword{ Cytoscape }
