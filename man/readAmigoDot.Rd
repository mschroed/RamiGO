\name{readAmigoDot}
\alias{readAmigoDot}
\title{
Parser for the GraphViz DOT format.
}
\description{
Reads DOT file and returns a graph, adjacency matrix, edges and leaves and also the annotation for the nodes.
}
\usage{
readAmigoDot(filename = NULL)
}
\arguments{
  \item{filename}{
	A .dot file in DOT format.
}
}
\value{
\item{graph }{A graph object}
\item{adjMatrix }{An adjacency matrix}
\item{annot }{The node annotation}
\item{relations }{The edges of the graph}
\item{leaves }{The leaves of the graph}
}
\author{
Markus Schroeder <mschroed@jimmy.harvard.edu>
}
\examples{
getAmigoTree(goIDs=c("GO:0051130","GO:0019912","GO:0005783"),color=c("lightblue","red","yellow"),picType="dot",filename="example.dot")
tt <- readAmigoDot("example.dot")
str(tt)
}