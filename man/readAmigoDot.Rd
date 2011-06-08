\name{readAmigoDot}
\alias{readAmigoDot}
\title{
Parser for the GraphViz DOT format.
}
\description{
Reads DOT file or takes the output of getAmigoTree (with picType="dot")and returns a graph, adjacency matrix, edges and leaves and also the annotation for the nodes.
}
\usage{
readAmigoDot(filename = NULL, object = NULL)
}
\arguments{
  \item{filename}{
	A .dot file in DOT format.
}
  \item{object}{
	A newline separated string in DOT format (as returned by getAmigoTree).
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
library(RCurl)
library(gsubfn)

## set GO ID's and color
goIDs <- c("GO:0051130","GO:0019912","GO:0005783")
color <- c("lightblue","red","yellow")

## get dot file / object
dd <- getAmigoTree(goIDs=goIDs,color=color,
  filename="example",picType="dot",saveResult=TRUE)

## parse file or object
tt1 <- readAmigoDot(filename="example.dot")
tt2 <- readAmigoDot(object=dd)

## look at results
str(tt1)
str(tt2)
}