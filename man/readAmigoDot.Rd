\name{readAmigoDot}
\alias{readAmigoDot}
\title{
Parser for the GraphViz DOT format.
}
\description{
Reads DOT file or takes the output of getAmigoTree (with picType="dot") and returns an AmigoDot S4 object with a graph, adjacency matrix, edges and leaves and also the annotation for the nodes.
}
\usage{
readAmigoDot(object, filename)
}
\arguments{
  \item{object}{
	A newline separated string in DOT format (as returned by getAmigoTree).
}
  \item{filename}{
	A .dot file in DOT format.
}
}
\value{
\item{}{An AmigoDot S4 object.}
}
\author{
Markus Schroeder <mschroed@jimmy.harvard.edu>
}
\examples{
## set GO ID's and color
goIDs <- c("GO:0051130","GO:0019912","GO:0005783")
color <- c("lightblue","red","yellow")

## get dot file / object
dd <- getAmigoTree(goIDs=goIDs,color=color,
  filename="example",picType="dot",saveResult=TRUE)

## parse file or object
#tt1 <- readAmigoDot(filename="example.dot")
tt2 <- readAmigoDot(object=dd)

## look at results
#show(tt1)
show(tt2)
}
