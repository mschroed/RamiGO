\name{AmigoDot}
\alias{AmigoDot}
\alias{agraph}
\alias{adjMatrix}
\alias{annot}
\alias{relations}
\alias{leaves}

\title{AmigoDot constructor and accessors}

\description{
  Functions for creating and manipulating AmigoDot-class objects.
}

\usage{
agraph(object, ...)
adjMatrix(object, ...)
annot(object, ...)
relations(object, ...)
leaves(object, ...)
}

\arguments{

  \item{agraph}{An igraph object}

  \item{adjMatrix}{An adjacency matrix.}

  \item{annot}{A data.frame containing the annotation of the tree with
    the columns node, GO_ID, description, color, fillcolor and fontcolor.}

  \item{relations}{A data.frame containing the relations between the node
    of the tree and also information about the edges. The columns are
    parent, child, arrowhead, arrowtail, color and style.}

  \item{leaves}{A data.frame containing the leaves of the tree with
    the columns node, GO_ID, description, color, fillcolor and fontcolor.}
}

\value{

  \item{AmigoDot}{Object of class
    \code{\linkS4class{AmigoDot}}.}

  \item{agraph}{igraph object.}

  \item{adjMatrix}{Adjacency Matrix.}

  \item{annot, leaves}{Annotation for each node.}

  \item{relations}{Meta information about the edges between nodes.}

}

\author{Markus Schroeder \url{mschroed@jimmy.harvard.edu}}

\seealso{\code{\linkS4class{AmigoDot}-class}}

\examples{
## set GO ID's and color
goIDs <- c("GO:0051130","GO:0019912","GO:0005783")
color <- c("lightblue","red","yellow")

dd <- getAmigoTree(goIDs=goIDs,color=color,
  filename="example",picType="dot",saveResult=FALSE)
tt <- readAmigoDot(object=dd)

agraph(tt)
adjMatrix(tt)
annot(tt)
relations(tt)
leaves(tt)
}

\keyword{classes}
