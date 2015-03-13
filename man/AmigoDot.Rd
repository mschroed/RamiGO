\name{AmigoDot}
\alias{AmigoDot}
\alias{agraph}
\alias{agraph<-}
\alias{adjMatrix}
\alias{adjMatrix<-}
\alias{annot}
\alias{annot<-}
\alias{relations}
\alias{relations<-}
\alias{leaves}
\alias{leaves<-}
% Accessors:
\alias{agraph,AmigoDot-method}
\alias{agraph<-,AmigoDot-method}
\alias{adjMatrix,AmigoDot-method}
\alias{adjMatrix<-,AmigoDot-method}
\alias{annot,AmigoDot-method}
\alias{annot<-,AmigoDot-method}
\alias{relations,AmigoDot-method}
\alias{relations<-,AmigoDot-method}
\alias{leaves,AmigoDot-method}
\alias{leaves<-,AmigoDot-method}

\title{AmigoDot constructor and accessors}

\description{
  Functions for creating and manipulating AmigoDot-class objects.
}

\usage{
agraph(object)
agraph(object) <- value
adjMatrix(object)
adjMatrix(object) <- value
annot(object)
annot(object) <- value
relations(object)
relations(object) <- value
leaves(object)
leaves(object) <- value
}

\arguments{
  \item{object}{An AmigoDot S4 object}

  \item{value}{An AmigoDot S4 object}
}
\details{
  \code{agraph(object)}{Gets the igraph object.}

  \code{agraph(object) <- value}{Sets the igraph object.}

  \code{adjMatrix(object)}{Gets the adjacency matrix.}

  \code{adjMatrix(object) <- value}{Sets the adjacency matrix.}

  \code{annot(object)}{Gets the data.frame containing the annotation of the tree with
    the columns node, GO_ID, description, color, fillcolor and fontcolor.}

  \code{annot(object) <- value}{Sets the the data.frame containing the annotation of the tree with
    the columns node, GO_ID, description, color, fillcolor and fontcolor.}

  \code{relations(object)}{Gets the data.frame containing the relations between the node
    of the tree and also information about the edges. The columns are
    parent, child, arrowhead, arrowtail, color and style.}

  \code{relations(object) <- value}{Sets the the data.frame containing the relations between the node
    of the tree and also information about the edges. The columns are
    parent, child, arrowhead, arrowtail, color and style.}

  \code{leaves(object)}{Gets the data.frame containing the leaves of the tree with
    the columns node, GO_ID, description, color, fillcolor and fontcolor.}

  \code{leaves(object) <- value}{Sets the the data.frame containing the leaves of the tree with
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

\seealso{\code{\linkS4class{AmigoDot}-class} \code{\link[igraph:igraph]{igraph}}}

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
