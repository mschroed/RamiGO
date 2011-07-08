\name{AmigoDot-class}
\docType{class}
\alias{AmigoDot-class}
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

\title{Class "AmigoDot"}
\description{
  AmigoDot represents a collection of objects that are extracted
  from an GraphViz DOT format file.
}
\section{Objects from the Class}{

  Objects can be created by calls to the constructor
  \code{\link{AmigoDot}}.

  Elements can be accessed with \code{\link{agraph}},
  \code{\link{adjMatrix}},\code{\link{annot}},
  \code{\link{relations}}, and \code{\link{leaves}}.

}
\section{Slots}{
  \describe{

    \item{\code{agraph}:}{Object of class \code{"igraph"} containing
      the graph extracted from the DOT format file.}

    \item{\code{adjMatrix}:}{Object of class \code{"matrix"}
      containing the adjacency matrix of the tree.}

    \item{\code{annot}:}{Object of class \code{"data.frame"}
      containing the annotation of the tree.}

    \item{\code{relations}:}{Object of class \code{"data.frame"}
      containing the relations between the node of the tree.}

    \item{\code{leaves}:}{Object of class \code{"data.frame"}
      containing the leaves of the tree.}
  }
}
\section{Accessors}{
  In the following code snippets, \code{x} is an AmigoDot object.

  \describe{
    \item{}{
      \code{agraph(x)}, \code{agraph(x) <- value}:
      Gets or sets the igraph object.
    }
    \item{}{
      \code{adjMatrix(x)}, \code{adjMatrix(x) <- value}:
      Gets or sets the adjacency matrix.
    }
    \item{}{
      \code{annot(x)}, \code{annot(x) <- value}:
      Gets or sets the annotation data.frame for the node of the tree.
    }
    \item{}{
      \code{relations(x)}, \code{relations(x) <- value}:
      Gets or sets the relations data.frame between the nodes.
    }
    \item{}{
      \code{leaves(x)}, \code{leaves(x) <- value}:
      Gets or sets annotation of the leaves of the tree.
    }
  }
}
\section{Methods}{

  See \code{\link{AmigoDot}}

}

\author{Markus Schroeder \url{mschroed@jimmy.harvard.edu}}

\examples{
showClass("AmigoDot")
}
\keyword{classes}
