\name{AmigoDot-class}
\docType{class}
\alias{AmigoDot-class}

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
\section{Methods}{
  See \code{\link{AmigoDot}}
}

\author{Markus Schroeder \url{mschroed@jimmy.harvard.edu}}

\examples{
showClass("AmigoDot")
}
\keyword{classes}
