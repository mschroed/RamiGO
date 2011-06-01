\name{getAmigoTree}
\alias{getAmigoTree}
\title{
Getting the DAG GO tree for a set of GO ID's.
}
\description{
Getting the DAG GO tree for a set of GO ID's.
}
\usage{
getAmigoTree(picType = "png", modeType = "basic", goIDs = NULL, color = NULL, filename = NULL)
}
\arguments{
  \item{picType}{
	\code{picType} supports "png","svg" and "dot"
}
  \item{modeType}{
	\code{modeType} is the amiGO query mode, only basic supported
}
  \item{goIDs}{
	\code{goIDs} is a vector of GO ID's
}
  \item{color}{
	\code{color} is a vector of colors()
}
  \item{filename}{
	\code{filename} is the output filename
}
}
\value{
No return value.
}
\author{
Markus Schroeder <mschroed@jimmy.harvard.edu>
}
\examples{
getAmigoTree(goIDs=c("GO:0051130","GO:0019912","GO:0005783"),color=c("lightblue","red","yellow"))
}