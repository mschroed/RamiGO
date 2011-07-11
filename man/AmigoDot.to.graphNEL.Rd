\name{AmigoDot.to.graphNEL}
\alias{AmigoDot.to.graphNEL}
\title{
Converts an AmigoDot S4 object to a graphNEL object.
}
\description{
Converts an AmigoDot S4 object to a graphNEL object.
}
\usage{
AmigoDot.to.graphNEL(object)
}
\arguments{
  \item{object}{
	is a AmigoDot S4 object.
}
}
\value{
  \item{gNEL}{
    is a graphNEL object.
}
}
\author{
Markus Schroeder <mschroed@jimmy.harvard.edu>
}
\examples{
## set GO ID's and color
goIDs <- c("GO:0051130","GO:0019912","GO:0005783")
color <- c("lightblue","red","yellow")

dd <- getAmigoTree(goIDs=goIDs,color=color,
  filename="example",picType="dot",saveResult=FALSE)
tt <- readAmigoDot(object=dd)
AmigoDot.to.graphNEL(tt)
}

