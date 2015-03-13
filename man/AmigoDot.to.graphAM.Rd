\name{AmigoDot.to.graphAM}
\alias{AmigoDot.to.graphAM}
\title{
Converts an AmigoDot S4 object to a graphAM object.
}
\description{
Converts an AmigoDot S4 object to a graphAM object.
}
\usage{
AmigoDot.to.graphAM(object)
}
\arguments{
  \item{object}{
	is a AmigoDot S4 object.
}
}
\value{
  \item{gAM}{
    is a graphAM object.
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
AmigoDot.to.graphAM(tt)
}
