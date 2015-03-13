\name{AmigoDot.to.Cyto}
\alias{AmigoDot.to.Cyto}
\title{
Opening the AmigoDot graph in Cytoscape through RCytoscape.
}
\description{
Opening the AmigoDot graph in Cytoscape through RCytoscape.
}
\usage{
AmigoDot.to.Cyto(object)
}
\arguments{
  \item{object}{
	is a AmigoDot S4 object.
}
}
\details{
See \url{http://rcytoscape.systemsbiology.net/versions/current/} and
\url{http://www.bioconductor.org/packages/release/bioc/html/RCytoscape.html}
for details on how to install and use RCytoscape.
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
#AmigoDot.to.Cyto(tt)
}
