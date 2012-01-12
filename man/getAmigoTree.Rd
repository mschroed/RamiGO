\name{getAmigoTree}
\alias{getAmigoTree}
\title{
Getting the DAG GO tree for a set of GO ID's.
}
\description{
Getting the DAG GO tree for a set of GO ID's from AmiGO. Saves it as a png, svg or dot file. Returns the webserver response as a string.
}
\usage{
getAmigoTree(goIDs, color, pvalues, pcolors = c("white","tomato"), 
    psplit = c(0.25,0.1,0.05,0.001), filename, picType = "png", 
    modeType = "advanced", webserver, saveResult = TRUE)
}
\arguments{
  \item{goIDs}{
	is a vector of GO ID's.
}
  \item{color}{
	is a vector of colors(). Either of length 1 or length(goIDs).
}
  \item{pvalues}{
	is a vector of numbers. Must have the same length as goIDs
}
  \item{pcolors}{
	is a vector of two or more colors(). It defines the color gradient. The default is
     from white to red (tomato).
}
  \item{psplit}{
	is a vector of numbers (p-values) that defines the split for the color gradient. The
     default color changes are 0.25, 0.1, 0.05, 0.001.
}
  \item{filename}{
	is the output filename. If the file ending is missing or not matching the picType, the file ending is automatically fixed (replaced or created).
}
  \item{picType}{
	supports "png","svg" and "dot". default is "png".
}
  \item{modeType}{
	is the AmiGO query mode, only "advanced" supported.
}
  \item{webserver}{
	is by default the AmiGO webserver. You can specify another one.
}
  \item{sav
eResult}{
	saving the results as a png, svg or dot file. Default is TRUE.
}
}
\details{
The edge colors in the tree represent the relationship between two nodes. In particular: green=positively regulates, red=negatively regulates, black=regulates, blue=is a, light blue=part of. The tree represents the set of input GO ID's and all parents of those GO ID's.
}
\value{
  \item{res}{
	string with the webserver response.
}
}
\author{
Markus Schroeder <mschroed@jimmy.harvard.edu>
}
\examples{
## set GO ID's, color and pvalues
goIDs <- c("GO:0051130","GO:0019912","GO:0005783")
color <- c("lightblue","red","yellow")
pvalues <- c(0.0001,0.16,0.04)

## get results with custom node colors
pp <- getAmigoTree(goIDs=goIDs,color=color,
  filename="example")
ss <- getAmigoTree(goIDs=goIDs,color=color,
  filename="example",picType="svg",saveResult=FALSE)
dd <- getAmigoTree(goIDs=goIDs,color=color,
  filename="example",picType="dot",saveResult=FALSE)

## get results without coloring the nodes
ppNoColor <- getAmigoTree(goIDs=goIDs,
  filename="example-no-color",saveResult=FALSE)

## get the results with a color gradient according to
## the pvalue
ppGradient <- getAmigoTree(goIDs=goIDs, pvalues=pvalues,
  filename="example-Gradient", saveResults=FALSE)

## get the results with a white-to-blue color gradient
## according to the pvalue
ppGradient <- getAmigoTree(goIDs=goIDs, pvalues=pvalues,
  pcolors=c("white","blue")
  filename="example-Gradient", saveResults=FALSE)
}