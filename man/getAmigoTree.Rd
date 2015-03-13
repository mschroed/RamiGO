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
  psplit = c(1,0.25,0.1,0.05,0.001), filename, picType = "png",
  modeType = "amigo", webserver, saveResult = TRUE)
}
\arguments{
  \item{goIDs}{
	is a vector of GO ID's.
  }
  \item{color}{
	is a vector of colors(). Either of length 1 or length(goIDs).
  }
  \item{pvalues}{
	is a vector of pvalues.
  }
  \item{pcolors}{
	colors for gradient.
  }
  \item{psplit}{
	is a vector of pvalues that sets different color gradients ranges.
  }
  \item{filename}{
	is the output filename. If the file ending is missing or not matching the picType, the file ending is automatically fixed (replaced or created).
  }
  \item{picType}{
	supports "png","svg" and "dot". default is "png".
  }
  \item{modeType}{
	is the AmiGO query mode, only "amigo" is supported for AmiGO v2. If you're using AmiGO v1 use modeType="advanced".
  }
  \item{webserver}{
	is by default the AmiGO v2 webserver. You can specify another one.
  }
  \item{saveResult}{
	saving the result as a png, svg or dot file. Default is TRUE.
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
## set GO ID's and color
goIDs <- c("GO:0051130","GO:0019912","GO:0005783")
color <- c("lightblue","red","yellow")

## get results
pp <- getAmigoTree(goIDs=goIDs,color=color,filename="example")
ss <- getAmigoTree(goIDs=goIDs,color=color,filename="example",picType="svg",saveResult=FALSE)
dd <- getAmigoTree(goIDs=goIDs,color=color,filename="example",picType="dot",saveResult=FALSE)

#ppNoColor <- getAmigoTree(goIDs=goIDs,filename="example-no-color",saveResult=FALSE)

#goIDs <- c("GO:0051130","GO:0050789","GO:0019912","GO:0016301","GO:0003824","GO:0005783","GO:0043229")
#pvalues <- c(0.001,0.5,0.0001,0.16,0.47,0.00006,0.002)
#ptest <- getAmigoTree(goIDs=goIDs, pvalues=pvalues, filename="pvaluetest")
}
