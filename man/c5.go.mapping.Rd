\name{c5.go.mapping}
\alias{c5.go.mapping}
\docType{data}
\title{MSigDB C5 GO term to GO ID mapping}
\description{
This object provides a mapping for MSigDB GO terms for the bp, mf and cc GO categories to official GO ID's.
}
\format{
  A data frame with 1454 observations on the following 2 variables.
  \describe{
    \item{\code{description}}{a character vector of MSigDB GO terms}
    \item{\code{goid}}{a character vector of official GO ID's}
  }
}
\source{
\bold{c5.go.mapping:}

\url{http://www.broadinstitute.org/gsea/msigdb/collections.jsp#C5}
}
\references{
Subramanian A, Tamayo P, Mootha VK, Mukherjee S, Ebert BL, Gillette MA, Paulovich A, Pomeroy SL, Golub TR, Lander ES, Mesirov JP. Gene set enrichment analysis: A knowledge-based approach for interpreting genome-wide expression profiles. \emph{Proceedings of the National Academy of Sciences of the United States of America} 2005 Oct;\bold{102}(43):15545 -15550.}
\examples{
## load the mapping
data(c5.go.mapping)

## look at the object
str(c5.go.mapping)

## create go term vector
terms <- c("CHROMATIN_REMODELING_COMPLEX",
	"RNA_POLYMERASE_COMPLEX",
	"CYTOKINESIS","CELL_RECOGNITION")
id <- sapply(terms,function(x)c5.go.mapping[
  match(x,c5.go.mapping[,1]),2])
id
}
\keyword{datasets}