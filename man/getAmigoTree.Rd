\name{getAmigoTree}
\alias{getAmigoTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Getting the DAG GO tree for a set of GO ID's.
}
\description{
Getting the DAG GO tree for a set of GO ID's.
}
\usage{
RamiGO(picType = "png", modeType = "basic", goIDs = NULL, color = NULL, filename = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{picType}{
%%     ~~Describe \code{picType} here~~
}
  \item{modeType}{
%%     ~~Describe \code{modeType} here~~
}
  \item{goIDs}{
%%     ~~Describe \code{goIDs} here~~
}
  \item{color}{
%%     ~~Describe \code{color} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
getAmigoTree(goIDs=c("GO:0051130","GO:0019912","GO:0005783"),color=c("lightblue","red","yellow"))
}