setOldClass("igraph")

setClass("AmigoDot",
  representation=representation(
  agraph="igraph",
  adjMatrix="matrix",
  annot="data.frame",
  relations="data.frame",
  leaves="data.frame"))

setValidity("AmigoDot", function(object) {
  msg <- NULL
  if(class(agraph(object)) != "igraph")
    msg <- c(msg, "graph should contain a valid igraph object!")
  if(any(dim(adjMatrix(object)) <= 0))
    msg <- c(msg, "adjMatrix should contain at least one node!")
  if(!is.na(table(c("node","GO_ID","description","color","fillcolor","fontcolor") == colnames(annot(object)))[2]))
    msg <- c(msg, "annot has not the required column names or column content!")
  if(!is.na(table(c("parent","child","arrowhead","arrowtail","color","style") == colnames(relations(object)))[2]))
    msg <- c(msg, "relations has not the required column names or column content!")
  if(!is.na(table(c("node","GO_ID","description","color","fillcolor","fontcolor") == colnames(leaves(object)))[2]))
    msg <- c(msg, "leaves has not the required column names or column content!")
  if (is.null(msg)) TRUE else msg
})

setGeneric("agraph", function(object) standardGeneric("agraph"))
setGeneric("agraph<-", function(object, value) standardGeneric("agraph<-"))
setMethod(agraph, "AmigoDot", function(object) slot(object, "agraph"))
setReplaceMethod("agraph", "AmigoDot", function(object, value)
{
      slot(object, "agraph") <- value
      validObject(object)
      object
})

setGeneric("adjMatrix", function(object) standardGeneric("adjMatrix"))
setGeneric("adjMatrix<-", function(object, value) standardGeneric("adjMatrix<-"))
setMethod(adjMatrix, "AmigoDot", function(object) slot(object, "adjMatrix"))
setReplaceMethod("adjMatrix", "AmigoDot", function(object, value)
{
      slot(object, "adjMatrix") <- value
      validObject(object)
      object
})

setGeneric("annot", function(object) standardGeneric("annot"))
setGeneric("annot<-", function(object, value) standardGeneric("annot<-"))
setMethod(annot, "AmigoDot", function(object) slot(object, "annot"))
setReplaceMethod("annot", "AmigoDot", function(object, value)
{
      slot(object, "annot") <- value
      validObject(object)
      object
})

setGeneric("relations", function(object) standardGeneric("relations"))
setGeneric("relations<-", function(object, value) standardGeneric("relations<-"))
setMethod(relations, "AmigoDot", function(object) slot(object, "relations"))
setReplaceMethod("relations", "AmigoDot", function(object, value)
{
      slot(object, "relations") <- value
      validObject(object)
      object
})

setGeneric("leaves", function(object) standardGeneric("leaves"))
setGeneric("leaves<-", function(object, value) standardGeneric("leaves<-"))
setMethod(leaves, "AmigoDot", function(object) slot(object, "leaves"))
setReplaceMethod("leaves", "AmigoDot", function(object, value)
{
      slot(object, "leaves") <- value
      validObject(object)
      object
})

setMethod(show, "AmigoDot", function(object) {
    cat("class:", class(object), "\n")
    cat("nodes:", str(E(agraph(object))), "\n")
    cat("edges:", str(V(agraph(object))), "\n")
    cat("leaves:", str(leaves(object)), "\n")
    cat("annot:", str(annot(object)), "\n")
    cat("relations:", str(relations(object)), "\n")
})

#  AmigoDot <- function(<...>) {
#     <...>
#     new("AmigoDot", graph=graph, adjMatrix=adjMatrix, annot=annot,
#         relations=relations, leaves=leaves, ...)
#  }
