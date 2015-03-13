## inspire from Gabor Csardi
## source: http://tolstoy.newcastle.edu.au/R/e13/help/11/04/10607.html
exportCytoGML <- function(graph, filename) {	
  ## check if there is a graph
  if(missing(graph)){
    stop("Graph missing.")
  }
  ## check if there is a file name
  if(missing(filename)){
    filename <- "graph.gml"
  }
	fileio <- file(filename, "w")
	## graph attributes
	cat("Creator \"RamiGO exportCytoGML\"\n", file=fileio)
	cat("Version 1.0\n", file=fileio)
	cat("graph\t[\n", file=fileio)
	cat("\tdirected", as.integer(is.directed(graph)), "\n", file=fileio)
	
	## vertex attributes
	if(vcount(graph) > 0) {
		for (i in seq_len(vcount(graph))) {
			cat("\tnode\t[\n", file=fileio)
			cat("\t\tid", i-1, "\n", file=fileio)
			cat("\t\tgraphics\t[\n", file=fileio)
			cat("\t\t\tfill\t\"", V(graph)$color[i], "\"\n", sep="", file=fileio)
			cat("\t\t\ttype\t\"ellipse\"\n", file=fileio)
			cat("\t\t\toutline\t\"#cccccc\"\n", file=fileio)
			cat("\t\t\toutline_width\t1.0\n", file=fileio)
			cat("\t\t]\n", file=fileio)
			cat("\t\tlabel\t", V(graph)$name[i], "\n", file=fileio)
			cat("\t\tdescription\t", V(graph)$description[i], "\n", file=fileio)
			cat("\t]\n", file=fileio)
		}
	}
	
	## edge attributes
	if(ecount(graph) > 0) {
		el <- get.edgelist(graph, names=FALSE)
		eln <- apply(get.edgelist(graph, names=TRUE), 1, paste, collapse="-")
		for (i in seq_len(nrow(el))) {
			cat("\tedge\t[\n", file=fileio)
			cat("\t\tsource", el[i,1], "\n", file=fileio)
			cat("\t\ttarget", el[i,2], "\n", file=fileio)
			cat("\t\tgraphics\t[\n", file=fileio)
			cat("\t\t\twidth\t1.0\n", file=fileio)
			cat("\t\t\tfill\t\"", E(graph)$color[i], "\"\n", sep="", file=fileio)
			cat("\t\t\ttype\t\"line\"\n", file=fileio)
			cat("\t\t\tsource_arrow\t0\n", file=fileio)
			cat("\t\t\ttarget_arrow\t3\n", file=fileio)
			cat("\t\t]\n", file=fileio)
			cat("\t]\n", file=fileio)
		}
	}
	cat("]\n", file=fileio) 
	close(fileio) 
}
