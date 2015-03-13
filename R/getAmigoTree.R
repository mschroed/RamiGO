getAmigoTree <- function (goIDs, color, pvalues, pcolors = c("white", "tomato"),
                           psplit = c(1, 0.25, 0.1, 0.05, 0.001), filename, picType = "png",
                           modeType = "amigo", webserver, saveResult = TRUE)
{
  if (missing(webserver)) {
    webserver <- "http://amigo.geneontology.org/visualize"
    # The old webserver for AmiGO v1 is currently still available at:
    # http://amigo1.geneontology.org/cgi-bin/amigo/visualize
  }
  if (missing(goIDs)) {
    stop("getAmigoTree: You must specifiy GO ID's!")
  }
  if (!missing(color) && length(goIDs) != length(color)) {
    if (length(color) == 1) {
      color <- rep(color, length(goIDs))
    }
    else {
      stop("getAmigoTree: Length of GO ID's and colors differ!")
    }
  }
  if (!missing(pvalues)) {
    if (length(pvalues) != length(goIDs) || !is.numeric(pvalues)) {
      stop("getAmigoTree: Not every GO ID has a P-Value or some P-Values are not numeric!")
    }
    else {
      color.palette <- colorRampPalette(colors = pcolors)
      color.gradient <- color.palette(length(psplit))
      color <- rep(NA, length(goIDs))
      for (i in 1:length(color)) {
        for (j in 1:length(psplit)) {
          if (pvalues[i] <= psplit[j]) {
            color[i] <- color.gradient[j]
          }
        }
      }
    }
  }
  if (picType != "png" && picType != "svg" && picType != "dot") {
    stop("getAmigoTree: Currently only picType: png, svg and dot are supported! (not svg_raw and navi)")
  }
  if (modeType != "amigo" & modeType != "advanced") {
    stop("getAmigoTree: Currently only the amigo (v2) and advanced (v1) modes are supported! (not quickgo or single)")
  }
  if (missing(filename)) {
    filename <- sprintf("RamiGO.%s", picType)
  }
  else {
    if (rev(strsplit(filename, "\\.")[[1]])[1] != picType) {
      filename <- sprintf("%s.%s", filename, picType)
    }
  }
  termDataType <- ifelse(missing(color), "string", "json")
  res <- NULL
  if (missing(color)) {
    termData <- paste(goIDs, collapse = ",")
  }
  else {
    termDataTmp <- paste("\"", goIDs, "\":{\"fill\": \"", 
                         color, "\"},", sep = "", collapse = "")
    termData <- paste("{", substr(termDataTmp, 1, nchar(termDataTmp) - 
                                    1), "}", sep = "", collapse = "")
  }
  aa <- postForm(webserver, .params = list(term_data = termData, 
                                           inline = "false", format = picType, mode = modeType, 
                                           term_data_type = termDataType))
  if (picType == "png") {
    res <- readPNG(aa)
    if (saveResult) {
      writePNG(res, filename)
    }
  }
  if (picType == "svg" || picType == "dot") {
    res <- aa
    if (saveResult) {
      writeLines(res, filename)
    }
  }
  return(res)
}
