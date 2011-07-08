getAmigoTree <- function(goIDs, color, filename, picType = "png", modeType = "advanced", webserver, saveResult=TRUE){
  if(missing(webserver)){
    webserver <- "http://amigo.geneontology.org/cgi-bin/amigo/visualize"
  }
  if(missing(goIDs)){
    stop("getAmigoTree: You must specifiy GO ID's!")
  }
  if(!missing(color) && length(goIDs) != length(color)){
    if(length(color) == 1){
      color <- rep(color,length(goIDs))
    } else {
      stop("getAmigoTree: Length of GO ID's and colors differ!")
    }
  }
  if(picType != "png" && picType != "svg" && picType != "dot"){
    stop("getAmigoTree: Currently only picType: png, svg and dot are supported! (not svg_raw and navi)")
  }
  if(modeType != "advanced"){
    stop("getAmigoTree: Currently only the advanced mode is supported! (not quickgo or single)")
  }
  if(missing(filename)){
    filename <- sprintf("RamiGO.%s",picType)
  } else {
    if(rev(strsplit(filename,"\\.")[[1]])[1] != picType){
      filename <- sprintf("%s.%s",filename,picType)
    }
  }
  termDataType <- ifelse(missing(color),"string","json")
  res <- NULL

  if(missing(color)){
    termData <- paste(goIDs,sep=",",collapse="")
  } else {
    termDataTmp <- paste('"',goIDs,'":{"fill": "',color,'"},',sep="",collapse="")
    termData <- paste('{',substr(termDataTmp,1,nchar(termDataTmp)-1),'}',sep="",collapse="")
  }

  aa <- postForm(webserver,
    .params=list(
      term_data=termData,
      inline="false",
      format=picType,
      mode=modeType,
      term_data_type=termDataType))     

  if(picType == "png"){
    res <- readPNG(aa)
    if(saveResult){
      writePNG(res,filename)
    }
  }
  
  if(picType == "svg" || picType == "dot"){
    res <- aa
    if(saveResult){
      writeLines(res,filename)
    }
  }
  return(res)
}
