getAmigoTree <- function(goIDs = NULL, color = NULL, filename = NULL, picType = "png", modeType = "advanced", webserver=NULL, saveResult=TRUE){
  if(is.null(webserver)){
    webserver <- "http://amigo.geneontology.org/cgi-bin/amigo/visualize"
  }
  if(is.null(goIDs)){
    stop("RamiGO: You must specifiy GO ID's!")
  }
  if(!is.null(color) && length(goIDs) != length(color)){
    if(length(color) == 1){
      color <- rep(color,length(goIDs))
    } else {
      stop("RamiGO: Length of GO ID's and colors differ!")
    }
  }
  if(picType != "png" && picType != "svg" && picType != "dot"){
    stop("RamiGO: Currently only picType: png, svg and dot are supported! (not svg_raw and navi)")
  }
  if(modeType != "advanced"){
    stop("RamiGO: Currently only the advanced mode is supported! (not quickgo or single)")
  }
  if(is.null(filename)){
    filename <- sprintf("RamiGO.%s",picType)
  } else {
    if(rev(strsplit(filename,"\\.")[[1]])[1] != picType){
      filename <- sprintf("%s.%s",filename,picType)
    }
  }
  termDataType <- ifelse(is.null(color),"string","json")
  res <- NULL

  if(is.null(color)){
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
