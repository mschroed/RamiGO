getAmigoTree <- function(picType = "png", modeType = "basic", goIDs = NULL, color = NULL, filename = NULL){
  if(is.null(goIDs)){
    stop("RamiGO: You must specifiy GO ID's!")
  }
  if(!is.null(color) && length(goIDs) != length(color)){
    stop("RamiGO: Length of GO ID's and colors differ!")
  }
  if(picType != "png" && picType != "svg" && picType != "dot"){
    stop("RamiGO: Currently only picType: png, svg and dot are supported! (not svg_raw and navi)")
  }
  if(modeType != "basic"){
    stop("RamiGO: Currently only modeType: basic is supported! (not quickgo, single, multi or advanced)")
  }
  if(is.null(filename)){
    filename <- sprintf("RamiGO.%s",picType)
  }
  
  ## write results out for amigo
  ## %22 = " ## %3A = : ## %2C = , ## %0D = \n
  
  URL.PREFIX <- "http://amigo.geneontology.org/cgi-bin/amigo/visualize?inline=false&term_data={%0D"
  URL.SUFFIX <- paste("%0D%20}&format=",picType,"&mode=",modeType,"&term_data_type=json",sep="")
  
  goSplit <- t(sapply(strsplit(goIDs,":"),function(x)x))
  if(!is.null(color)){
    URL.DATA <- paste('%22',goSplit[,1],'%3A',goSplit[,2],'%22%3A{%22fill%22%3A%20%20%22',color,'%22}%2C%20%0D',sep='',collapse="")
  } else {
    URL.DATA <- paste('%22',goSplit[,1],'%3A',goSplit[,2],'%22%3A{%22fill%22%3A%20%20%22lightblue%22}%2C%20%0D',sep='',collapse="")
  }
  URL.REQ <- paste(URL.PREFIX,substr(URL.DATA,1,nchar(URL.DATA)-9),URL.SUFFIX,sep="")
  download.file(url=URL.REQ,destfile=filename)
}
