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
  } else {
    if(rev(strsplit(filename,"\\.")[[1]])[1] != picType){
      filename <- sprintf("%s.%s",filename,picType)
    }
  }

  ## write results out for amigo
  ## %22 = " ## %3A = : ## %2C = , ## %0D = \n
  if(is.null(color)){
    URL.PREFIX <- "http://amigo.geneontology.org/cgi-bin/amigo/visualize?inline=false&term_data="
    URL.SUFFIX <- paste("&format=",picType,"&mode=",modeType,"&term_data_type=string",sep="")    
    goSplit <- t(sapply(strsplit(goIDs,":"),function(x)x))
    URL.DATA <- paste(goSplit[,1],'%3A',goSplit[,2],'%2C',sep='',collapse="")
    URL.REQ <- paste(URL.PREFIX,substr(URL.DATA,1,nchar(URL.DATA)-3),URL.SUFFIX,sep="")
    download.file(url=URL.REQ,destfile=filename)
  } else {
    URL.PREFIX <- "http://amigo.geneontology.org/cgi-bin/amigo/visualize?inline=false&term_data={%0D"
    URL.SUFFIX <- paste("%0D%20}&format=",picType,"&mode=",modeType,"&term_data_type=json",sep="")    
    goSplit <- t(sapply(strsplit(goIDs,":"),function(x)x))
    URL.DATA <- paste('%22',goSplit[,1],'%3A',goSplit[,2],'%22%3A{%22fill%22%3A%20%20%22',color,'%22}%2C%20%0D',sep='',collapse="")
    URL.REQ <- paste(URL.PREFIX,substr(URL.DATA,1,nchar(URL.DATA)-9),URL.SUFFIX,sep="")
    download.file(url=URL.REQ,destfile=filename)
  }
}
