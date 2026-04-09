##' One interface for ggwrite and writeFlextab
##' @param x
##' @param file
##' @param save
##' @param show
##' @param formats.ft
##' @param formats.gg
##' @param formats.data passed to NMwriteData() as format.write if file is a data set.
##' @param script
##' @param canvas
##' @param fun.path
##' @param ...
##' @importFrom NMdata NMwriteData
##' @importFrom utils modifyList


## add ,args.ggwrite and and args.flextable
writer <- function(x,file,formats.ft,formats.gg,formats.data,##script=NULL,time,model=NULL,
                   fun.path,
                   ...){
  
  
  if(missing(formats.ft)||is.null(formats.ft)){
    formats.ft <- "png"
  }

  if(missing(formats.gg)||is.null(formats.gg)){
    formats.gg <- "png"
  }

  if(missing(formats.data)||is.null(formats.data)){
    formats.data <- "rds"
  }

  dots <- list(...)
  
  

  class_x <- "unknown"
  if("flextable" %in% class(x)) {
    class_x <- "flextable"
  }
  if( class_x == "unknown"&&is.data.frame(x)){
    class_x <- "data.frame"
  }
  if( class_x == "unknown"&&is.list(x) && !is.gg(x)){
    class_x <- "list"
  }
  if( class_x == "unknown"){
    stop("x is an unknown format",paste(class(x),collapse=", "))
  }
  
  if(class_x=="flextable") {
    
    dots <- dots[names(dots)%in%c(names(formals(ftwrite)))]
    args <- c(list(ft=x,
                   file = file,
                   formats=formats.ft,
                   fun.path=fun.path
                   ),
              dots)
    res <- do.call(ftwrite,args)

  }

  if(class_x=="data.frame") {
    dots <- dots[unique(names(dots)%in%c(

      names(formals(NMwriteData)),
      names(formals(datwrite))
      ))]

    args <- c(list(x=x,
                   file = file,
                   formats=formats.data,
                   fun.path=fun.path
                   ),
              dots)
    res <- do.call(datwrite,args)
    
if(F){
  args.stamp <- dots[names(dots)%in%c("model")]
    if(!is.null(args.stamp$model) && is.list(args.stamp$model)) args.stamp$model <- args.stamp$model$lst
    
    args.fun.path <- dots[intersect(names(dots),c("name","model","subdir"))]
    #args.fun.path$file <- file

    dots <- dots[names(dots)%in%names(formals(NMwriteData))]
    
    args <- c(list(data=x,
                   file = do.call(fun.path,args.fun.path), 
                   formats=formats.data,
                   ##script=script,
                   genText=FALSE,
                   args.stamp=args.stamp
                   ),
              dots)
    if("time"%in%names(args)){
      args$args.stamp$time <- args$time
      args$time <- NULL
    }
    res <- do.call(NMwriteData,args)
    ##res <- do.call(datwrite,args )
}
    
  }

  ###if( class_x == "unknown"&&is.list(x) && !is.gg(x)){
  if(class_x=="list"){
    ## if(is.list(x) && !is.ggplot(x) && ! "gtable"%in%class(x) ){
    ## if(is.list(x) && !is.gg(x) ){
    ##   x <- x[!sapply(x,is.null)]
    ## }
    
    ## message("Calling ggwrite()")
    args.gg.def <- list(
      onefile=TRUE,
      canvas="standard"
    )
    
    dots <- dots[names(dots)%in%names(formals(ggwrite))]    
    args.gg <- modifyList(
      args.gg.def,
      dots)
    
    args.ggwrite <- modifyList(
      list(plot=x,
           file=file,
           use.names=TRUE,
           formats=formats.gg,
           fun.path=fun.path),
      args.gg
    )
    
    res <- try(do.call(ggwrite,args.ggwrite))
  }
  
  invisible(res)
}
