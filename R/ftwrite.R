##' Stamp and write flextab objects to one or multiple formats
##' @param ft A flextab object.
##' @param file to save to. See formats to generate multiple files.
##' @param script path to script - will be pasted as caption. If
##'     provided, a stamp will be included on the plot when writng
##'     file(s).
##' @param time The default behavior is to include a time stamp if
##'     `script` is provided. You can pass any string as `time` if you
##'     prefer a different format or a completely different string
##'     here instead (like model name?). Use `time=""` to omit.
##' @param formats One or more of png, docx, pptx, html. As a
##'     character vector.
##' @param save Save the table to the given file or just show?
##'     Defaults to TRUE. Hint, if you use an "exportFlag", use
##'     save=exportFlag.
##' @param show Print the plot to the screen? Defaults to the opposite
##'     of save. Combining save and show in knitr can give you
##'     both a high quality plot in your pdf and a png optimized for
##'     powerpoint.
##' @param quiet Default is false but use TRUE to suppress messages
##'     about what was saved.
##' @param ... Arguments passed to stampFlextab.
##' @import flextable
##' @importFrom NMdata fnExtension
##' @export

ftwrite <- function(ft,file,formats,save=TRUE,show=!save,quiet=FALSE,script,time,model,format.stamp,fun.path,subdir=NULL){
  
  
  use.names <- TRUE
  if(!is.listnotplot(ft)) {
    ## ft was not a list, so cannot be named
    ft <- list(ft)
    names(ft) <- file
  }

  if(missing(script)) script <- NULL
  if(missing(time)) time <- NULL

  if(use.names){
    names.els <- names(ft)
    if(length(names.els)==0) names.els <- rep("",length(ft))
    
    nulls <- which(sapply(names.els,function(x)x==""))

    if(length(nulls)){
      nchars <- round(log10(length(ft)))+1
      names.els[nulls] <- padZeros(nulls,nchars=nchars)
    }
  } else {
    ## not sure this is necessary. Handled already?
    nchars <- round(log10(length(ft)))+1
    names.els <- padZeros(1:length(ft),nchars=nchars)
  }

  names.all <- names.els

  if(F){
    subdir <- NULL
    if(lists.as.subdirs){
      if(use.names) {
        names.all <- names.els
      } else {
        ## names.all <- paste(names.els,sep="_")
        ## names.all <- padZeros(paste(1:length(names.els)))
        names.all <- names.els
      }
      subdir <- file
    } else {
      if(use.names) {
        ### if no names provided, they will all get the same name(?) 
        names.all <- rep(file,length(ft))
      } else {
        names.all <- paste(file,names.els,sep="_")
      }
    }
  }

  if(missing(format.stamp)) format.stamp <- NULL



  res <- mapply(FUN=ftwriteOne,
                ft,
                file=names.all,
                MoreArgs=list(
                  formats=formats,
                  save=save,
                  show=show,
                  quiet=quiet,
                  script=script,
                  time=time,
                  model=model,
                  format.stamp=format.stamp,
                  fun.path=fun.path,
                  subdir=subdir
                )
                )

  invisible(res)

}

##' write a single flextable object

ftwriteOne <- function(ft,file,formats,save=TRUE,show=!save,quiet=FALSE,script,time,model,format.stamp,fun.path,subdir){
  
  ## save_as_docx
  ## save_as_html
  ## save_as_image
  ## save_as_pptx
  ## rds

  ##all.files <- fnExtension(file,formats)

  if(missing(save)||is.null(save)) save <- TRUE
  if(!save) {
    if(show){
      return(ft)
    } else {
      return(invisible(ft))
    }
  }
  if(missing(format.stamp)) format.stamp <- NULL
  if(missing(fun.path)) fun.path <- NULL

  if(missing(formats)||is.null(formats)) {
    ## formats <- sub(".*\\.(.+)$","\\1",file)
    formats <- fnExtension(file)
  }

  if(missing(script)) script <- NULL
  if(missing(time)) time <- NULL
  if(missing(model)) model <- NULL

  ## if(lists.as.subdirs){
  ##   if(!is.null(fun.path)) file <- fun.path(name=,model=model,subdir=name)
  ## } else {
  ##   if(!is.null(fun.path)) file <- fun.path(name=file,model=model)
  ## }


  if(!is.null(fun.path)) file <- fun.path(name=file,model=model,subdir=subdir)



  ## write all requested formats  
  silent <- lapply(formats,function(ext){
    fn <- fnExtension(file,ext)
    fun.write <- switch(sub("\\.","",ext),
                        png=save_as_image
                       ,html=save_as_html
                       ,docx=save_as_docx
                       ,pptx=save_as_pptx,
                        stop("format not supported. See ?ftwrite"))
    
      ft <- ftstamp(ft=ft,file=fn,script=script,time=time,model=model,format.stamp=format.stamp)
    
    fun.write(ft,path=fn)
    if(!quiet&&!is.null(fn)) message("Written to ",fn)
    fn
  })
  
  
  if(show){
    return(ft)
  }
  invisible(silent)
  

}
