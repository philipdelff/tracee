##' Write data to file(s) using NMwriteData
##'
##' This function writes data objects to files, with support for lists of data objects.
##' It wraps NMwriteData and provides additional functionality for file path construction
##' and metadata stamping.
##'
##' @param x A data frame or list of data frames to write
##' @param file Character string specifying the output file name
##' @param script Optional character string specifying the script name for stamping
##' @param time Optional time stamp to include in output
##' @param model Optional model object or path for organizing output files
##' @param formats Character vector of output formats (passed to NMwriteData)
##' @param fun.path Optional function to construct file paths based on name, model, and subdir
##' @param subdir Optional subdirectory for organizing output files
##' @param ... Additional arguments passed to NMwriteData
##'
##' @return A list of results from NMwriteData calls
##' @export
datwrite <- function(x,file,script,time,model,formats,fun.path,subdir=NULL,...){

  if(!is.listnotplot(x)) x <- list(x)

  res <- lapply(x,
                FUN=datwriteOne,
                file=file,
                formats=formats,
                ## save=save,
                ## show=show,
                ## quiet=quiet,
                script=script,
                time=time,
                model=model,
                ## format.stamp=format.stamp,
                fun.path=fun.path,
                ...
                )

  res
}



##' Write a single data object to file
##'
##' Internal function that handles writing a single data frame to file using NMwriteData.
##' Constructs file paths, prepares stamping arguments, and calls NMwriteData with
##' appropriate parameters.
##'
##' @param x A data frame to write
##' @param file Character string specifying the output file name
##' @param formats Character vector of output formats
##' @param script Optional character string specifying the script name for stamping
##' @param time Optional time stamp to include in output
##' @param model Optional model object or path for organizing output files
##' @param fun.path Optional function to construct file paths based on name, model, and subdir
##' @param subdir Optional subdirectory for organizing output files
##' @param ... Additional arguments passed to NMwriteData
##'
##' @return Result from NMwriteData call
##' @keywords internal
datwriteOne <- function(x,file,formats,script,time,model,fun.path,subdir=NULL,...){
  

  ## if(missing(format.stamp)) format.stamp <- NULL
  if(missing(fun.path)) fun.path <- NULL

  if(missing(script)) script <- NULL
  if(missing(time)) time <- NULL
  if(missing(model)) model <- NULL
  
  ## if(!is.null(fun.path)) file <- fun.path(name=file,model=model)

  args.stamp <- list(script=script,time=time,model=model)
  args.stamp <- args.stamp[!sapply(args.stamp,is.null)]
  
  if(!is.null(args.stamp$model) && is.list(args.stamp$model)){
    args.stamp$model <- args.stamp$model$lst
  }



  args.fun.path <- list(name=file,model=model)
  args.fun.path <- args.fun.path[!sapply(args.fun.path,is.null)]
  ## args.fun.path <- dots[intersect(names(dots),c("name","model","subdir"))]
  #args.fun.path$file <- file

  dots <- list(...)
  dots <- dots[names(dots)%in%names(formals(NMwriteData))]
  
  if(!is.null(fun.path)) file <- fun.path(name=file,model=model,subdir=subdir)
  
  args <- c(list(data=x,
                 ## file = do.call(fun.path,args.fun.path), 
                 file = file,
                 #formats=formats,
                 ##script=script,
                 genText=FALSE,
                 args.stamp=args.stamp
                 ),
            dots)

  res <- do.call(NMwriteData,args)

  res
  
}
