##' Write flextable objects to one or multiple file formats
##'
##' Writes \code{flextable} objects to various file formats with optional stamping
##' (caption with script name, timestamp, and model information). Can write single
##' flextables or lists of flextables, with automatic file naming and directory
##' structure organization.
##'
##' @param ft A \code{flextable} object or a named list of \code{flextable} objects.
##'   If a list is provided, each element will be written to a separate file with
##'   names derived from the list element names.
##' @param file Character string specifying the base file name to save to (without
##'   extension). The actual extension(s) will be determined by the \code{formats}
##'   argument. If \code{ft} is a list and elements are named, those names will be
##'   appended to this base file name.
##' @param formats Character vector specifying one or more output formats. Supported
##'   formats are: "png", "docx", "pptx", "html", "rds". Default is \code{NULL}, which
##'   will infer the format from the \code{file} extension.
##' @param save Logical. Should the table be saved to file? Default is \code{TRUE}.
##'   Set to \code{FALSE} to only return/show the flextable without saving.
##'   Hint: if you use a flag variable (like \code{exportFlag}), you can use
##'   \code{save = exportFlag} to control output generation.
##' @param show Logical. Should the flextable be printed/displayed? Default is the
##'   opposite of \code{save} (i.e., \code{!save}). Combining \code{save = TRUE}
##'   and \code{show = TRUE} in knitr documents can be useful to display the table
##'   in the rendered document while also saving high-quality files.
##' @param quiet Logical. If \code{TRUE}, suppresses messages about files being written.
##'   Default is \code{FALSE}.
##' @param script Character string or path to the script file. If provided, will be
##'   included in the table stamp/caption. This helps document which script generated
##'   the output.
##' @param time Character string or POSIXct timestamp. The default behavior is to
##'   include a timestamp if \code{script} is provided. You can pass any string as
##'   \code{time} for a custom format, or use \code{time = ""} to omit the timestamp
##'   entirely.
##' @param model Character string or model object specifying the model name/identifier.
##'   Used for stamping and for organizing output directories via \code{path.format}.
##' @param stamp.ft.style A list of formatting options for the stamp,
##'     passed to \code{\link{ftstamp}}. Valid elements: \code{size},
##'     \code{align}, \code{space}, \code{bg}, \code{color}.
##' @param path.format Character string or function defining the output path.
##'   A template using \code{\%model}, \code{\%script}, and \code{\%name}
##'   placeholders, or a function as returned by \code{\link{pathFormat}}. Passed to
##'   \code{\link{pathFormat}}. Default (\code{NULL}) uses \code{file} as is.
##' @param fun.path Deprecated. Use \code{path.format}.
##' @param subdir Character string specifying a subdirectory within the path structure.
##'   Used in conjunction with \code{path.format}.
##' @param stamp.format A template for the stamp text using the
##'     placeholders \code{\%script}, \code{\%time}, \code{\%model}, and
##'     \code{\%file}, with \code{"\\n"} for line breaks. Default is
##'     \code{"\%file\\n\%script\\n\%model\\n\%time"}. See \code{\link{createStamp}}.
##' @param stamp.funs Optional named list of functions (elements \code{script},
##'     \code{time}, \code{model}, \code{file}) used to preprocess the values
##'     before they are inserted into \code{stamp.format}. See
##'     \code{\link{createStamp}}.
##' @param use.names Logical. If \code{length(ft) > 1}, should \code{names(ft)}
##'   be used in the output file names? Default is \code{TRUE}. If \code{FALSE},
##'   numeric indices (1, 2, 3, ...) are used instead. Only used if \code{save = TRUE}
##'   and \code{ft} is a list.
##' @param ... Additional arguments passed to \code{\link{ftstamp}} for stamp customization.
##'
##' @details
##' The function handles both single flextable objects and lists of flextables:
##' \itemize{
##'   \item For single flextables: writes to file(s) with name specified by \code{file}
##'   \item For lists of flextables: writes each element to a separate file, with names
##'         constructed from \code{file} and the list element names
##' }
##'
##' When \code{ft} is a list:
##' \itemize{
##'   \item Named list elements use their names in the output file names
##'   \item Unnamed elements are automatically numbered with zero-padded indices
##' }
##'
##' Multiple formats can be written simultaneously by specifying a vector of formats.
##' Each format will be saved as a separate file with the appropriate extension.
##'
##' @section File Formats:
##' The following formats are supported via the \code{flextable} package save functions:
##' \itemize{
##'   \item \code{"png"}: Image format via \code{\link[flextable]{save_as_image}}
##'   \item \code{"docx"}: Microsoft Word via \code{\link[flextable]{save_as_docx}}
##'   \item \code{"pptx"}: Microsoft PowerPoint via \code{\link[flextable]{save_as_pptx}}
##'   \item \code{"html"}: HTML format via \code{\link[flextable]{save_as_html}}
##'   \item \code{"rds"}: R data format (saves the flextable object itself)
##' }
##'
##' @return Invisibly returns the result from \code{\link{ftwriteOne}} (for lists,
##'   returns results for all written tables). If \code{show = TRUE}, also prints
##'   the flextable(s) to the current device.
##'
##' @seealso
##' \code{\link{ftwriteOne}} for writing a single flextable,
##' \code{\link{ftstamp}} for adding stamps to flextables,
##' \code{\link{writer}} for a unified interface to write various object types,
##' \code{\link{pathFormat}} for organizing output directories,
##' \code{\link[flextable]{save_as_image}},
##' \code{\link[flextable]{save_as_docx}},
##' \code{\link[flextable]{save_as_pptx}},
##' \code{\link[flextable]{save_as_html}}
##'
##' @import flextable
##' @importFrom NMdata fnExtension
##' @export
##'
##' @examples
##' \dontrun{
##' library(flextable)
##'
##' # Create a simple flextable
##' ft1 <- flextable(head(mtcars))
##'
##' # Write to PNG (default format)
##' ftwrite(ft1, file = "mytable")
##'
##' # Write to multiple formats
##' ftwrite(ft1, file = "mytable", formats = c("png", "docx", "html"))
##'
##' # Write with stamping
##' ftwrite(ft1, file = "mytable",
##'         script = "analysis.R",
##'         model = "run001")
##'
##' # Write a list of tables
##' ft_list <- list(
##'   summary = flextable(summary(mtcars)),
##'   head = flextable(head(mtcars)),
##'   tail = flextable(tail(mtcars))
##' )
##' ftwrite(ft_list, file = "tables", formats = "docx")
##' # Creates: tables_summary.docx, tables_head.docx, tables_tail.docx
##'
##' # Use with path structure for model organization
##' ftwrite(ft1, file = "summary",
##'         model = "run001",
##'         path.format = "%model/%name_%model")
##' # Writes to: run001/summary_run001.png
##' }
ftwrite <- function(ft,file,formats,save=TRUE,show=!save,quiet=FALSE,script,time,model,stamp.ft.style,path.format=NULL,subdir=NULL,use.names=TRUE,stamp.format=NULL,stamp.funs=NULL,fun.path=NULL){
  

  
  if(is.listnotplot(ft)) {
    was.list <- TRUE
  } else {
    was.list <- FALSE
    ## ft was not a list, so cannot be named
    ft <- list(ft)
    ## names(ft) <- file
    names.all <- file
  }

  if(missing(script)) script <- NULL
  if(missing(time)) time <- NULL
  if(missing(model)) model <- NULL
  fun.path <- argPathFormat(path.format=path.format, fun.path=fun.path)
  if(missing(formats)) formats <- NULL

  if(was.list){ 
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

    names.all <- fnAppend(file,names.els,collapse=NULL)
  }

  if(missing(stamp.ft.style)) stamp.ft.style <- NULL

  

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
                  stamp.ft.style=stamp.ft.style,
                  fun.path=fun.path,
                  subdir=subdir,
                  stamp.format=stamp.format,
                  stamp.funs=stamp.funs
                )
                )

  invisible(res)

}

##' Write a single flextable object to file(s)
##'
##' Internal function called by \code{\link{ftwrite}} to handle writing of individual
##' flextable objects. Generally not called directly by users.
##'
##' @param ft A \code{flextable} object to write.
##' @param file Character string, base file name (without extension).
##' @param formats Character vector of output format(s). See \code{\link{ftwrite}}.
##' @param save Logical, whether to save to file. Default \code{TRUE}.
##' @param show Logical, whether to display the table. Default \code{!save}.
##' @param quiet Logical, suppress messages. Default \code{FALSE}.
##' @param script Character string, path to script for stamping.
##' @param time Character string or POSIXct, timestamp for stamping.
##' @param model Character string or model object, model identifier for stamping.
##' @param stamp.ft.style Format specification for stamp. Passed to \code{\link{ftstamp}}.
##' @param fun.path Function for constructing file paths. See \code{\link{pathFormat}}.
##' @param subdir Character string, subdirectory for outputs.
##' @param stamp.format,stamp.funs Passed to \code{\link{ftstamp}}.
##'
##' @return If \code{show = TRUE}, returns the flextable object (possibly stamped).
##'   Otherwise returns invisibly a list of file paths written.
##'
##' @seealso \code{\link{ftwrite}}
##' @keywords internal
ftwriteOne <- function(ft,file,formats,save=TRUE,show=!save,quiet=FALSE,script,time,model,stamp.ft.style,fun.path,subdir,stamp.format=NULL,stamp.funs=NULL){
  
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
  if(missing(stamp.ft.style)) stamp.ft.style <- NULL
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


  if(!is.null(fun.path)) file <- applyFunPath(fun.path,name=file,model=model,script=script,subdir=subdir,quiet=quiet)
  dir.out <- dirname(file)
  OutputDirCreate(dir.out)
  
  ## write all requested formats  
  silent <- lapply(formats,function(ext){
    fn <- fnExtension(file,ext)
    fun.write <- switch(sub("\\.","",ext),
                        png=save_as_image
                       ,html=save_as_html
                       ,docx=save_as_docx
                       ,pptx=save_as_pptx,
                        stop("format not supported. See ?ftwrite"))
    
    ft <- ftstamp(ft=ft,file=fn,script=script,time=time,model=model,stamp.ft.style=stamp.ft.style,
                  stamp.format=stamp.format,stamp.funs=stamp.funs)
    
    fun.write(ft,path=fn)
    if(!quiet&&!is.null(fn)) message(fn)
    fn
  })
  
  
  if(show){
    return(ft)
  }
  invisible(silent)
  

}
