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
##'   Used for stamping and for organizing output directories via \code{fun.path}.
##' @param format.stamp Character string or format specification for the stamp.
##'   Passed to \code{\link{ftstamp}} to control stamp formatting.
##' @param fun.path Function or character string defining the directory structure for
##'   output files. If a character string, must be one of the predefined structures.
##'   If a function, should accept \code{name}, \code{model}, and \code{subdir} arguments
##'   and return a file path. See \code{\link{pathStruct}} for details.
##' @param subdir Character string specifying a subdirectory within the path structure.
##'   Used in conjunction with \code{fun.path}.
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
##' \code{\link{pathStruct}} for organizing output directories,
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
##' path.fun <- pathStruct("model/file_model")
##' ftwrite(ft1, file = "summary",
##'         model = "run001",
##'         fun.path = path.fun)
##' # Writes to: run001/summary_run001.png
##' }
ftwrite <- function(ft,file,formats,save=TRUE,show=!save,quiet=FALSE,script,time,model,format.stamp,fun.path,subdir=NULL){
  
  
  use.names <- TRUE
  if(!is.listnotplot(ft)) {
    ## ft was not a list, so cannot be named
    ft <- list(ft)
    names(ft) <- file
  }

  if(missing(script)) script <- NULL
  if(missing(time)) time <- NULL
  if(missing(model)) model <- NULL
  if(missing(fun.path)) fun.path <- NULL
  if(missing(formats)) formats <- NULL

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
##' @param format.stamp Format specification for stamp. Passed to \code{\link{ftstamp}}.
##' @param fun.path Function for constructing file paths. See \code{\link{pathStruct}}.
##' @param subdir Character string, subdirectory for outputs.
##'
##' @return If \code{show = TRUE}, returns the flextable object (possibly stamped).
##'   Otherwise returns invisibly a list of file paths written.
##'
##' @seealso \code{\link{ftwrite}}
##' @keywords internal
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
