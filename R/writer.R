##' Write various R objects to files with flexible formatting
##'
##' A unified interface for \code{\link{ggwrite}}, \code{\link{ftwrite}}, and
##' \code{\link[NMdata]{NMwriteData}} that automatically detects object type and
##' applies the appropriate writing function with consistent file naming and
##' stamping conventions.
##'
##' @param x Object to write. Can be a \code{ggplot}, \code{flextable},
##'   \code{data.frame}, or list of such objects. Lists can be arbitrarily nested
##'   and will be processed according to object type.
##' @param file Character string specifying the base file name (without extension).
##'   The actual extension will be determined by the \code{formats.*} arguments.
##' @param formats.ft Character vector of file formats for flextable objects.
##'   Default is \code{"png"}. Supported formats include: "png", "docx", "pptx",
##'   "html". See \code{\link{ftwrite}} for details.
##' @param formats.gg Character vector of file formats for ggplot objects.
##'   Default is \code{"png"}. Supported formats: "png", "pdf".
##'   See \code{\link{ggwrite}} for details.
##' @param formats.data Character vector of file formats for data frame objects.
##'   Default is \code{"rds"}. This is passed to \code{\link[NMdata]{NMwriteData}}
##'   as the \code{format.write} argument. Common formats include: "rds", "csv",
##'   "fst".
##' @param path.format Character string or function defining the output path.
##'   A template using \code{\%model}, \code{\%script}, and \code{\%name}
##'   placeholders, or a function as returned by \code{\link{pathFormat}}. Passed to
##'   \code{\link{pathFormat}}. Default (\code{NULL}) uses \code{file} as is.
##' @param fun.path Deprecated. Use \code{path.format}.
##' @param stamp.format A template for the stamp text using the
##'     placeholders \code{\%script}, \code{\%time}, \code{\%model}, and
##'     \code{\%file}, with \code{"\\n"} for line breaks. Default is
##'     \code{"\%file\\n\%script\\n\%model\\n\%time"}. See \code{\link{createStamp}}.
##' @param stamp.funs Optional named list of functions (elements \code{script},
##'     \code{time}, \code{model}, \code{file}) used to preprocess the values
##'     before they are inserted into \code{stamp.format}. See
##'     \code{\link{createStamp}}.
##' @param stamp.gg.style A list of formatting options for the stamp text,
##'     passed to \code{\link{ggstamp}}. For ggplot objects, elements accepted
##'     by \code{ggplot2::element_text()} (e.g. \code{size}, \code{colour},
##'     \code{hjust}).
##' @param stamp.ft.style A list of formatting options for the stamp,
##'     passed to \code{\link{ftstamp}}. Valid elements: \code{size},
##'     \code{align}, \code{space}, \code{bg}, \code{color}.
##' @param subdir Character string specifying a subdirectory within the path structure.
##'   This is passed to the path function generated from \code{path.format}.
##' @param ... Additional arguments passed to the specific writer functions
##'   (\code{\link{ggwrite}}, \code{\link{ftwrite}}, or \code{\link[NMdata]{NMwriteData}}).
##'   Common arguments include:
##'   \itemize{
##'     \item \code{script}: Path to the script file (for stamping)
##'     \item \code{time}: Time stamp (for stamping)
##'     \item \code{model}: Model identifier (for stamping and path construction)
##'     \item \code{save}: Logical, whether to save the output (default \code{TRUE})
##'     \item \code{show}: Logical, whether to show/print the output (default \code{!save})
##'     \item \code{canvas}: Canvas size for plots (see \code{\link{canvasSize}})
##'     \item \code{quiet}: Logical, suppress messages (default \code{FALSE})
##'   }
##'
##' @details
##' The function dispatches to appropriate writer functions based on object class:
##' \itemize{
##'   \item \code{flextable} objects → \code{\link{ftwrite}}
##'   \item \code{data.frame} objects → \code{\link{datwrite}} (which wraps
##'         \code{\link[NMdata]{NMwriteData}})
##'   \item \code{ggplot}/\code{gtable} objects or lists → \code{\link{ggwrite}}
##' }
##'
##' For lists of objects, each element is written separately with appropriate naming.
##' The function intelligently handles nested lists by flattening them and constructing
##' meaningful file names from the list structure.
##'
##' File paths are constructed using \code{path.format}, which can organize
##' outputs by model, file name, or custom patterns. This is particularly useful in
##' modeling workflows where outputs need to be organized by model number or name.
##'
##' @section Object Type Detection:
##' The function determines object type in the following order:
##' \enumerate{
##'   \item Check if \code{flextable}
##'   \item Check if \code{data.frame}
##'   \item Check if \code{ggplot} or \code{gtable} (using \code{\link{is.gg}})
##'   \item Check if \code{list} (and not a plot object)
##'   \item If none of the above, throw an error
##' }
##'
##' @section Trace Attributes:
##' If the object \code{x} has traceit attributes (added via \code{\link{traceit}}),
##' those attributes will be automatically extracted and used by the writer function,
##' unless explicitly overridden by arguments passed to \code{writer}.
##'
##' @return Invisibly returns the result from the called writer function.
##'   The specific return value depends on which writer function was dispatched to.
##'
##' @seealso
##' \code{\link{ggwrite}} for writing ggplot objects,
##' \code{\link{ftwrite}} for writing flextable objects,
##' \code{\link{datwrite}} for writing data frames,
##' \code{\link{lwrite}} for writing lists with subdirectory organization,
##' \code{\link{pathFormat}} for path format options,
##' \code{\link{traceit}} for adding traceit attributes to objects
##'
##' @importFrom NMdata NMwriteData
##' @importFrom utils modifyList
##' @export
##'
##' @examples
##' \dontrun{
##' library(ggplot2)
##' library(flextable)
##'
##' # Write a ggplot
##' p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
##' writer(p1, file = "myplot", formats.gg = c("png", "pdf"))
##'
##' # Write a flextable
##' ft1 <- flextable(head(mtcars))
##' writer(ft1, file = "mytable", formats.ft = c("png", "docx"))
##'
##' # Write a data frame
##' writer(mtcars, file = "mydata", formats.data = "csv")
##'
##' # Use with model organization
##' writer(p1, file = "gof", model = "run001", path.format = "%model/%name_%model")
##' # Writes to: run001/gof_run001.png
##'
##' # Write with stamping
##' writer(p1, file = "myplot",
##'        script = "analysis.R",
##'        time = Sys.time(),
##'        model = "run001")
##' }
writer <- function(x,file,formats.ft,formats.gg,formats.data,##script=NULL,time,model=NULL,
                   path.format=NULL,subdir=NULL,stamp.format=NULL,stamp.funs=NULL,stamp.gg.style=NULL,stamp.ft.style=NULL,fun.path=NULL,
                   ...){

  ## resolve once here, then pass on to ftwrite()/datwrite()/ggwrite() as path.format
  path.format <- argPathFormat(path.format=path.format, fun.path=fun.path)
  
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
  
  class_x <- trclass(x)
  
  if(class_x=="flextable") {
    
    dots <- dots[names(dots)%in%c(names(formals(ftwrite)))]
    args <- c(list(ft=x,
                   file = file,
                   formats=formats.ft,
                   path.format=path.format,
                   subdir=subdir,
                   stamp.format=stamp.format,
                   stamp.funs=stamp.funs,
                   stamp.ft.style=stamp.ft.style
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
                   path.format=path.format,
                   subdir=subdir
                   ),
              dots)
    res <- do.call(datwrite,args)

  }

  if(class_x=="list" || class_x=="gg"){
    
    ## message("Calling ggwrite()")
    ## onefile only makes sense for pdf. Defaulting to TRUE for non-pdf formats
    ## makes ggwrite() emit "onefile can only be used with pdf device" on every
    ## call (e.g. from lwrite()). A user-supplied onefile in ... still overrides.
    args.gg.def <- list(
      onefile="pdf"%in%formats.gg
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
           path.format=path.format,
           subdir=subdir,
           stamp.format=stamp.format,
           stamp.funs=stamp.funs,
           stamp.gg.style=stamp.gg.style),
      args.gg
    )
    
    ## res <- try(do.call(ggwrite,args.ggwrite))
    res <- do.call(ggwrite,args.ggwrite)
  }
  
  invisible(res)
}

