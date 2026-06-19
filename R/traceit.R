##' Add tracing attributes to R objects for automated output writing
##'
##' The \code{traceit} family of functions attaches metadata to R objects that can be
##' used later by writer functions to automatically determine output settings. This
##' allows you to "trace" or "tag" objects with information about how they should be
##' written, then write them later without having to re-specify those settings.
##'
##' @param x An R object to attach trace attributes to. Can be a \code{ggplot},
##'   \code{flextable}, or \code{data.frame} object.
##' @param ... Named arguments that will be stored as trace attributes. For ggplot
##'   objects, valid arguments match those in \code{\link{ggwrite}}. For flextable
##'   objects, valid arguments match those in \code{\link{ftwrite}}. For data.frame
##'   objects, valid arguments match those in \code{\link[NMdata]{NMwriteData}}.
##' @param quite Logical. If \code{TRUE}, suppresses messages. Default is \code{TRUE}.
##'   Note: this parameter name appears to be a typo and should likely be "quiet".
##'
##' @details
##' The \code{traceit} function is a generic that dispatches to specific methods based
##' on the class of \code{x}:
##' \itemize{
##'   \item \code{traceit.ggplot}: For ggplot objects
##'   \item \code{traceit.flextable}: For flextable objects
##'   \item \code{traceit.data.frame}: For data frames
##'   \item \code{traceit.traceit}: For objects that already have traceit attributes
##'         (allows updating existing trace information)
##' }
##'
##' When an object is traced, two attributes are added:
##' \itemize{
##'   \item \code{args}: A list of arguments to be used when writing the object
##'   \item \code{class}: The class vector is prepended with "traceit" to indicate
##'         the object has been traced
##' }
##'
##' Arguments passed to \code{traceit} are filtered to include only those that are
##' valid for the corresponding writer function. Invalid arguments are silently ignored.
##'
##' When calling \code{traceit} on an already-traced object, the new arguments are
##' merged with existing trace arguments using \code{\link[utils]{modifyList}}, with
##' new arguments taking precedence.
##'
##' @return Returns \code{x} invisibly with trace attributes attached. The object
##'   can be used normally and the trace attributes will be preserved.
##'
##' @section Extracting Trace Attributes:
##' To extract trace attributes from an object, use \code{\link{argsFromTrace}}.
##'
##' @seealso
##' \code{\link{ggwrite}} for writing ggplot objects,
##' \code{\link{ftwrite}} for writing flextable objects,
##' \code{\link[NMdata]{NMwriteData}} for writing data frames,
##' \code{\link{argsFromTrace}} for extracting trace attributes,
##' \code{\link{writer}} for a unified writing interface
##'
##' @examples
##' \dontrun{
##' library(ggplot2)
##' library(flextable)
##'
##' # Create a plot and trace it with output settings
##' p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
##' p1_traced <- traceit(p1, canvas = "wide", formats = c("png", "pdf"))
##'
##' # The traced plot can be written later without re-specifying settings
##' # writer() will automatically use the traced arguments
##'
##' # Trace a flextable
##' ft1 <- flextable(head(mtcars))
##' ft1_traced <- traceit(ft1, formats = "docx")
##'
##' # Update trace attributes on an already-traced object
##' p1_traced <- traceit(p1_traced, canvas = "standard")  # overrides previous canvas
##'
##' # Trace a data frame for NMwriteData
##' df1 <- data.frame(ID = 1:10, DV = rnorm(10))
##' df1_traced <- traceit(df1, file = "mydata.csv")
##' }
##'
##' @export
traceit <- function(x,...){
    UseMethod("traceit")
}

##' @rdname traceit
##' @method traceit ggplot
##' @export
traceit.ggplot <- function(x,...,quite=TRUE){


    ## add rags as attributes
    dots <- list(...)
    args <- dots[names(dots)%in%names(formals(ggwrite))]

    setattr(x,"args",args)
    ## add traceit class
    setattr(x,"class",c("traceit",class(x)))

    invisible(x)
}

##' @rdname traceit
##' @method traceit flextable
##' @export
traceit.flextable <- function(x,...,quite=TRUE){

    ## add rags as attributes
    dots <- list(...)
    args <- dots[names(dots)%in%names(formals(ftwrite))]

    setattr(x,"args",args)
    ## add traceit class
    setattr(x,"class",c("traceit",class(x)))

    invisible(x)
}

##' @rdname traceit
##' @method traceit data.frame
##' @import NMdata
##' @export
traceit.data.frame <- function(x,...,quite=TRUE){

    ## add rags as attributes
    dots <- list(...)
    args <- dots[names(dots)%in%names(formals(NMwriteData))]

    setattr(x,"args",args)
    ## add traceit class
    setattr(x,"class",c("traceit",class(x)))

    invisible(x)
}

##' @rdname traceit
##' @method traceit traceit
##' @export
traceit.traceit <- function(x,...,quite=TRUE){


    ## add rags as attributes
    dots <- list(...)

## trclass ignores traceit class
  class_x <- trclass(x)

  args <- switch(trclass(x),
                 gg=dots[names(dots)%in%names(formals(ggwrite))],
                 data.frame=dots[names(dots)%in%names(formals(NMwriteData))],
                 ## list=,
                 flextable=dots[names(dots)%in%names(formals(ftwrite))]
                 )

    args.old <- attr(x,"args")
    args <- modifyList(args.old,args)

    setattr(x,"args",args)

    invisible(x)
}
