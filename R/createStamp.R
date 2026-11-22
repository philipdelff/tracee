##' Create a stamp with script path, output file path and time stamp
##' in it.
##' @param script The script path
##' @param file the output file path
##' @param time The default is to insert a time stamp taken from
##'     result of Sys.time(). Using the time argument you can overrule
##'     this by setting a fixed string instead. Use "" to omit.
##' @param model An optional model name or object to be included in
##'     the stamp.
##' @param addto An existing caption to keep above the caption created
##'     by this function.
##' @param stamp.format A template for the stamp text using the
##'     placeholders \code{\%script}, \code{\%time}, \code{\%model},
##'     and \code{\%file}. Use \code{"\\n"} for line breaks. Placeholders
##'     without a value are removed, together with the resulting
##'     repeated spaces and empty lines. Default is
##'     \code{"\%file\\n\%script\\n\%model\\n\%time"}.
##' @param stamp.funs Named list of functions used to preprocess
##'     \code{script}, \code{time}, \code{model}, and/or \code{file}
##'     before they are substituted into \code{stamp.format}. Each
##'     function takes the value as its single argument and returns a
##'     character string. By default, values are prefixed with
##'     \code{"Output:"}, \code{"Script:"}, \code{"Model:"} and
##'     \code{"Time:"}, and \code{time} is formatted as
##'     \code{"\%d-\%b-\%Y \%H:\%M"} if it is a \code{POSIXct}. Elements not supplied keep their defaults, so
##'     e.g. \code{stamp.funs = list(file = basename)} only changes
##'     \code{file}. Model and script objects (lists) are reduced to their
##'     \code{label} element before preprocessing.
##' @return A character string, or \code{NULL} if neither
##'     \code{script} nor \code{model} is given.
##' @keywords internal
## do not export.

createStamp <- function(file,script,time,model,addto,stamp.format,
                        stamp.funs=list(script=function(x) paste("Script:",x),
                                        time=function(x) paste("Time:",if(inherits(x,"POSIXt")) format(x,"%d-%b-%Y %H:%M") else x),
                                        model=function(x) paste("Model:",x),
                                        file=function(x) paste("Output:",x))){

  if(missing(script)) script <- NULL
  if(missing(time)) time <- NULL
  if(missing(model)) model <- NULL
  if(missing(file)) file <- NULL
  if(missing(addto)) addto <- NULL
  if(missing(stamp.format) || is.null(stamp.format)) stamp.format <- "%file\n%script\n%model\n%time"

  if(is.null(script) && is.null(model)) return(NULL)

  ## Defaults are taken from the signature. A partial stamp.funs list
  ## (or NULL, as passed on by the writers) only replaces the elements
  ## it contains.
  funs.default <- eval(formals(sys.function())$stamp.funs)
  if(missing(stamp.funs)) stamp.funs <- NULL

  if(!is.null(stamp.funs)){
    if(!is.list(stamp.funs) || is.null(names(stamp.funs)) || any(names(stamp.funs)=="")){
      stop("stamp.funs must be a named list of functions.")
    }
    unsupported.funs <- setdiff(names(stamp.funs), names(funs.default))
    if(length(unsupported.funs)){
      stop("Unsupported element(s) in stamp.funs: ",
           paste(unsupported.funs, collapse=", "),
           ". Supported elements are script, time, model, and file.")
    }
    if(!all(vapply(stamp.funs, is.function, logical(1)))){
      stop("All elements of stamp.funs must be functions.")
    }
    funs.default[names(stamp.funs)] <- stamp.funs
  }

  ## Normalize inputs before preprocessing: model/script objects are
  ## reduced to their label, and logical time means now/nothing.
  if(is.list(script)) script <- script$label
  if(is.list(model)) model <- model$label
  if(is.logical(time)) time <- if(isTRUE(time)) Sys.time() else ""

  ## Check for unsupported placeholders (e.g. typos like %scrpt)
  placeholders <- regmatches(stamp.format, gregexpr("%[[:alpha:]]+", stamp.format))[[1]]
  unsupported <- setdiff(placeholders, c("%script","%time","%model","%file"))
  if(length(unsupported)){
    stop("Unsupported placeholder(s) in stamp.format: ",
         paste(unsupported, collapse=", "),
         ". Supported placeholders are %script, %time, %model, and %file.")
  }

  values <- list(script=script, time=time, model=model, file=file)
  stamp <- stamp.format
  for(nm in names(values)){
    x <- values[[nm]]
    val <- if(is.null(x) || (is.character(x) && all(!nzchar(x)))) "" else as.character(funs.default[[nm]](x))
    if(is.null(val) || length(val)==0 || is.na(val[1])) val <- ""
    stamp <- gsub(paste0("%",nm), paste(val,collapse=" "), stamp, fixed=TRUE)
  }

  ## Clean up after empty placeholders: collapse repeated spaces, trim
  ## each line, and drop empty lines.
  lines <- strsplit(stamp, "\n", fixed=TRUE)[[1]]
  lines <- trimws(gsub(" {2,}", " ", lines))
  lines <- lines[nzchar(lines)]

  paste(c(addto,lines),collapse="\n")
}
