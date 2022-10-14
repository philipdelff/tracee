##' Change file name extension
##' 
##' Very simple but often applicable function to retrieve or change
##' the file name extension (from say file.lst to file.mod)
##' @param fn file name. Often ending in an extension after a period
##'     but the extension is not needed.
##' @param ext new file name extension. If omitted or NULL, the
##'     extension of fn is returned.
##' @return A text string
##' @keywords internal


### don't export. This is only needed until we have NMdata 0.0.14.

fnExtensionTracee <- function(fn,ext){

    if(missing(ext) || is.null(ext)){
        return(sub(".*\\.([^.]*)","\\1",x=fn))
    }

    ext <- sub("^ *\\.{0,1}(.+)","\\.\\1",ext)
    fn.new <- sub("\\.[^\\.]+$","",fn)
    fn.new <- paste0(fn.new,ext)
    fn.new
}
