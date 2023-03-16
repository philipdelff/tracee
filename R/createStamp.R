##' Create a stamp with script path, output file path and time stamp
##' in it.
##' @param script The script path
##' @param file the output file path
##' @param time The default is to insert a time stamp taken from
##'     result of Sys.time(). Using the time argument you can overrule
##'     this by setting a fixed string instead. Use "" to omit.
##' @param addto An existing caption to keep above the caption created
##'     by this function.
##' @keywords internal
##' 
## do not export.

createStamp <- function(script,file,time,addto){

    if(missing(time)||is.null(time)||(is.logical(time)&&time)) time <- Sys.time()
    if(is.logical(time)&&!time) time <- ""
    if(missing(addto)) addto <- NULL
    if(inherits(time,"POSIXt")){
        time <- format(time, "%d-%b-%Y %H:%M")
    }
    caption.stamp <- paste(time,file)
    caption <- paste(c(addto,script,caption.stamp),collapse="\n")
    
    caption
    
}
