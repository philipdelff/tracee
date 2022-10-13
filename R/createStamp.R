
## do not export.

createStamp <- function(script,file,time,addto){

    if(missing(time)||is.null(time)) time <- Sys.time()
    if(missing(addto)) addto <- NULL
    if(inherits(time,"POSIXt")){
        time <- format(time, "%d-%b-%Y %H:%M")
    }
    caption.stamp <- paste(time,file)
    caption <- paste(c(addto,script,caption.stamp),collapse="\n")
    
    caption
    
}
