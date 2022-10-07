
## do not export.

createStamp <- function(script,file,time,addto){

    if(missing(time)||is.null(time)) time <- Sys.time()
    if(missing(addto)) addto <- NULL
    
    date.txt <- format(time, "%d-%b-%Y %H:%M")
    caption.stamp <- paste(date.txt,file)
    caption <- paste(c(addto,script,caption.stamp),collapse="\n")
    
    caption
    
}
