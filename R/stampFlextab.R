##' Stamp and write flextab objects to one or multiple formats
##' @param ft a flextable object
##' @param script path to script - will be pasted as caption.
##' @param file The file that the flextable will be written to (no
##'     file is written by this function)
##' @param bg Default bacground colour is #ffffff.
##' @param time The default is to insert a time stamp taken from result of
##'     Sys.time(). Using the time argument you can overrule this by
##'     setting a fixed string instead. Use "" to omit. 
##' @import flextable
##' @export

## put stamps on tables plus a little tailoring of visuals
stampFlextab <- function(ft,file,script,bg="#ffffff",time){
    .Deprecated(new="ftstamp")
    ftstamp(ft,file,script,bg="#ffffff",time)
}

