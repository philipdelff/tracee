##' Stamp and write flextab objects (deprecated - use ftstamp)
##'
##' Deprecated - use `ftstamp()` instead.
##' @param ft a flextable object
##' @param script path to script - will be pasted as caption.
##' @param file The file that the flextable will be written to (no
##'     file is written by this function)
##' @param bg Default bacground colour is #ffffff.
##' @param time The default is to insert a time stamp taken from result of
##'     Sys.time(). Using the time argument you can overrule this by
##'     setting a fixed string instead. Use "" to omit. 
##' @return A flextable object with the stamp added as a footer line.
##' @import flextable
##' @export

## put stamps on tables plus a little tailoring of visuals
stampFlextab <- function(ft,file,script,bg="#ffffff",time){
    .Deprecated(new="ftstamp")
    ftstamp(ft,file,script,stamp.ft.style=list(bg="#ffffff"),time)
}

