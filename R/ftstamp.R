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
ftstamp <- function(ft,file,script,bg="#ffffff",time){

    if(missing(file)){
        file <- NULL
    } else {
        file=basename(file)
    }
    if(missing(bg)||is.null(bg)) bg <- "#ffffff"
    if(missing(time)) time <- NULL
    
    ## stamp.full <- paste(format(Sys.time(), "%d-%b-%Y %H:%M"),script, file)
    stamp.full <- createStamp(script=script,file=file,time=time)
    
    ft <- theme_vanilla(ft)
    ft <- add_footer_lines(ft, stamp.full)
    ft <- color(ft, part = "footer", color = "#666666")
    ft <- fontsize(ft, part = "footer", size=6)
    ft <- align(ft, part="footer", align = "right")
    ft <- line_spacing(ft, space = 1, part = "footer")
    if(!is.null(bg)){
        ft <- bg(ft, part = "all", bg = bg)
    }
    ft
}

