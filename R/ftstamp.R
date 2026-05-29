##' Stamp and write flextab objects to one or multiple formats
##' @param ft a flextable object
##' @param script path to script - will be pasted as caption.
##' @param file The file that the flextable will be written to (no
##'     file is written by this function)
##' @param bg Default background colour is #ffffff.
##' @param time The default is to insert a time stamp taken from result of
##'     Sys.time(). Using the time argument you can overrule this by
##'     setting a fixed string instead. Use "" to omit. 
##' @import flextable
##' @export

## put stamps on tables plus a little tailoring of visuals
ftstamp <- function(ft,file,script,time,model,format.stamp){

    if(missing(file)){
        file <- NULL
    } else {
        file=basename(file)
    }

    if(missing(time)) time <- NULL
    if(missing(model)) model <- NULL
    if(missing(script)) script <- NULL

    format.stamp.0 <- list(size=4,align="right",space=1,bg="#ffffff",color="#666666")
    if(missing(format.stamp)) format.stamp <- NULL
    ## if(is.null(format.stamp)){
    ##     format.stamp <- format.stamp.0
    ## } else {
    ##     format.stamp <- modifyList(format.stamp.0,format.stamp)
    ##     newnames <- setdiff(names(format.stamp),names(format.stamp.0))
    ##     if(length(newnames)) {
    ##         stop(paste("elements not allowed in format.stamp:",paste(newnames,collapse=", ")))
    ##     }
    ## }
  
    format.stamp <- modifyListCheck(x=format.stamp.0,format.stamp,elems.allowed=names(format.stamp.0))
    
    stamp.full <- createStamp(script=script,file=file,time=time,model=model)

    if(is.null(stamp.full)) return(ft)

    nrow.foot.orig <- nrow_part(ft, part = "footer")

    
### why is theme_vanilla applied here? Seems arbitrary.   
    ## ft <- theme_vanilla(ft)
    ft <- add_footer_lines(ft, stamp.full)

    # Target only the new lines dynamically
    nrow.foot.new <- nrow_part(ft, part = "footer")
    rows.stamp <- (nrow.foot.orig+1):nrow.foot.new

    

    ft <- color(ft,i=rows.stamp,part = "footer", color = format.stamp$color)

    ft <- fontsize(ft, 
                   i=rows.stamp,
                   size = format.stamp$size, 
                   part = "footer")
    ft <- align(ft, 
                i=rows.stamp,
                align=format.stamp$align,
                part = "footer")
    
    ## ft <- fontsize(ft, part = "footer", size=6)
    ## ft <- align(ft, part="footer", align = "right")
    ft <- line_spacing(ft, 
                       i=rows.stamp,
                       space = format.stamp$space, 
                       part = "footer")

    if(!is.null(bg)){
        ft <- bg(ft, i=rows.stamp,part = "footer", bg = format.stamp$bg)
    }
    ft
}

