##' Stamp and write flextab objects to one or multiple formats
##' @param ft a flextable object
##' @param script path to script - will be pasted as caption.
##' @param file The file that the flextable will be written to (no
##'     file is written by this function)
##' @param time The default is to insert a time stamp taken from result of
##'     Sys.time(). Using the time argument you can overrule this by
##'     setting a fixed string instead. Use "" to omit.
##' @param model Character string or model object specifying the model name/identifier.
##'     Used for stamping. If a list (model object) is provided, the \code{$label}
##'     element is extracted.
##' @param stamp.ft.style A list specifying formatting options for the stamp. Valid
##'     elements include: \code{size} (font size, default 4), \code{align} (text
##'     alignment, default "right"), \code{space} (line spacing, default 1),
##'     \code{bg} (background color, default "#ffffff"), and \code{color} (text
##'     color, default "#666666"). Any unspecified elements use the defaults.
##' @param stamp.format A template for the stamp text using the
##'     placeholders \code{\%script}, \code{\%time}, \code{\%model}, and
##'     \code{\%file}, with \code{"\\n"} for line breaks. Default is
##'     \code{"\%file\\n\%script\\n\%model\\n\%time"}. See \code{\link{createStamp}}.
##' @param stamp.funs Optional named list of functions (elements \code{script},
##'     \code{time}, \code{model}, \code{file}) used to preprocess the values
##'     before they are inserted into \code{stamp.format}. See
##'     \code{\link{createStamp}}.
##' @return A flextable object with the stamp added as a footer line.
##' @import flextable
##' @export

## put stamps on tables plus a little tailoring of visuals
ftstamp <- function(ft,file,script,time,model,stamp.ft.style,stamp.format=NULL,stamp.funs=NULL){

    if(missing(file)) file <- NULL

    if(missing(time)) time <- NULL
    if(missing(model)) model <- NULL
    if(missing(script)) script <- NULL

    stamp.ft.style.0 <- list(size=4,align="right",space=1,bg="#ffffff",color="#666666")
    if(missing(stamp.ft.style)) stamp.ft.style <- NULL
    ## if(is.null(stamp.ft.style)){
    ##     stamp.ft.style <- stamp.ft.style.0
    ## } else {
    ##     stamp.ft.style <- modifyList(stamp.ft.style.0,stamp.ft.style)
    ##     newnames <- setdiff(names(stamp.ft.style),names(stamp.ft.style.0))
    ##     if(length(newnames)) {
    ##         stop(paste("elements not allowed in stamp.ft.style:",paste(newnames,collapse=", ")))
    ##     }
    ## }
  
    stamp.ft.style <- modifyListCheck(x=stamp.ft.style.0,stamp.ft.style,elems.allowed=names(stamp.ft.style.0))
    
    stamp.full <- createStamp(script=script,file=file,time=time,model=model,
                              stamp.format=stamp.format,stamp.funs=stamp.funs)

    if(is.null(stamp.full)) return(ft)

    nrow.foot.orig <- nrow_part(ft, part = "footer")

    
### why is theme_vanilla applied here? Seems arbitrary.   
    ## ft <- theme_vanilla(ft)
    ft <- add_footer_lines(ft, stamp.full)

    # Target only the new lines dynamically
    nrow.foot.new <- nrow_part(ft, part = "footer")
    rows.stamp <- (nrow.foot.orig+1):nrow.foot.new

    

    ft <- color(ft,i=rows.stamp,part = "footer", color = stamp.ft.style$color)

    ft <- fontsize(ft, 
                   i=rows.stamp,
                   size = stamp.ft.style$size, 
                   part = "footer")
    ft <- align(ft, 
                i=rows.stamp,
                align=stamp.ft.style$align,
                part = "footer")
    
    ## ft <- fontsize(ft, part = "footer", size=6)
    ## ft <- align(ft, part="footer", align = "right")
    ft <- line_spacing(ft, 
                       i=rows.stamp,
                       space = stamp.ft.style$space, 
                       part = "footer")

    if(!is.null(stamp.ft.style$bg)){
        ft <- bg(ft, i=rows.stamp,part = "footer", bg = stamp.ft.style$bg)
    }
    ft
}

