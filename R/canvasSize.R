##' Get predefined canvas sizes
##'
##' @param canvas Can either be a character with one of the values
##'\itemize{
##' \item{"standard"} - A powerpoint standard size, and two can be shown side by side - 12 by 9 inch.
##' \item{"wide"} - For a single wide plot on a powerpoint slide - 16 by 9 inch
##' \item{"A4"} A full A4 page - 9 by 12 inch
##' \item{"square"} As reads - 9 by 9 inch
##' \item{"wide-screen"} For full screen display - 31 by 15 inch
##'}
##' or it can be a
##'     list with elements width and height with single values (unit
##'     is inches). Example: canvas=list(height=5,width=9).
##' @param scale A scale to apply to both directions of the canvas
##'     size. This can be useful in combination with the pre-defined
##'     canvas sizes.
##' @export
##' @family Plotting


### a function that looks up the canvas size
canvasSize <- function(canvas,scale=1){
    possible.canvases <- list(
        standard=list(width=12,height=9),
        wide=list(width=16,height=9),
        A4=list(width=9,height=12),
        square=list(width=9,height=9),
        "wide-screen"=list(width=31,height=15)
    )
    
    ## size of plot
    ## A "screen" version is needed that will save graphics nice to read on screen. Could be like 1.4*standard.
    if(is.list(canvas) ){ if (all(c(!is.null(canvas$height),!is.null(canvas$width)))) {
                              ## todo: width and height must be numerics of length one 
                              return(canvas)
                          } else {
                              stop("Canvas is a list but does not include height and width")
                          }
    } else {
        ## todo must be a character of length one

        ## browser()                
        size.matched <- grep(paste0("^ *",canvas," *$"),names(possible.canvases),ignore.case=T)
        if(length(size.matched)!=1) stop(
                                        paste("canvas has to match exactly one of",paste(names(possible.canvases),collapse=", "),". Matching is not case-sensitive.")
                                    )
        size <- possible.canvases[[size.matched]]
    }
    size <- lapply(size,function(x)x*scale)
    return(size)
}
