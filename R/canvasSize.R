##' Get predefined canvas sizes
##'
##' @param canvas Can either be a character with one of the values
##'     \itemize{
##' \item{"standard"} A powerpoint standard size, and
##'     two can be shown side by side - 7.2 by 5.4 in (WxH).
##' \item{"wide"} For a single wide plot on a powerpoint slide - 9.6 by 5.4 in
##' \item{"A4"} A full A4 page - 5.4 by 7.2 in
##' \item{"square"} As reads - 5.4 by 5.4 in
##' \item{"wide-screen"} For full screen display - 18.6 by 9 in
##' }
##' or it can be a list with elements
##'     width and height with single values (unit is inches). Example:
##'     canvas=list(height=5,width=9).
##' @param scale A scale to apply to both directions of the canvas
##'     size. This can be useful in combination with the pre-defined
##'     canvas sizes.
##' @param simplify If only one canvas returned and simplify=TRUE, the
##'     result will not be wrapped in a list. For programming, you
##'     most likely want simplify=FALSE to be sure to always get the
##'     same format back.
##' @return A list with numerical elements width and height as used by
##'     ggwrite.
##' @export
##' @family Plotting


### a function that looks up the canvas size
canvasSize <- function(canvas,scale=1,simplify=TRUE){

    possible.canvases <- list(
        standard=list(width=7.2,height=5.4),
        wide=list(width=9.6,height=5.4),
        A4=list(width=5.4,height=7.2),
        square=list(width=5.4,height=5.4),
        "wide-screen"=list(width=18.6,height=9)

        ## standard=list(width=12,height=9),
        ## wide=list(width=16,height=9),
        ## A4=list(width=9,height=12),
        ## square=list(width=9,height=9),
        ## "wide-screen"=list(width=31,height=15)
    )
    
    unfold.canvas <- function(canvas){
        
        if(length(canvas)==1 && is.character(canvas)){
            size.matched <- grep(paste0("^ *",canvas," *$"),names(possible.canvases),ignore.case=TRUE)
            if(length(size.matched)!=1) stop(
                                            paste("If a character string, canvas to match exactly one of",paste(names(possible.canvases),collapse=", "),". Matching is not case-sensitive.")
                                        )
            canvas <- possible.canvases[[size.matched]]
        }

        

        ## if(!is.list(canvas)||!all(c(!is.null(canvas$height),!is.null(canvas$width)))){
        if(!is.list(canvas)||any(is.null(canvas$height),is.null(canvas$width))){
            stop("canvas must be either a single character string or a list. If a list, it must contain elements, height and width.")
        }
        
        canvas <- lapply(canvas,function(x)x*scale)
        canvas
    }

    
    if(is.list(canvas) && !is.null(canvas$height) && !is.null(canvas$width) ){
        canvas <- list(canvas)
    }
    canvas <- lapply(canvas,unfold.canvas)
    if(simplify && length(canvas)==1){
        canvas <- canvas[[1]]
    }
    return(canvas)
}
