##' Always stamp your plots with script name
##'
##' This function is used to stamp ggplot type plots with datetime and
##' script name. User must provide the script name. 
##'
##' @param plot The plot to be stamped.
##' @param script the script name. Date and time will be added
##'     automatically.
##' @param file An optional output filename to be included in the stamp.
##' @param time The timestamp to be included.
##'
##' @return the plot with a stamp
##' @details The stamp is adding using the caption label. If a caption
##'     is already in the plot, the stamp will be added in a new
##'     line.
##' 
##' The caption is derived as
##' caption=paste(c(plot$label$caption,stamp,paste(date.txt,file)),collapse="\\n")
##'
##' ggplot 2.2.1 (which is years old) or newer is required.
##' @return A plot object with the stamp added as caption
##' @import ggplot2
##' @import grid
##' @import patchwork
##' @importFrom gridExtra arrangeGrob
##' @importFrom utils packageVersion
##' @examples
##' library(ggplot2)
##' data(ChickWeight)
##' p1 <- ggplot(ChickWeight,aes(Time,weight,group=Chick,colour=factor(Diet)))+geom_line()
##' script <- "note"
##' ggstamp(p1,script)
##' ## Or use ggwrite which will call ggstamp when the `script` argument is provided.
##' ggwrite(p1,script=script,canvas="wide")
##' @family Plotting
##' @export


ggstamp <- function(plot, file, script, time, model, format.stamp) {
### Captions are only available in ggplot 2.2.1

### A list of plots is supported so we will run everything with lapply
    plot.was.list <- TRUE
    if(!( length(class(plot))==1 && "list"%in%class(plot) )) {
        plot.was.list <- FALSE
        plot <- list(plot)
    }
    if(missing(file)) file <- NULL
    if(!is.null(file)) file <- basename(file)

    if(missing(time)) time <- NULL
    if(missing(model)) model <- NULL
    if(missing(script)) script <- NULL

    if(missing(format.stamp)) format.stamp <- NULL
    

    stamp1 <- function(plot){
        caption.existing <- NULL
### determine method to use. otype is object type
        otype <- NA
        if("ggplot"%in%class(plot)||is.ggplot(plot)){
####### for single ggplot objects
            otype <- "ggplot"
            caption.existing <- try(plot$label$caption)
        }
        if("gtable"%in%class(plot)){
            if(!is.na(otype)) stop("Confused. type both ggplot and gtable. Dont knot how to stamp this object.")
######## for gtables as returned by arrangeGrob and grid.arrange
            otype <- "gtable"
        }
        if("ggmatrix"%in%class(plot)){
            if(!is.na(otype)) stop("Confused. type both ggmatrix and ggplot or gtable. Dont know how to stamp this object.")
######## ggmatrix can be stamped just like ggplot. But the existing caption will have to be extracted differently. 
            otype <- "ggplot"
            caption.existing <- try(plot$gg$labs$caption)
        }
        if(is.na(otype)) stop("Dont know how to stamp this object type.")
        if("try-error" %in% class(caption.existing)) caption.existing  <- ""
        
        caption <- createStamp(script=script,file=file,time=time,addto=caption.existing)
        
        plot.stamped <- switch(otype,
                               ggplot={
                                   if(packageVersion("ggplot2")<"2.2.1"){
                                       stop("ggplot >= 2.2.1 needed to stamp ggplot objects.")
                                   }
                                   format.stamp <- modifyListCheck(format.stamp,
                                                                   x=list(size=6, 
                                                                          colour="#666666",
                                                                          hjust=1,
                                                                          vjust=1),
                                                                   elems.allowed=setdiff(names(formals(element_text)),"..."))

                                   ## plot+ggplot2::labs(tag = caption) +
                                   ##     theme(
                                   ##         plot.tag = do.call(element_text, format.stamp),
                                   ##      # 1 is the right edge of the panel, 0 is the bottom
                                   ##         plot.tag.position = c(1, 0), 
                                   ##      # Use 'plot' to align relative to the panel, not the outer margin
                                   ##         plot.tag.location = "plot", 
                                   ##      # vjust < 0 pushes it down into the margin area without shrinking the plot
                                   ##         plot.tag.vjust = 2 
                                   ##     )

####### caption on the same plot
                                   ## plot+ggplot2::labs(caption = caption) +
                                   ##     theme(
                                   ##         plot.caption = do.call(element_text, format.stamp)
                                   ##         )
### caption as separate plot, using patchwork
plot / plot_annotation(
  caption = caption,
  theme=theme(
      plot.caption = do.call(element_text, format.stamp)
                                           )

)


                               },
                               gtable={
                                   format.stamp <- modifyListCheck(format.stamp,
                                                                   x=list(font=1, col = "grey", cex = 0.5),
                                                                   elems.allowed=setdiff(names(formals(gpar)),"..."))
                                   arrangeGrob(plot, bottom = textGrob(caption, gp=do.call(gpar,format.stamp)),heights=c(0.98,0.02))
                               }
                               )
        return(plot.stamped)
    }

    plot <- lapply(plot,stamp1)
    if(!plot.was.list) plot <- plot[[1]]

    return(plot)
    
}
