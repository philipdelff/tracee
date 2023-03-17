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


ggstamp <- function(plot, script = "", file, time=Sys.time()) {
### Captions are only available in ggplot 2.2.1

### A list of plots is supported so we will run everything with lapply
    plot.was.list <- TRUE
    if(!( length(class(plot))==1 && "list"%in%class(plot) )) {
        plot.was.list <- FALSE
        plot <- list(plot)
    }
    if(missing(file)) file <- NULL
    if(!is.null(file)) file <- basename(file)

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
                                   if(sum(unlist(packageVersion("ggplot2")[1,])*c(1000)^c(2:0))<2002001){
                                           stop("ggplot >= 2.2.1 needed to stamp ggplot objects.")
                                       }
                                   plot+ggplot2::labs(caption=caption)+theme(plot.caption=element_text(size=6, colour="grey"))},
                               gtable={
                                   arrangeGrob(plot, bottom = textGrob(caption, gp=gpar(font=1, col = "grey", cex = 0.5)),heights=c(0.98,0.02))

                               }
                               )
        return(plot.stamped)
    }

    plot <- lapply(plot,stamp1)
    if(!plot.was.list) plot <- plot[[1]]

    return(plot)
    
}
