##' Add watermarks to ggplots
##' @param text What should be written on the plot
##' @param scale the size
##' @param rot rotation of the mark. Don't remember the unit.
##' @param col the colour (a value, not an expression) of the
##'     watermark. Default is grey.
##' @param alpha alpha value for the watermark. Default is 0.5.
##' @details This used to be based on
##'     https://www.r-bloggers.com/adding-watermarks-to-plots/ That
##'     solution stopped working, and this new solution is simpler and
##'     based on ggplot2 alone.
##' @import ggplot2
##' @return a layer with a watermark that can be added to a plot
##' @family Plotting
##' @examples
##' library(ggplot2)
##' ff <- qplot(1:10, 11:20) + ggwater()
##' @export


ggwater <- function(text="Not validated",scale=1,rot=30,col="grey",alpha=.5){
    
    annotation_custom(textGrob(text, gp = gpar(fontsize = 80*scale,col=col,alpha=alpha),rot=rot),xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)


}
