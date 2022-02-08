##' A custom theme - aim is powerpoint
##' @param theme0 The theme to modify
##' @param ... Arguments passed to theme() after default values have
##'     been set. This means ... can be used to overwrite the
##'     defaults.
##' @import ggplot2
##' @family Plotting
##' @export

theme_pp <- function(theme0 = theme_bw,...){
    theme1 <- 
        theme0()+

    ## title font and margins
    theme(plot.title=element_text(size=18,face="bold",hjust=.5,margin=unit(c(3,0,3,0),"mm")))+

### axis
    theme(axis.text = element_text(size = 15,colour="black"),
### distance from axis tick labels (values) to tick marks
          axis.text.x = element_text(margin=unit(c(2, 0, 0, 0), "mm")),
          axis.text.y = element_text(margin=unit(c(0, 2, 0, 0), "mm")),
### legnth of tick marks
          axis.ticks.length  = unit(2,"mm"),
          legend.position="bottom"
          )
    
    ##        theme(plot.title=element_text(size=18,face="bold",hjust=.5,margin=unit(c(
    ##                                                                       3,0,3,0),"mm")))+
    theme1 <- theme1 +
        theme(legend.title=element_blank())+
        ## increment font size in legend
        theme(legend.text=element_text(size=12,colour="black"))+
### axis
        theme(axis.title= element_text(size = 16,colour="black"),
              axis.title.x = element_text(margin = unit(c(6, 0, 6, 0), "mm")),
              axis.title.y = element_text(margin = unit(c(0, 6, 0, 0), "mm"))
              )+
        ## ### text size in facet labels
        theme(strip.text = element_text(size = 16,colour="black"))+
        ## ### background colour for the facet labels
        theme(strip.background=element_rect(fill="white",colour="black" ))+
        ## ### black frame, black tick labels?
        ##    theme(panel.border = element_blank(colour = "white"))+
        ## ### remove grid lines
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())+
        theme(panel.spacing = unit(4, "lines"))+
        ## Captions are only available in ggplot 2.2.1
        theme(plot.caption=element_text(size=6,
                                        colour="grey"))

    ## if(!missing(...)) this.theme <- this.theme()+theme(...)
### taking a chance here - not sure. If it does, we can just add it
### without using this.theme as name at all.

    theme1+theme(...)

}

##' a version of theme_pp suited for facet'ed plots
##' @param theme0 The theme to modify
##' @param ... Arguments passed to theme() after default values have
##'     been set. This means ... can be used to overwrite the
##'     defaults.
##' @import ggplot2
##' @family Plotting
##' @export
theme_pp_facet <- function(theme0 = theme_bw,...){
    theme1 <- 
        theme0()+
        theme(strip.text       = element_text(size = 16,colour="black"),
              axis.text        = element_text(size = 15,colour="black"),
              legend.title     = element_blank(),
              legend.text      = element_text(size = 16,colour="black"),
              strip.background = element_rect(fill="white",colour="black" ),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              plot.caption     = element_text(size=6,
                                        colour="grey"),
              axis.title= element_text(size = 16,colour="black"),
              legend.position="bottom")
    
    theme1+theme(...)

}
