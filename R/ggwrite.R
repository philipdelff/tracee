##' Export plots created with ggplot (and more) or tables to files
##' (png or pdf) - or show them on screen.
##'
##' @param plot A plot object or a list of plots. Normally generated
##'     with ggplot or qplot. But it can also be from grid.arrange or
##'     arrangeGrob with class gtable. That is experimental
##'     though. Not sure exactly what classes are supported.
##' @param file A file to export to. Must end in .png or .pdf. If plot
##'     is a list, see onefile. If missing, plot is shown on screen.
##' @param script This should normally be the path to your
##'     script. Requires ggplot >=2.2.1.
##' @param time Passed to ggwrite. 
##' @param canvas Either a list of height and width or a shortname of
##'     predefined canvas size. See ?canvasSize.
##' @param formats File formats to write to as a character
##'     vector. Must be a subset of c("png","pdf"). Default is to only
##'     write to the format matching the file name extension of
##'     `file`.
##' @param onefile Only applicable if plot is a list. If plot is a
##'     list and onefile=TRUE, all plots will be put in a pdf (file
##'     must end in pdf) with one plot per page. If plot is a list and
##'     onefile=FALSE, numbered files will be created - one per list
##'     element.
##' @param res Resolution. Passed to png.
##' @param save Save the plot to the given file or just show? Defaults
##'     to TRUE. Hint, if you use an "exportFlag", use
##'     save=exportFlag.
##' @param show Print the plot to the screen? Defaults to the opposite
##'     of save. Hint, combining save and show in knitr can give you
##'     both a high quality plot in your pdf and a png optimized for
##'     powerpoint.
##' @param paper Only used with pdf device. See ?pdf.
##' @param useNames If length(plot)>1 use names(plot) in the file
##'     names? Default is to use 1:length(plot). Only used if save is
##'     TRUE, and length(plot)>1.
##' @param quiet Default is false but use TRUE to suppress messages
##'     about what was saved.
##' @export
##' @return Nothing. Files written and/or plots shown, depending on
##'     argument values.
##' @examples
##' library(ggplot2)
##' writeOutput <- FALSE
##' data(ChickWeight)
##' p1 <- ggplot(ChickWeight,aes(Time,weight,group=Chick,colour=factor(Diet)))+geom_line()
##' ggwrite(p1)  ## view plot on screen
##' script <- "note"
##' ggwrite(p1,script=script,canvas="wide",file="myplot1.png",save=writeOutput)
##' @family Plotting
##' @import grDevices
##' @import grid
##' @import data.table
##' @import NMdata

### had to skip this example - lagging data.table
## library(gridExtra)
## tab1 <- pksim1[,.N,by=.(ID,EVID,CMT)]
## tg1 <- tableGrob(tab1)
## ggwrite(tg1,script=script,file="mytab1.png",save=writeOutput)


ggwrite <- function(plot, file, script, time, canvas="standard", formats,
                    onefile=FALSE, res=200, paper="special",
                    save=TRUE, show=!save, useNames=FALSE, quiet=FALSE){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    name.canvas <- NULL
    . <- NULL
    size <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    if(missing(plot) || !exists("plot")){
        stop("An existing plot must be passed as the plot argument.")
    }

    if(useNames && length(plot)==1) warning("useNames is ignored because plot is of length 1.")

    if(!missing(file) && (missing(formats)||is.null(formats))) formats <- fnExtension(file)
    if(is.null(canvas)) canvas <- "standard"
    if(missing(time)) time <- NULL
    
###### functions to be used internally
### print1 does the actual printing to the device. Because if the plot is a
### table it must be written with draw.grid, and if not by print.
    print1 <- function(plot){
        if("gtable"%in%class(plot)) {
            ## message("plot is of class gtable. Using grid::grid.draw.")
            ## grid::grid.draw
            grid.draw(plot)
        } else {
            if(!is.null(plot)){
                print(plot)
            }
        }
    }

    ## make function to use for one plot. Then we will call tht on plot or loop
    ## it over the elements of plot in case plot is a list.
    write1 <- function(plot,fn=NULL,type,onefile=FALSE,size){  
        if(is.null(plot)) {
            message("plot is NULL, nothing to do.")
            return(NULL)
            }
        if(is.null(fn)) fn <- file
        if(!is.null(script)){
            plot <- ggstamp(plot,script,file=fn,time=time)
        }
        
        if(!is.null(fn)&&type!="x11"){
            switch(type,
                   png={
                       png(filename = fn, width = size$width, 
                           height = size$height, units = "in",
                           res=res
                           ## res = 18 * max(width, height)
                           )
                   },
                   pdf={
                       pdf(file = fn, width = size$width, 
                           height = size$height,onefile=onefile,paper = paper)
                   })
            print1(plot)
            dev.off()
        } else {
            print1(plot)
        }
    }
    


###### internal functions done

    
##### Check inputs
    
    if(missing(file)) file <- NULL
    if(!save) {
        file <- NULL
        if(onefile) onefile <- TRUE
    }
    if(is.null(file)) save <- FALSE

    if(missing(script)) script <- NULL

    ## If file is an empty string or null is the same.
    if(!missing(file)&&!is.null(file)){
        file2 <- gsub(" ","",file)
        if(!all(file==file2)) warning("Blank characters in filename have been removed.")
        file <- file2
    }
    
    if(!is.null(file)&&length(file)==1&&file=="") {
        file <- NULL
    }
    

    
#### check inputs done

    
    writeObj <- function(plot,file,size,type){

        ## get filname extension to determine device
        type <- "x11"
        fnroot <- NULL
        if(!is.null(file)){
            ## type <- sub(".+\\.(.+)$","\\1",file)
            
            type <- sub(".*\\.([^\\.]+)$","\\1",file)
            if(!type%in%c("pdf","png")) stop("Only extensions .png and .pdf are supported")
            fnroot <- sub("^(.+)\\..+$","\\1",file)
        }
        
        if(is.list(plot)&&!any(c("gg","gtable")%in%class(plot))) {
            if(onefile){
                if(type!="pdf"){
                    warning("onefile can only be used with pdf device. Will not be used.")
                    onefile <- FALSE
                }
                
                write1(plot,fn=file,type=type,onefile=onefile,size=size)
            } else {
                
                Nplots <- length(plot)
                ## debug
                ## cat("Number of plots: ",Nplots)
                Nplots.log10 <- round(log10(Nplots))
                fname.num <- function(fnroot,type,I) paste(fnroot,"_",sprintf(fmt=paste("%0",Nplots.log10+1,"d",sep=""),I),".",type,sep="")
                fname.char <- function(fnroot,type,name) paste(fnroot,"_",name,".",type,sep="")
                if (type=="x11"){
                    write1(plot[[1]],type="x11")
                    if(Nplots>2){
                        silent <- lapply(2:Nplots,function(I){
                            write1(plot=plot[[I]],type=type,size=size)
                        })
                    }
                } else {
                    if(useNames){
                        silent <- lapply(1:Nplots,function(I)write1(plot=plot[[I]],type=type,fn=fname.char(fnroot,type,name=names(plot)[I]),size=size))
                    } else{
                        silent <- lapply(1:Nplots,function(I)write1(plot=plot[[I]],type=type,fn=fname.num(fnroot,type,I),size=size))
                    }
                }
            }
        } else {
            write1(plot=plot,fn=file,type=type,size=size)
        }
        invisible(NULL)
    }



    if(save){
#### Section start: create data.table with all combinations of formats and canvases ####

        is.chars <- sapply(canvas,is.character)
        ## if more than one canvas is given, lists must be named
        if(
            length(canvas)>1&&is.null(names(canvas)) && any(!is.chars) ){
            stop("If more than one canvas is requested, non-character elements must be named.")
        }
        ## character elements do not need to be named. If they are not, we use the canvas name
        nms <- names(canvas)
        ## get rid of special characters
        
if(all(is.chars) && length(nms)==0) {
            nms <- unlist(canvas)
        }

        nms <- gsub(" ","",nms)
        nms <- gsub("[[:punct:]]","",nms)
        
        
        nms[is.chars&nms==""] <- unlist(canvas[is.chars&nms==""])
        names(canvas) <- nms
        ## check that names are unique
        if(any(duplicated(nms))) stop("canvas names must be unique")

        
        dt.canvas <- do.call(rbind,
                             lapply(canvasSize(canvas,simplify=FALSE),as.data.table)
                             )
        dt.canvas$name.canvas <- names(canvas)

        allcombs <- egdt(data.table(format=formats),
                         dt.canvas,quiet=TRUE)


### Section end: create data.table with all combinations of formats and canvases

        
        n.canvas <- allcombs[,uniqueN(name.canvas)]
        for(n in 1:nrow(allcombs)){
            file.n <- file
            if(n.canvas>1){
                file.n <- fnAppend(file,
                                   allcombs[n,name.canvas]
                                   )
            }
            file.n <- fnExtension(file.n,allcombs[n,format])
            writeObj(plot,file=file.n,size=allcombs[n,.(width,height)])
            if(!quiet&&!is.null(file.n)) message("Written to ",file.n)
        }
    }
    if(show){
        writeObj(plot,file=NULL,size=size)
    }
    invisible(NULL)
}

