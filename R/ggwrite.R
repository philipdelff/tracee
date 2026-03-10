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
##'     to TRUE. If a variable is used to control whether a script
##'     generates outputs (say `writeOutputs=TRUE/FALSE`), if you use
##'     `save=writeOutputs` to comply with this.
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
##' @param useNames Deprecated. Use \code{use.names} instead.
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




ggwrite <- function(plot, file, canvas="standard", formats,
                    onefile=FALSE, res=200, paper="special",
                    save=TRUE, show=!save, use.names=FALSE, script, time, model,quiet=FALSE, useNames){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    name.canvas <- NULL
    . <- NULL
    size <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    ## Which arguments were actually supplied by the caller
    args.given <- as.list(match.call())[-1]   # remove function name
    args.x <- argsFromTrace(sys.function(), plot)

    ## inject defaults into local environment
    if (length(args.x)) {
        for (nm in names(args.x)) {
            if (!nm %in% names(args.given)) {
                assign(nm, args.x[[nm]], envir = environment())
            }
        }
    }
    
    if(missing(plot) || !exists("plot")){
        stop("An existing plot must be passed as the plot argument.")
    }

    if(!missing(useNames)){
        if(!missing(use.names)){
            stop("use.names and useNames supplied. Use use.names and not the deprecated useNames.")
        }
        message("useNames is deprecated. Use use.names.")
        use.names <- useNames
    }

    if(!missing(file) && (missing(formats)||is.null(formats))) formats <- fnExtension(file)
    if(is.null(canvas)) canvas <- "standard"
    if(missing(time)) time <- NULL
    if(missing(model)) model <- NULL

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
        if(!all(file==file2)) message("Blank characters in filename have been removed.")
        file <- file2
    }
    
    if( !is.null(file) && length(file)==1 && file=="" ) {
        file <- NULL
    }

#### check inputs done

    if(save){
        ## Resolve all file names and canvas/format combinations
        ## allcombs <- ggwrite_names(file = file, formats = formats, canvas = canvas)

        ## Write each combination
        ## for (n in seq_len(nrow(allcombs))) {
        

        ggwrite_save(
            plot      = plot,
            ## allcombs=allcombs,
            ## file      = allcombs[n, file],
            file      = file,
            ## size      = allcombs[n, .(width, height)],
            script    = script,
            time      = time,
            model     = model,
            onefile   = onefile,
            use.names = use.names,
            quiet     = quiet,
            res       = res,
            paper     = paper,
            formats = formats,
            canvas= canvas
        )
        ## }
    }

    if(show){
        writeObj(plot, file=NULL,  script=script, time=time, res=res, paper=paper,formats=NULL,canvas=NULL)
    }
    invisible(NULL)
}


###### functions to be used internally
### print1 does the actual printing to the device. Because if the plot is a
### table it must be written with draw.grid, and if not by print.
##' @keywords internal
## Don't export

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
###### internal functions done


## I think type should be dropped. formats seem to mean the same thing. And why not use allcombs instead of formats, canvas, etc? 


##' @keywords internal
## Don't export
writeObj <- function(plot,file,script,time,onefile,use.names=FALSE,formats,canvas,quiet=FALSE,...){

    ## get filname extension to determine device
    type <- "x11"
    fnroot <- NULL

    if(missing(formats)) formats <- NULL
    if(is.null(formats)) formats <- fnExtension(type)
    if(is.null(formats)) formats <- "png"

    allcombs <- ggwrite_names(file = file, formats = formats, canvas = canvas)
    
    if(!is.null(file)){
        ## type <- sub(".+\\.(.+)$","\\1",file)
        
        ## type <- sub(".*\\.([^\\.]+)$","\\1",file)

        ## type <- fnExtension(file)
        type <- formats
        ####### TODO can type be of length > 1?  
        if(!type%in%c("pdf","png")) stop("Only extensions .png and .pdf are supported")
        ## fnroot <- sub("^(.+)\\..+$","\\1",file)
        fnroot <- fnExtension(file,"")
    }


    fname.char <- function(fn,name=NULL,...){
        
        dots <- list(...)
        mypaste <- function(...)paste(...,sep="_")

        if(!is.null(name)){
            fn <- fnAppend(fn,name,allow.noext=TRUE) 
        }
        if(length(dots)) {
            str.dots <- do.call(mypaste,dots)
            fn <- fnAppend(fn,str.dots,allow.noext=TRUE)
        } 
        cleanFileNames(fn,allow.slash=TRUE)
    }
    ## fname.num <- function(fn,I){
    ##     paste(fnroot,"_",sprintf(fmt=paste("%0",Nplots.log10+1,"d",sep=""),I),".",type,sep="")

    ##     fnAppend(fn,names) |>
    ##         fnAppend(str.dots)
    ## }

    
    if(is.list(plot)&&!any(c("gg","gtable")%in%class(plot))) {
        if(onefile && type!="pdf"){
            warning("onefile can only be used with pdf device. Will not be used.")
            onefile <- FALSE
        }

        if(onefile){

### extract all canvas names, then save all plots requested with each of the canvas names.
            ##all.canvas.names <- allcombs[,unique(name.file.canvas)]

            
            allcombs[format=="pdf",
                     ## run write1 on all plots. Each row in allcombs is for all plots, not one row per plot
                     write1(plot=plot,
                            type="pdf",fn=fname.char(fn=file,name=NULL,name.file.canvas),
                            size=list(height=height,width=width),script=script,time=time,quiet=quiet,...),by=row]
            allcombs <- allcombs[format!="pdf"]


            ## silent <- lapplydt(allcombs,by="name.file.canvas",fun=function(x)write1(plot[x$row],
            ##                                                                         fn=fnAppend(file,unique(x$name.file.canvas)),
            ##                                                                         script=script,
            ##                                                                         time=time,
            ##                                                                         type="pdf",
            ##                                                                         size=list(width=unique(x$width),height=unique(x$height))))

            ##allcombs <- ggwrite_names(file = file, formats = formats, canvas = canvas)
            
            ## allcombs[,write1(plot,fn=fnAppend())]
            ## write1(plot,fn=file,type=type,onefile=onefile,size=size,script=script,time=time,quiet=quiet,...)
        } 
        
        Nplots <- length(plot)
        ## debug
        ## cat("Number of plots: ",Nplots)

        ## fname.num <- function(fnroot,type,I) paste(fnroot,"_",sprintf(fmt=paste("%0",Nplots.log10+1,"d",sep=""),I),".",type,sep="")
        ## fname.char <- function(fnroot,type,name) paste(fnroot,"_",name,".",type,sep="")

        if (type=="x11"){
            write1(plot[[1]],type="x11")
            if(Nplots>2){
                silent <- lapply(2:Nplots,function(I){
                    write1(plot=plot[[I]],type=type,size=size,script=script,time=time,quiet=quiet)
                })
            }
        } else {

            if(!use.names){
                Nplots.log10 <- round(log10(Nplots))
                names(plot) <- sprintf(fmt=paste("%0",Nplots.log10+1,"d",sep=""),1:Nplots)
            }
            
            
            ## silent <- lapply(1:Nplots,function(I){
            ##     allcombs[,mapply(write1,plot=plot[[I]],type=type,fn=fname.char(fn=file,name=names(plot)[I]),size=list(height=height,width=width),script=script,time=time,quiet=quiet,...)]
            ## })
            
            if(nrow(allcombs)){
                silent <- lapply(1:Nplots,function(I){
                    allcombs[,write1(plot=plot[[I]],type=format,fn=fname.char(fn=file,name=names(plot)[I],name.file.canvas),size=list(height=height,width=width),script=script,time=time,quiet=quiet,...),by=row]

                })
            }
            
        }
    } else {
        ##allcombs[,mapply(write1,plot=plot,type=type,fn=file,size=list(width,height),script=script,time=time,...)]
        
        ## cat("not a list")
        ## allcombs[,mapply(write1,
        ##                  type=format,
        ##                 fn=fnExtension(file,format),
        ##                 size=list(width=width,height=height),
        ##                 ##args that are not taken from allcombs, so "constant" 
        ##                 MoreArgs=list(plot=plot,script=script,time=time,...)
        ##                 )]

        ## allcombs[,apply(.SD,1,write1,
        ##                          type=format,
        ##                         fn=fnExtension(file,format),
        ##                         size=list(width=width,height=height),
        ##                         ##args that are not taken from allcombs, so "constant" 
        ##                         plot=plot,script=script,time=time,...
        ##                 )
        ##          ]


        allcombs[,write1(
            type=format[1],
            fn=fnAppend(fnExtension(file[1],format[1]),name.file.canvas,allow.noext=TRUE),
            size=list(width=width[1],height=height[1]),
            ##args that are not taken from allcombs, so "constant" 
            plot=plot,
            script=script,time=time,quiet=quiet,...
        ),by=row
        ]
        
        
        ##write1(plot=plot,fn=file,type=type,size=size,script=script,time=time,...)
    }
    invisible(NULL)
}


## write1 <- function(x,...){
##     UseMethod(x,...)
## }



## make function to use for one plot. Then we will call tht on plot or loop
## it over the elements of plot in case plot is a list.
write1 <- function(plot,fn=NULL,type,onefile=FALSE,size,script,time,model,quiet=FALSE,...){  
    
    ## print(str(size))
    if(is.null(plot)) {
        message("plot is NULL, nothing to do.")
        return(NULL)
    }
    if(is.null(fn)) fn <- file
    if(!is.null(script)){
        plot <- ggstamp(plot,script=script,file=fn,time=time,model=model)
    }
    
    dots <- try(list(...),silent=T)
    if("try-error"%in%class(dots)) dots <- NULL

    
    
    if(!is.null(fn)&&type!="x11"){
        switch(type,
               png={
                   
                   dots <- dots[intersect(names(dots),"res")]
                   args <- c(list(filename = fn, width = size$width, 
                                  height = size$height, units = "in"),dots)
                   do.call(png,args)
                   ## png(filename = fn, width = size$width, 
                   ##     height = size$height, units = "in",
                   ##     ...
                   ##     ## res=res
                   ##     )
               },
               pdf={

                   dots <- dots[intersect(names(dots),"onefile")]
                   args <- c(list(file = fn, width = size$width, 
                                  height = size$height),dots)
                   do.call(pdf,args)

                   ## pdf(file = fn, width = size$width, 
                   ##     height = size$height,onefile=onefile,...)
               })
        print1(plot)
        dev.off()
        if (!quiet) message("Written to ", fn)
    } else {
        print1(plot)
    }
}



##' Resolve all output file names for ggwrite.
##' Returns a data.table with columns: format, name.canvas, width, height, file
##' @keywords internal
ggwrite_names <- function(file, formats, canvas) {

    name.canvas <- NULL

    if(missing(canvas)) canvas <- NULL
    if(is.null(canvas)) canvas <- "standard"
    if(missing(formats)) formats <- NULL
    if(is.null(formats)) formats <- fnExtension(file)
    
    is.chars <- sapply(canvas, is.character)
    if (length(canvas) > 1 && is.null(names(canvas)) && any(!is.chars)) {
        stop("If more than one canvas is requested, non-character elements must be named.")
    }

    nms <- names(canvas)
    if (all(is.chars) && length(nms) == 0) {
        nms <- unlist(canvas)
    }
    nms <- gsub(" ", "", nms)
    nms <- gsub("[[:punct:]]", "", nms)
    nms[is.chars & nms == ""] <- unlist(canvas[is.chars & nms == ""])
    names(canvas) <- nms

    if (any(duplicated(nms))) stop("canvas names must be unique")

    dt.canvas <- do.call(rbind,
                         lapply(canvasSize(canvas, simplify = FALSE), as.data.table))
    dt.canvas$name.canvas <- names(canvas)

    allcombs <- egdt(data.table(format = formats),
                     dt.canvas, quiet = TRUE)

    n.canvas <- allcombs[, uniqueN(name.canvas)]
    ## allcombs[, file := {
    ##     file.n <- file
    ##     if (n.canvas > 1) {
    ##         file.n <- fnAppend(file, name.canvas)
    ##     }
    ##     fnExtension(file.n, format)
    ## }, by = seq_len(nrow(allcombs))]

    allcombs[,name.file.canvas := ""]
    if (n.canvas > 1) {
        allcombs[,name.file.canvas := name.canvas]
    }

    allcombs[,row := .I]

    allcombs
}


##' Write a single plot to a single file.
##' @keywords internal
ggwrite_save <- function(plot, file, script, time, canvas,onefile, use.names, quiet, ...) {
    writeObj(plot=plot, file = file,  script = script, time = time, canvas=canvas,
             onefile = onefile, use.names = use.names,quiet=quiet, ...)

    invisible(file)
}
