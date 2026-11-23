##' Export ggplot plots (and related objects) to files with stamping support
##'
##' Writes \code{ggplot} plots, \code{gtable} objects, or lists thereof to PNG or PDF
##' files with optional stamping (caption with script name, timestamp, and model information).
##' Supports flexible canvas sizing, multiple output formats, and both single-file and
##' multi-file output for lists of plots.
##'
##' @param plot A plot object or a list of plot objects. Normally generated with
##'   \code{ggplot} or \code{qplot}. Can also be \code{gtable} objects from
##'   \code{\link[gridExtra]{grid.arrange}} or \code{\link[gridExtra]{arrangeGrob}}.
##'   Support for \code{gtable} is experimental.
##' @param file Character string specifying the output file path. Must end in
##'   \code{.png} or \code{.pdf}. If missing and \code{save = TRUE}, an error is raised.
##'   If missing and \code{save = FALSE}, the plot is shown on screen instead.
##'   For lists of plots, see \code{onefile} for naming behavior.
##' @param script Character string or path to the script file. If provided, a stamp
##'   (caption) will be added to the plot showing the script name, timestamp, and
##'   optionally the model. Requires \code{ggplot2} >= 2.2.1.
##' @param time Character string or POSIXct timestamp. By default, a timestamp is
##'   included if \code{script} is provided. You can pass a custom time string or
##'   use \code{time = ""} to omit the timestamp.
##' @param model Character string or model object specifying the model name/identifier.
##'   Used for stamping and for organizing output directories via \code{fun.path}.
##' @param canvas Either a list with \code{height} and \code{width} elements (in inches),
##'   or a character string shortname of a predefined canvas size. Default is
##'   \code{"standard"}. See \code{\link{canvasSize}} for available sizes and custom
##'   size specification. Multiple canvas sizes can be specified as a named list to
##'   generate outputs at different sizes.
##' @param formats Character vector specifying file format(s) to write. Must be a
##'   subset of \code{c("png", "pdf")}. Default is to use the format matching the
##'   \code{file} extension. Multiple formats can be specified to write the same
##'   plot in different formats.
##' @param onefile Logical. Only applicable if \code{plot} is a list. If \code{TRUE},
##'   all plots are written to a single multi-page PDF (\code{file} must end in
##'   \code{.pdf}). If \code{FALSE}, numbered files are created - one per list element.
##'   Default is \code{FALSE}.
##' @param res Numeric. Resolution in pixels per inch for PNG output. Default is 200.
##'   Passed to \code{\link[grDevices]{png}}.
##' @param save Logical. Should the plot be saved to file? Default is \code{TRUE}.
##'   Set to \code{FALSE} to only display the plot without saving. Useful when
##'   controlled by a flag variable (e.g., \code{save = writeOutputs}).
##' @param show Logical. Should the plot be displayed on screen? Default is the
##'   opposite of \code{save} (i.e., \code{!save}). Setting both \code{save = TRUE}
##'   and \code{show = TRUE} in knitr documents can display the plot in the rendered
##'   document while also saving high-quality files.
##' @param paper Character string specifying paper type. Only used with PDF device.
##'   Default is \code{"special"}. See \code{\link[grDevices]{pdf}} for options.
##' @param use.names Logical. If \code{length(plot) > 1}, should \code{names(plot)}
##'   be used in the output file names? Default is \code{FALSE}, which uses numeric
##'   indices (1, 2, 3, ...). Only used if \code{save = TRUE} and \code{plot} is a list.
##' @param quiet Logical. If \code{TRUE}, suppresses messages about files being written.
##'   Default is \code{FALSE}.
##' @param subdir Character string specifying a subdirectory within the path structure.
##'   This is passed to the path function generated from \code{path.format}.
##'   Default is \code{NULL}.
##' @param name.all.pdf Character string specifying the filename to use when writing
##'   all plots to a single PDF file (when \code{onefile = TRUE}). Only used when
##'   \code{file = ""}. Default is \code{"all.pdf"}.
##' @param path.format Character string or function defining the output path.
##'   A template using \code{\%model}, \code{\%script}, and \code{\%name}
##'   placeholders, or a function as returned by \code{\link{pathFormat}}. Passed to
##'   \code{\link{pathFormat}}. Default (\code{NULL}) uses \code{file} as is.
##' @param fun.path Deprecated. Use \code{path.format}.
##' @param method.png Character string, one of \code{"png"} or \code{"ragg"}, controlling
##'   which graphics device is used for PNG output. \code{"png"} uses the base
##'   \code{\link[grDevices]{png}} device; \code{"ragg"} uses \code{\link[ragg]{agg_png}},
##'   which produces output that renders more consistently across platforms (recommended
##'   for reproducible/snapshot-tested figures). Default is \code{"png"}.
##' @param stamp.format A template for the stamp text using the
##'     placeholders \code{\%script}, \code{\%time}, \code{\%model}, and
##'     \code{\%file}, with \code{"\\n"} for line breaks. Default is
##'     \code{"\%file\\n\%script\\n\%model\\n\%time"}. See \code{\link{createStamp}}.
##' @param stamp.funs Optional named list of functions (elements \code{script},
##'     \code{time}, \code{model}, \code{file}) used to preprocess the values
##'     before they are inserted into \code{stamp.format}. See
##'     \code{\link{createStamp}}.
##' @param stamp.gg.style A list of formatting options for the stamp text,
##'     passed to \code{\link{ggstamp}}. For ggplot objects, elements accepted
##'     by \code{ggplot2::element_text()} (e.g. \code{size}, \code{colour},
##'     \code{hjust}).
##' @param useNames Deprecated. Use \code{use.names} instead.
##'
##' @details
##' The function handles both single plot objects and lists of plots with flexible
##' output options:
##'
##' \strong{Single plots:}
##' \itemize{
##'   \item Written to \code{file} with specified format(s)
##'   \item Can generate multiple formats simultaneously
##'   \item Can generate multiple canvas sizes simultaneously
##' }
##'
##' \strong{Lists of plots:}
##' \itemize{
##'   \item If \code{onefile = TRUE}: creates a single multi-page PDF
##'   \item If \code{onefile = FALSE}: creates separate files (numbered or named)
##'   \item Automatic naming based on list element names or indices
##' }
##'
##' @section Canvas Sizes:
##' Predefined canvas sizes (see \code{\link{canvasSize}}):
##' \itemize{
##'   \item \code{"standard"}: 7.2 × 5.4 inches
##'   \item \code{"wide"}: 9.6 × 5.4 inches
##'   \item \code{"wide-screen"}: 18.6 × 9 inches
##'   \item \code{"square"}: 5.4 × 5.4 inches
##'   \item \code{"A4"}: 5.4 × 7.2 inches
##'   \item \code{"letter"}: 6.5 × 9 inches
##' }
##'
##' Custom sizes can be specified as \code{list(width = w, height = h)} where
##' \code{w} and \code{h} are in inches.
##'
##' @section Stamping:
##' When \code{script} is provided, a caption is added to the plot using
##' \code{\link{ggstamp}}. The stamp typically includes:
##' \itemize{
##'   \item Script file name
##'   \item Timestamp (unless \code{time = ""})
##'   \item Model identifier (if \code{model} is provided)
##' }
##'
##' The stamp is added as a ggplot2 caption and will appear at the bottom of the plot.
##'
##' @section Trace Attributes:
##' If \code{plot} has traceit attributes (added via \code{\link{traceit}}), those
##' attributes will be automatically extracted and used, unless explicitly overridden
##' by arguments passed to \code{ggwrite}.
##'
##' @return Returns \code{NULL} invisibly. Files are written as a side effect.
##'   If \code{show = TRUE}, plots are also displayed.
##'
##' @seealso
##' \code{\link{ggstamp}} for adding stamps to plots,
##' \code{\link{canvasSize}} for canvas size options,
##' \code{\link{writer}} for a unified interface to write various object types,
##' \code{\link{pathFormat}} for organizing output directories,
##' \code{\link{traceit}} for adding traceit attributes,
##' \code{\link[grDevices]{png}}, \code{\link[grDevices]{pdf}} for graphics devices
##'
##' @family Plotting
##' @import grDevices
##' @import grid
##' @import data.table
##' @import NMdata
##' @export
##'
##' @examples
##' library(ggplot2)
##' writeOutput <- FALSE
##' data(ChickWeight)
##' p1 <- ggplot(ChickWeight, aes(Time, weight, group = Chick, colour = factor(Diet))) +
##'   geom_line()
##'
##' # View plot on screen
##' ggwrite(p1)
##'
##' # Save plot with stamping
##' script <- "analysis.R"
##' ggwrite(p1, script = script, canvas = "wide",
##'         file = "myplot1.png", save = writeOutput)
##'
##' # Save in multiple formats
##' ggwrite(p1, file = "myplot1.png",
##'         formats = c("png", "pdf"),
##'         save = writeOutput)
##'
##' # Multiple canvas sizes
##' ggwrite(p1, file = "myplot1.png",
##'         canvas = list(small = "standard", large = "wide"),
##'         save = writeOutput)
##' # Creates: myplot1_small.png, myplot1_large.png
##'
##' # List of plots to separate files
##' p2 <- ggplot(ChickWeight, aes(Time, weight)) + geom_point()
##' plots <- list(lines = p1, points = p2)
##' ggwrite(plots, file = "chickweight.png",
##'         use.names = TRUE, save = writeOutput)
##' # Creates: chickweight_lines.png, chickweight_points.png
##'
##' # List of plots to single PDF
##' ggwrite(plots, file = "chickweight.pdf",
##'         onefile = TRUE, save = writeOutput)
##' # Creates: chickweight.pdf (2 pages)
##'
##' # Organize output using a %script-based directory structure
##' ggwrite(p1, file = "chickweight_lines.png",
##'         script = script, path.format = "%script/%name",
##'         save = writeOutput)
##' # Creates: analysis/chickweight_lines.png

ggwrite <- function(plot, file, canvas=NULL, formats,
                    onefile=FALSE, res=200, paper="special",
                    save=TRUE, show=!save, use.names=FALSE, subdir=NULL,
                    script, time, model,name.all.pdf,quiet=FALSE, path.format=NULL,method.png=c("ragg","png"),stamp.format=NULL,stamp.funs=NULL,stamp.gg.style=NULL,useNames,fun.path=NULL){

  #### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

if(FALSE) {
  ## name.all.pdf <- NULL
  name.canvas <- NULL
  . <- NULL
  size <- NULL
 }  

  ### Section end: Dummy variables, only not to get NOTE's in pacakge checks
  
  ## internally, the resolved path function is carried as fun.path
  fun.path <- argPathFormat(path.format=path.format, fun.path=fun.path)
  method.png <- match.arg(method.png)

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

  ### deprecated in 0.0.4
  if(!missing(useNames)){
    if(!missing(use.names)){
      stop("use.names and useNames supplied. Use use.names and not the deprecated useNames.")
    }
    message("useNames is deprecated. Use use.names.")
    use.names <- useNames
  }
  if(!missing(file) && (missing(formats)||is.null(formats))) formats <- fnExtension(file)

  ## Fall back to "standard" only when neither caller nor traceit attrs supplied a canvas
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

  if(is.null(file)) type <- "x11"

  if(missing(script)) script <- NULL

  ## If file is an empty string or null is the same.
  if(!missing(file)&&!is.null(file)){
    file2 <- gsub(" ","",file)
    if(!all(file==file2)) message("Blank characters in filename have been removed.")
    file <- file2
  }

  if(missing(name.all.pdf)) name.all.pdf <- NULL
  
  ### we want to allow "", especially for named lists
  ## if( !is.null(file) && length(file)==1 && file=="" ) {
  ##   file <- NULL
  ## }

  #### check inputs done

  if(save){
    ## Resolve all file names and canvas/format combinations
    ## allcombs <- ggwrite_names(file = file, formats = formats, canvas = canvas)

    ## Write each combination
    ## for (n in seq_len(nrow(allcombs))) {
    
    writeObj(
      plot      = plot,
      file      = file,
      script    = script,
      time      = time,
      model     = model,
      canvas    = canvas,
      onefile   = onefile,
      use.names = use.names,
      quiet     = quiet,
      res       = res,
      paper     = paper,
      formats   = formats,
      fun.path  = fun.path,
      name.all.pdf=name.all.pdf,
      subdir=subdir,
      method.png= method.png,
      stamp.format=stamp.format,
      stamp.funs=stamp.funs,
      stamp.gg.style=stamp.gg.style
    )

    
    ## }
  }

  if(show){
    writeObj(plot, file=NULL,  script=script, time=time,model=model, res=res, paper=paper,formats=NULL,canvas=NULL,fun.path=NULL,subdir=subdir,onefile=onefile,stamp.format=stamp.format,stamp.funs=stamp.funs,stamp.gg.style=stamp.gg.style)
  }
  invisible(NULL)
}


###### functions to be used internally

##' Print a plot or gtable object to the current device
##'
##' Internal helper that dispatches to the correct printing mechanism depending
##' on the class of \code{plot}. \code{gtable} objects (e.g. from
##' \code{\link[gridExtra]{grid.arrange}}) must be drawn with
##' \code{\link[grid]{grid.draw}}, while other objects (e.g. \code{ggplot}) use
##' the standard \code{print} method. \code{NULL} input is silently ignored.
##'
##' @param plot A plot object (\code{ggplot}, \code{gtable}, or similar), or \code{NULL}.
##'
##' @return \code{NULL}, invisibly. Called for the side effect of drawing to
##'   the current device.
##'
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

##' Write a plot or list of plots to one or more files
##'
##' Internal workhorse function called by \code{\link{ggwrite}}. Resolves the
##' combination of output formats and canvas sizes (via \code{\link{ggwrite_names}}),
##' and dispatches to \code{\link{write1}} for each individual plot/format/canvas
##' combination. Handles both single plot objects and lists of plots, including
##' the \code{onefile} (multi-page PDF) case.
##'
##' @param plot A plot object or a list of plot objects. See \code{\link{ggwrite}}.
##' @param file Character string, base output file path, or \code{NULL} to only
##'   display the plot(s) on screen.
##' @param script Character string or path to the calling script, used for stamping.
##'   See \code{\link{ggstamp}}.
##' @param time Character string or POSIXct timestamp used for stamping.
##' @param model Character string or model object used for stamping and path
##'   construction.
##' @param onefile Logical. If \code{TRUE} and \code{plot} is a list, all plots are
##'   combined into a single multi-page PDF.
##' @param use.names Logical. If \code{plot} is a list, should \code{names(plot)}
##'   be used in output file names rather than numeric indices?
##' @param formats Character vector of file formats, a subset of \code{c("png","pdf")}.
##' @param canvas Canvas size specification. See \code{\link{canvasSize}}.
##' @param quiet Logical. If \code{TRUE}, suppresses messages about files written.
##' @param fun.path Function or character string defining output directory structure.
##'   See \code{\link{pathFormat}}. If the path format uses \code{\%script}, \code{script}
##'   is forwarded to it automatically.
##' @param subdir Character string, subdirectory passed on to \code{fun.path}.
##' @param name.all.pdf Character string, file name to use for a combined multi-page
##'   PDF when \code{file = ""} and \code{onefile = TRUE}. Default \code{"all.pdf"}.
##' @param method.png Character string, one of \code{"png"} or \code{"ragg"},
##'   specifying which graphics device to use for PNG output. See \code{\link{write1}}.
##' @param ... Passed on to \code{\link{write1}} (and ultimately to
##'   \code{\link[grDevices]{png}}/\code{\link[grDevices]{pdf}}).
##'
##' @return \code{NULL}, invisibly. Files are written (and/or plots displayed) as
##'   a side effect.
##'
##' @seealso \code{\link{ggwrite}}, \code{\link{write1}}, \code{\link{ggwrite_names}}
##'
##' @keywords internal
## Don't export
writeObj <- function(plot,file,script,time,model=model,onefile,use.names=FALSE,formats,canvas,quiet=FALSE,fun.path,subdir,name.all.pdf="all.pdf",method.png=c("ragg","png"),...){
  
  name.file.canvas <- NULL
  size <- NULL

  ## get filname extension to determine device

  type <- "x11"
  fnroot <- NULL
  if(!is.null(file)){
    type <- "file"
    fnroot <- fnExtension(file,"")
  }


  if(type=="x11") onefile <- FALSE

  if(missing(formats)) formats <- NULL
  if(is.null(formats)) formats <- fnExtension(type)
  if(is.null(formats)) formats <- "png"

  method.png <- match.arg(method.png)

  allcombs <- ggwrite_names(file = file, formats = formats, canvas = canvas)
  if(is.null(subdir)) subdir <- ""
  ## allcombs[,subdir := var.subdir,env=list(var.subdir=subdir)]
  allcombs[,subdir := subdir]

  if(!is.null(file) && allcombs[,any(!format%in%c("png","pdf"))]){
    stop("Only extensions .png and .pdf are supported")
  }

  
  
  if(F){
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
      ## str.dots <- do.call(mypaste,dots)
      ## fn <- fnAppend(fn,str.dots,allow.noext=TRUE)
      fn <- fnAppend(fn,unlist(dots),allow.noext=TRUE)
    } 
    cleanFileNames(fn,allow.slash=TRUE)
  }
  ## fname.num <- function(fn,I){
  ##     paste(fnroot,"_",sprintf(fmt=paste("%0",Nplots.log10+1,"d",sep=""),I),".",type,sep="")

  ##     fnAppend(fn,names) |>
  ##         fnAppend(str.dots)
  ## }

  
  if(is.list(plot)&&!any(c("gg","gtable")%in%class(plot))) {
    ## if(onefile && type!="pdf"){
    ##     message("onefile can only be used with pdf device. Will not be used.")
    ##     onefile <- FALSE
    ## }

    if(onefile){

      ### extract all canvas names, then save all plots requested with each of the canvas names.
      ##all.canvas.names <- allcombs[,unique(name.file.canvas)]
      
      if(nrow(allcombs[format=="pdf"])){
        allcombs[format=="pdf",{
          ## run write1 on all plots. Each row in allcombs is for all plots, not one row per plot
          
          name <- NULL
          if(file=="") name <- name.all.pdf
          
          write1(plot=plot,
                 type="pdf",
                 fn=fname.char(fn=file,name=name,name.file.canvas),
                 size=list(height=height,width=width),
                 script=script,
                 time=time,
                 model=model,
                 quiet=quiet,
                 subdir=subdir,
                 fun.path=fun.path,
                 method.png=method.png,
                 ...)
        },
        by=row]
          allcombs <- allcombs[format!="pdf"]
        
      }
      if(nrow(allcombs)) {
        if(!quiet) message("onefile can only be used with pdf device. Will not be used.")
        onefile <- FALSE
      }
      ## silent <- lapplydt(allcombs,by="name.file.canvas",fun=function(x)write1(plot[x$row],
      ##                                                                         fn=fnAppend(file,unique(x$name.file.canvas)),
      ##                                                                         script=script,
      ##                                                                         time=time,
      ##                                                                         type="pdf",
      ##                                                                        size=list(width=unique(x$width),height=unique(x$height))))

    } 
    
    Nplots <- length(plot)
    ## debug
    ## cat("Number of plots: ",Nplots)

    ## fname.num <- function(fnroot,type,I) paste(fnroot,"_",sprintf(fmt=paste("%0",Nplots.log10+1,"d",sep=""),I),".",type,sep="")
    ## fname.char <- function(fnroot,type,name) paste(fnroot,"_",name,".",type,sep="")


    if (type=="x11"){
      
      write1(plot[[1]],type="x11")
      if(Nplots>1){
        silent <- lapply(2:Nplots,function(I){
          write1(plot=plot[[I]],type=type,size=size,subdir=subdir,
                 fun.path=fun.path,
                 script=script,time=time,model=model,
                 method.png=method.png,quiet=quiet)
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
          ## Resolve per-element traceit attributes; caller-level values take priority
          
          elem <- plot[[I]]
          elem.args  <- argsFromTrace(ggwrite, elem)
          elem.canvas <- if(!is.null(elem.args[["canvas"]])) elem.args[["canvas"]] else canvas
          elem.script <- if(!is.null(elem.args[["script"]])) elem.args[["script"]] else script
          elem.time   <- if(!is.null(elem.args[["time"]]))   elem.args[["time"]]   else time
          elem.model  <- if(!is.null(elem.args[["model"]]))  elem.args[["model"]]  else model
          elem.combs  <- ggwrite_names(file = file, formats = formats, canvas = elem.canvas)
          elem.combs[,write1(plot=elem,type=format,fn=fname.char(fn=file,name=names(plot)[I],name.file.canvas),size=list(height=height,width=width),
                             subdir=subdir,
                             fun.path=fun.path,                             script=elem.script,time=elem.time,model=elem.model,quiet=quiet,...),by=row]
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

    
    res <- lapplydt(allcombs,by="row",fun=function(x)
      with(x,write1(
        type=format[1],
        fn=fnAppend(fnExtension(file[1],format[1]),name.file.canvas,allow.noext=TRUE),
        size=list(width=width[1],height=height[1]),
        ##args that are not taken from allcombs, so "constant" 
        plot=plot,
        subdir=subdir,
        fun.path=fun.path,
        script=script,time=time,model=model,
        method.png=method.png,quiet=quiet,...
      )
      ))
    ## allcombs[,write1(
    ##     type=format[1],
    ##     fn=fnAppend(fnExtension(file[1],format[1]),name.file.canvas,allow.noext=TRUE),
    ##     size=list(width=width[1],height=height[1]),
    ##     ##args that are not taken from allcombs, so "constant" 
    ##     plot=plot,
    ##     script=script,time=time,model=model,quiet=quiet,...
    ## ),by=row
    ## ]
    
    
    ##write1(plot=plot,fn=file,type=type,size=size,script=script,time=time,...)
  }
  invisible(NULL)
}


## write1 <- function(x,...){
##     UseMethod(x,...)
## }



## make function to use for one plot. Then we will call tht on plot or loop
## it over the elements of plot in case plot is a list.

##' Write a single plot to a single file (or display it)
##'
##' Internal function that performs the actual device handling (opening a
##' \code{\link[grDevices]{png}}/\code{\link[ragg]{agg_png}} or
##' \code{\link[grDevices]{pdf}} device, printing the plot, and closing the
##' device) for a single plot/file/size combination. Also applies stamping via
##' \code{\link{ggstamp}} before printing. If \code{fn} or \code{type} indicate
##' on-screen display ("x11"), the plot is printed directly without opening a
##' file device.
##'
##' @param plot A single plot object (\code{ggplot}, \code{gtable}, or similar).
##' @param fn Character string, output file name (without or with extension;
##'   the extension is replaced/added based on \code{type}). \code{NULL} or
##'   \code{""} to only display on screen.
##' @param type Character string, one of \code{"png"}, \code{"pdf"}, or
##'   \code{"x11"} (display only, no file written).
##' @param onefile Logical, passed to the \code{pdf} device via \code{dots} when
##'   writing multiple pages to a single PDF file. Not directly used for the
##'   \code{png} device.
##' @param size A list with \code{width} and \code{height} elements (in inches),
##'   as produced by \code{\link{canvasSize}}/\code{\link{ggwrite_names}}.
##' @param script Character string or path to the calling script, used for
##'   stamping. See \code{\link{ggstamp}}.
##' @param time Character string or POSIXct timestamp used for stamping.
##' @param model Character string or model object used for stamping and path
##'   construction.
##' @param quiet Logical. If \code{TRUE}, suppresses the "Written to ..." message.
##' @param subdir Character string, subdirectory passed on to \code{fun.path}.
##' @param method.png Character string, one of \code{"png"} or \code{"ragg"}.
##'   Controls whether \code{\link[grDevices]{png}} or \code{\link[ragg]{agg_png}}
##'   is used as the PNG graphics device. \code{ragg} produces output that is
##'   more consistent across platforms.
##' @param fun.path Function or character string defining output directory
##'   structure, applied to \code{fn} before the file is written. See
##'   \code{\link{pathFormat}}. If the path format uses \code{\%script}, \code{script}
##'   is forwarded to it automatically (if the function accepts a \code{script}
##'   argument).
##' @param stamp.format,stamp.funs,stamp.gg.style Passed to \code{\link{ggstamp}}.
##' @param ... Additional arguments. \code{res} is passed on to the \code{png}/
##'   \code{ragg} device, \code{onefile} to the \code{pdf} device. A \code{dir}
##'   element (if present) is extracted but currently only used as a default
##'   working directory. A \code{fun.path} element (if present) overrides the
##'   \code{fun.path} argument.
##'
##' @return \code{NULL}, invisibly. Called for the side effect of writing a file
##'   and/or displaying the plot.
##'
##' @seealso \code{\link{ggwrite}}, \code{\link{writeObj}}, \code{\link{ggstamp}}
##'
##' @import ragg
##' @keywords internal
## Don't export
write1 <- function(plot,fn=NULL,type=NULL,onefile=FALSE,size,script,time,model,quiet=FALSE,subdir=NULL,method.png=c("ragg","png"),
                   fun.path=NULL,stamp.format=NULL,stamp.funs=NULL,stamp.gg.style=NULL,...){  
  
  ## print(str(size))
  if(is.null(plot)) {
    message("plot is NULL, nothing to do.")
    return(NULL)
  }
  if(!is.null(fn) && fn=="") fn <- NULL

  if(type=="") type <- NULL
  if(is.null(file)&&(is.null(type)||type=="")) type <- "x11"
  if(!is.null(fn)&&!is.null(type)&&type!="x11"){

    ## if(is.null(fn)) fn <- file
    fn <- fnExtension(fn,type)
  }
  if(!is.null(subdir) && subdir=="") subdir <- NULL
  method.png <- match.arg(method.png)
  
  plot <- ggstamp(plot,script=script,file=fn,time=time,model=model,size=size,
                  stamp.format=stamp.format,stamp.funs=stamp.funs,
                  stamp.gg.style=stamp.gg.style)

  dots <- try(list(...),silent=TRUE)
  if("try-error"%in%class(dots)) dots <- NULL

  dir <- NULL
  if("dir"%in%names(dots)) {
    dir <- dots[["dir"]]
    dots <- dots[setdiff(names(dots),"dir")]
  }
  if(is.null(dir)) dir <- "."

    ## if("fun.path"%in%names(dots) && !is.null(dots$fun.path)) {
  ##   fun.path <- dots$fun.path
  ##   dots$fun.path <- NULL
  ## }
  if(missing(fun.path)) fun.path <- NULL
  if(!is.null(type) && type!="x11" && !is.null(fun.path)){
    fn <- applyFunPath(fun.path,name=fn,model=model,script=script,subdir=subdir,quiet=quiet)
  }

  if(!is.null(fn)&&type!="x11"){
    dir.file <- dirname(fn)
    OutputDirCreate(dir.file)

    fn <- fnExtension(fn,type)
    switch(type,
           png={
             
             dots <- dots[intersect(names(dots),"res")]
             args <- c(list(filename = fn, width = size$width, 
                            height = size$height, units = "in"),dots)
             switch(method.png,
                    ragg=do.call(ragg::agg_png, args),
                    png=do.call(png, args)
                    )
             ## do.call(png,args)
             ## if (requireNamespace("ragg", quietly = TRUE)) {
             ##   do.call(ragg::agg_png, args)
             ## } else {
             ##   do.call(png, args)
             ## }

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
             do.call(pdf,args)             ## pdf(file = fn, width = size$width, 
             ##     height = size$height,onefile=onefile,...)
           })
    print1(plot)
    dev.off()
    if (!quiet) message(fn)
  } else {
    print1(plot)
  }
}



##' Resolve all output file names for ggwrite
##'
##' Internal function that generates all combinations of file formats and canvas sizes
##' for \code{\link{ggwrite}}. Returns a \code{data.table} with columns: format,
##' name.canvas, width, height, name.file.canvas, row.
##'
##' @param file Character string, base file name
##' @param formats Character vector of file formats ("png", "pdf")
##' @param canvas Canvas size specification (see \code{\link{canvasSize}})
##'
##' @return A \code{data.table} with one row per format-canvas combination
##'
##' @import data.table
##' @keywords internal
ggwrite_names <- function(file, formats, canvas) {

  name.canvas <- NULL
  name.file.canvas <- NULL

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

  allcombs[,name.file.canvas := ""]
  if (n.canvas > 1) {
    allcombs[,name.file.canvas := name.canvas]
  }

  allcombs[,row := .I]

  allcombs
}
