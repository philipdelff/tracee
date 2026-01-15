##' One interface for ggwrite and writeFlextab
##' @param x
##' @param file
##' @param save
##' @param show
##' @param formats.ft
##' @param formats.gg
##' @param formats.data passed to NMwriteData() as format.write if file is a data set.
##' @param script
##' @param canvas
##' @param ...
##' @import tracee
##' @importFrom NMdata NMwriteData
##' @importFrom utils modifyList

## this should be exported from tracee, not from NMgof/NMauto

## add ,args.ggwrite and and args.flextable
writer <- function(x,file,formats.ft,formats.gg,formats.data,script=NULL,model=NULL,...){

    if(is.list(model)) {
        model <- model$label
    }

    if(missing(formats.ft)||is.null(formats.ft)){
        formats.ft <- "png"
    }

    if(missing(formats.gg)||is.null(formats.gg)){
        formats.gg <- "png"
    }

    if(missing(formats.data)||is.null(formats.data)){
        formats.data <- "rds"
    }

    dots <- list(...)
    
    if("flextable" %in% class(x)) {
        dots <- dots[names(dots)%in%names(formals(ftwrite))]
        args <- c(list(ft=x,
                       file = file, 
                       script=script,
                       formats=formats.ft),
                  dots)
        res <- do.call(ftwrite,args)

        ## res <- ftwrite(x,
        ##                file=file,
        ##                script=script,
        ##                formats=formats.ft,
        ##                ##time=model,
        ##                bg="#ffffff",
        ##                ...)
    } else if(is.data.frame(x)) {
        dots <- dots[names(dots)%in%names(formals(NMwriteData))]
        args <- c(list(data=x,
                       file = file, 
                       formats=formats.data,
                       script=script,
                       genText=FALSE,
                       args.stamp=list(model=model)
                       ),
                  dots)
        if("time"%in%names(args)){
            args$args.stamp$time <- args$time
            args$time <- NULL
        }
        res <- do.call(NMwriteData,args)

        ## res <- NMwriteData(x,
        ##                    file=file,
        ##                    formats.write=formats.data,
        ##                    script=script,
        ##                    genText=FALSE,
        ##                    args.stamp=list(model=model))
    } else { 
        if(is.list(x) && !is.ggplot(x) && ! "gtable"%in%class(x) ){
            x <- x[!sapply(x,is.null)]
        }
        ## message("Calling ggwrite()")
        dots <- dots[names(dots)%in%names(formals(ggwrite))]
        args.gg <- modifyList(
                              list(
                                  onefile=TRUE,
                                  canvas="standard"
                                  ),
            dots)

        args.ggwrite <- modifyList(
            list(plot=x,
                 file=file,
                 script=script,
                 ## onefile=TRUE,
                 ## canvas=canvas,
                 ## time=model,
                 use.names=TRUE,
                 formats=formats.gg),
            args.gg
        )

        res <- try(do.call(ggwrite,args.ggwrite))

        ## res <- try(ggwrite(x,
        ##                    file=file,
        ##                    script=script,
        ##                    onefile=TRUE,
        ##                    canvas=canvas,
        ##                    ## time=model,
        ##                    use.names=TRUE,
        ##                    formats=formats.gg))
        ## if("try-error"%in%class(res)) browser()

    }
    invisible(res)
}
