
##' @export
traceit <- function(x,...){
    UseMethod("traceit")
}

##' @method traceit ggplot
##' @export
traceit.ggplot <- function(x,...,quite=TRUE){


    ## add rags as attributes
    dots <- list(...)
    args <- dots[names(dots)%in%names(formals(ggwrite))]

    setattr(x,"args",args)
    ## add traceit class
    setattr(x,"class",c("trace",class(x)))

    invisible(x)
}

##' @method traceit flextable
##' @export
traceit.flextable <- function(x,...,quite=TRUE){

    ## add rags as attributes
    dots <- list(...)
    args <- dots[names(dots)%in%names(formals(ftwrite))]

    setattr(x,"args",args)
    ## add traceit class
    setattr(x,"class",c("trace",class(x)))

    invisible(x)
}

##' @import NMdata
##' @method traceit data.frame
##' @export
traceit.data.frame <- function(x,...,quite=TRUE){

    ## add rags as attributes
    dots <- list(...)
    args <- dots[names(dots)%in%names(formals(NMwriteData))]

    setattr(x,"args",args)
    ## add traceit class
    setattr(x,"class",c("trace",class(x)))

    invisible(x)
}

##' @method traceit trace
##' @export
traceit.trace <- function(x,...,quite=TRUE){

    ## add rags as attributes
    dots <- list(...)
    args <- dots[names(dots)%in%names(formals(NMwriteData))]

    args.old <- attr(x,"args")
    args <- modifyList(args.old,args)

    setattr(x,"args",args)

    invisible(x)
}


