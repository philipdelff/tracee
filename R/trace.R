trace.gg <- function(x,...,quite=TRUE){


    ## add rags as attributes
    dots <- list(...)
    args <- dots[names(dots)%in%names(formals(ggwrite))]

    setattr(x,"args",args)
    ## add trace class
    setattr(x,"class",c("trace",class(x)))

    x
}

trace.flextable <- function(x,...,quite=TRUE){

    ## add rags as attributes
    dots <- list(...)
    args <- dots[names(dots)%in%names(formals(ftwrite))]

    setattr(x,"args",args)
    ## add trace class
    setattr(x,"class",c("trace",class(x)))

    x
}

##' @import NMdata
trace.data.frame <- function(x,...,quite=TRUE){

    ## add rags as attributes
    dots <- list(...)
    args <- dots[names(dots)%in%names(formals(NMwriteData))]

    setattr(x,"args",args)
    ## add trace class
    setattr(x,"class",c("trace",class(x)))

    x
}

trace.trace <- function(x,...,quite=TRUE){

    ## add rags as attributes
    dots <- list(...)
    args <- dots[names(dots)%in%names(formals(NMwriteData))]

    args.old <- attr(x,"args")
    args <- modifyList(args.old,args)

    setattr(x,"args",args)

    x
}


