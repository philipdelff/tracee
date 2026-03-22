## write roxygen documentation 

#### names aren't right. Use a better function than is.list to find lists

## lists.as.subdirs

lwrite <- function(list,model,dir=".",structure,...){
    
    if(missing(model)) model <- NULL

    if(is.list(model)) {
        model <- model$label
    }

    ### how about if fun.path is a string, like "model/model-file"?
    ### Shouldn't we run pathStruct on it?
    if(missing(structure)) structure <- NULL


    fun.path <- pathStruct(structure=structure)

    dir.model <- fun.path("dummy",dir,model) |> dirname()
    if(!file.exists(dir.model)) dir.create(dir.model)

    

    ## make sure list2 is a list of lists
    nolist <- list[!sapply(list,is.listnotplot)]
    list2 <- c(list[sapply(list,is.listnotplot)],
               list(main=nolist))
    list2 <- list2[sapply(list2,function(x)length(x) > 0)]

    lwrite1 <- function(x,name){
        
        if(any(
            sapply(x,is.listnotplot)
        )) stop("too deep listing")
        
        ## Collect plots in one list. When should this be done?
        plots <- x[sapply(x,is.gg)]
        notplots <- x[!sapply(x,is.gg)]
        
        xlist <- c(notplots,plots=list(plots))
        file.out <- fun.path(name=name,dir=dir,model=model)
        ## res <- lapply(1:length(xlist),function(n){
        ##     file.out <- fun.path(name=name,dir=dir,model=model)
        ##     writer(x=xlist[[n]],
        ##            file=file.out,
        ##            model=model,
        ##            ...)
        ## })
        res <- list()
        if(length(plots)) {
            res1 <- writer(x=plots,
                   file=file.out,
                   model=model,
                   ...)
            res <- append(res,res1)
        }

        if(length(notplots)) {
            res1 <- mapply(writer,
                           notplots,
                           file=fnAppend(file.out,names(notplots),allow.noext=TRUE),
                           MoreArgs=list(
                               model=model,
                               ...))
            res <- append(res,res1)
        }

        return(invisible(res))
    }

    
    res <- mapply(lwrite1,list2,name=names(list2),SIMPLIFY=FALSE)

    invisible(res)

}


