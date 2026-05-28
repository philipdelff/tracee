## write roxygen documentation 

#### names aren't right. Use a better function than is.list to find lists

## lists.as.subdirs

lwrite <- function(list,model,dir=".",structure,subdir=NULL,lists.as.subdirs=FALSE,...){

  if(missing(model)) model <- NULL
  
  if(is.list(model)) {
    model <- model$label
  }
  
  ### how about if fun.path is a string, like "model/model-file"?
  ### Shouldn't we run pathStruct on it?
  if(missing(structure)) structure <- NULL

  fun.path <- pathStruct(structure=structure,dir=dir,subdir=subdir)

  dir.model <- fun.path("dummy",model=model,subdir=subdir) |> dirname()
  if(!file.exists(dir.model)) dir.create(dir.model)
  
  if(!dir.exists(dir)) stop("dir must exist")
  

  ## make sure list2 is a list of lists
  nolist <- list[!sapply(list,is.listnotplot)]
  list2 <- c(list[sapply(list,is.listnotplot)],
             list(main=nolist))
  list2 <- list2[sapply(list2,function(x)length(x) > 0)]

  lwrite1 <- function(x,name,model,...){
    
    if(any(
      sapply(x,is.listnotplot)
    )) stop("too deep listing")
    
    ## Collect plots in one list. When should this be done?
    plots <- x[sapply(x,is.gg)]
    notplots <- x[!sapply(x,is.gg)]
    
    
    ## xlist <- c(notplots,plots=list(plots))
    ## file.out <- fun.path(name=name,dir=dir,model=model)
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
                     file=name,
                     model=model,
                     fun.path=fun.path,
                     ...)
      res <- append(res,res1)
    }
    
    ## dots <- list(...)

    if(length(notplots)) {
      subdir <- NULL
      if(lists.as.subdirs) {
        subdir <- name
      } else {
        names(notplots) <- paste0(name,"_",names(notplots))
      }

      res1 <- mapply(FUN=writer,
                     notplots,
                     ## file=fnAppend(file.out,names(notplots),allow.noext=TRUE),
                     ## file=fnAppend(name,names(notplots),allow.noext = TRUE),
                     ## file = sapply(names(notplots),filePathSimple(dir,subdir,x)),
                     file = names(notplots),
                     ## file=name,

                     ## name=names(notplots),
                     MoreArgs=list(
                       model=model,
                       fun.path=fun.path,
                       subdir=subdir,
                       ...))
      res <- append(res,res1)
    }

    return(invisible(res))
  }

  
   
  if(lists.as.subdirs){
    res <- mapply(lwrite1,list2,name=names(list2),
                  MoreArgs=list(model=model,
                                lists.as.subdirs=lists.as.subdirs,
                                ...),
                  SIMPLIFY=FALSE)
  } else {  
    res <- mapply(lwrite1,list2,name=names(list2),
                  MoreArgs=list(model=model,
                                lists.as.subdirs=lists.as.subdirs,
                                ...),
                  SIMPLIFY=FALSE)
  }


  invisible(res)

}



