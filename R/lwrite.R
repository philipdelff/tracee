
lwrite <- function(list,model,dir=".",structure,...){
    
    if(missing(model)) model <- NULL

    if(is.list(model)) {
        model <- model$label
    }

    ### how about if fun.path is a string, like "model/model-file"?
    ### Shouldn't we run pathStruct on it?
    if(missing(structure)) structure <- NULL

    ## if( !is.null(model) ){
    ##     fun.path <- pathStruct(structure=structure)
    ##     dir.model <- fun.path("dummy",dir,model) |> dirname() 
    ## } else {
    ##     fun.path(name,dir,model) file.path(dir,name)
    ## } 

    fun.path <- pathStruct(structure=structure)

    dir.model <- fun.path("dummy",dir,model) |> dirname()
    if(!file.exists(dir.model)) dir.create(dir.model)
    
    ## Collect plots in one list. When should this be done?
    plots <- list[sapply(list,is.gg)]
    notplots <- list[!sapply(list,is.gg)]
    list <- c(notplots,plots=list(plots))

    

    names.out <- names(list)
    res <- lapply(1:length(list),function(n){
        file.out <- fun.path(name=names.out[n],dir=dir,model=model)
        writer(x=list[[n]],
               file=file.out,
               model=model,
               ...)
    })
    return(invisible(res))
}
