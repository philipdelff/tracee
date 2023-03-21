##' Stamp and write flextab objects to one or multiple formats
##' @param ft A flextab object.
##' @param file to save to. See formats to generate multiple files.
##' @param script path to script - will be pasted as caption.
##' @param formats One or more of png, docx, pptx, html. As a
##'     character vector.
##' @param save Save the table to the given file or just show?
##'     Defaults to TRUE. Hint, if you use an "exportFlag", use
##'     save=exportFlag.
##' @param quiet Default is false but use TRUE to suppress messages
##'     about what was saved.
##' @param ... Arguments passed to stampFlextab.
##' @import flextable
##' @importFrom NMdata fnExtension
##' @export

ftwrite <- function(ft,file,script,formats,save,quiet=FALSE,...){

    ## save_as_docx
    ## save_as_html
    ## save_as_image
    ## save_as_pptx
    ## rds

    ##all.files <- fnExtension(file,formats)

    if(missing(save)||is.null(save)) save <- TRUE
    if(!save) return(ft)

    if(missing(formats)||is.null(formats)) {
        ## formats <- sub(".*\\.(.+)$","\\1",file)
        formats <- fnExtension(file)
    }

    if(missing(script)) script <- NULL


    ## Write all requested formats  
    silent <- lapply(formats,function(ext){
        fn <- fnExtension(file,ext)
        fun.write <- switch(sub("\\.","",ext),
                            png=save_as_image
                           ,html=save_as_html
                           ,docx=save_as_docx
                           ,pptx=save_as_pptx,
                            stop("format not supported. See ?writeFlextab"))
        
        ft <- ftstamp(ft=ft,file=fn,script=script,...)
        fun.write(ft,path=fn)
        if(!quiet&&!is.null(fn)) message("Written to ",fn)
        fn
    })
    
    

    invisible(silent)
    

}