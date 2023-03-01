##' Stamp and write flextab objects to one or multiple formats
##' @param ft A flextab object.
##' @param file to save to. See formats to generate multiple files.
##' @param script path to script - will be pasted as caption.
##' @param formats One or more of png, docx, pptx, html. As a
##'     character vector.
##' @param save Save the table to the given file or just show?
##'     Defaults to TRUE. Hint, if you use an "exportFlag", use
##'     save=exportFlag.
##' @param ... Arguments passed to stampFlextab.
##' @import flextable
##' @importFrom NMdata fnExtension
##' @export

writeFlextab <- function(ft,file,script,formats,save,...){

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



    ##### currently, we let lapply throw errors. It would be more
    ##### elegant to summarize all the formats that weren't supported.
### formats <- intersect(formats,cc(png,html,docx,pptx))
    ## if(length(formats)==0) {
    ##     warning("No supported formats requested. Nothing done.")
    ##     return(invisible(NULL))
    ## }


    ## if(!length(formats))
    
    silent <- lapply(formats,function(ext){
        fn <- fnExtension(file,ext)
        fun.write <- switch(sub("\\.","",ext),
                            png=save_as_image
                           ,html=save_as_html
                           ,docx=save_as_docx
                           ,pptx=save_as_pptx,
                            stop("format not supported. See ?writeFlextab"))
        
        ft <- stampFlextab(ft=ft,file=fn,script=script,...)
        fun.write(ft,path=fn)
        fn
    })
    
    

    invisible(silent)
    

}
