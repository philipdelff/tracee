##' Stamp and write flextab objects to one or multiple formats
##' @param ft A flextab object.
##' @param File to save to. See formats to generate multiple files.
##' @param formats One or more of png, docx, pptx, rds
##' @param bg The background color. Passed to stampFlextab.
##' @import flextable
##' @export 

writeFlextab <- function(ft,file,script,formats,...){

    ## save_as_docx
    ## save_as_html
    ## save_as_image
    ## save_as_pptx
    ## rds

    ##all.files <- fnExtension(file,formats)
    
    if(missing(formats)||is.null(formats)) formats <- sub(".*\\.(.+)$","\\1",file)

    silent <- lapply(formats,function(ext){
        fn <- fnExtensionTracee(file,ext)
        fun.write <- switch(sub("\\.","",ext),
                            png=save_as_image
                           ,html=save_as_html
                           ,docx=save_as_docx
                           ,pptx=save_as_pptx)
        
        ft <- stampFlextab(ft=ft,file=fn,script=script,...)
        fun.write(ft,path=fn)
        fn
    })
    
    

    invisible(silent)
    

}
