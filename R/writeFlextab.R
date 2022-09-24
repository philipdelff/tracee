
##' @param formats One or more of png, docx, pptx, rds
##' @importFrom NMdata fnExtension
##' @export 

writeFlextab <- function(ft,file,script,formats,bg=NULL){

    ## save_as_docx
    ## save_as_html
    ## save_as_image
    ## save_as_pptx
    ## rds

    ##all.files <- fnExtension(file,formats)
    
    silent <- lapply(formats,function(ext){
        fn <- fnExtension(file,ext)
        fun.write <- switch(sub("\\.","",ext),
                            png=save_as_image
                           ,html=save_as_html
                           ,docx=as_as_docx
                           ,pptx=save_as_pptx)
        
        ft <- stampFlextab(ft=ft,file=fn,script=script,bg)
        fun.write(ft,path=fn)
        fn
    })
    
    

    invisible(fn)
    

}
