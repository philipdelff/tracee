
##' @param formats One or more of png, docx, pptx, rds
##' @export 
writeFlextab <- function(ft,file,script,formats,bg=NULL){

    ft <- stampFlextab(ft,script=script,file=file,bg=bg)

    save_as_image(ft,file)
}
