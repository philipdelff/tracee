#' @describeIn ftwrite Deprecated function name. Use ftwrite.
#' @export

writeFlextab <- function(ft,file,script,formats,save,quiet=FALSE,...){
    .Deprecated(new="ftwrite")
    ## pass by name - argument order differs from ftwrite()
    args <- list(ft=ft,quiet=quiet,...)
    if(!missing(file)) args$file <- file
    if(!missing(script)) args$script <- script
    if(!missing(formats)) args$formats <- formats
    if(!missing(save)) args$save <- save
    do.call(ftwrite,args)
}
