#' @describeIn ftwrite Deprecated function name. Use ftwrite.

writeFlextab <- function(ft,file,script,formats,save,quiet=FALSE,...){
    .Deprecated(new="ftwrite")
    ftwrite(ft,file,script,formats,save,quiet=FALSE,...)
}
