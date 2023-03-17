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

writeFlextab <- function(ft,file,script,formats,save,quiet=FALSE,...){
    .Deprecated(new="ftwrite")
    ftwrite(ft,file,script,formats,save,quiet=FALSE,...)
}
