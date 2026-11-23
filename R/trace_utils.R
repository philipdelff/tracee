##' Remove traceit information from an object
##'
##' Removes the \code{args} attribute and the \code{"traceit"} class
##' added by \code{\link{traceit}}. The object is modified in place.
##' @param x An object
##' @return x without traceit information (invisibly)
##' @keywords internal
untraceit <- function(x){

  setattr(x,"args",NULL)
  setattr(x,"class",setdiff(class(x),"traceit"))
  invisible(x)

}
