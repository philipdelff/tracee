##' Determine the class type of an R object for writer dispatch
##'
##' Internal function that determines the appropriate class type of an object
##' for dispatching to the correct writer function. This function classifies
##' objects into one of several categories: "flextable", "data.frame", "gg"
##' (for ggplot/gtable objects), "list", or throws an error for unknown types.
##'
##' @param x An R object to classify. Can be a \code{flextable}, \code{data.frame},
##'   \code{ggplot}, \code{gtable}, \code{list}, or object with "traceit" class.
##'
##' @details
##' The function checks object class in the following priority order:
##' \enumerate{
##'   \item \code{flextable}: checked via \code{"flextable" \%in\% class(x)}
##'   \item \code{data.frame}: checked via \code{is.data.frame(x)}
##'   \item \code{gg}: checked via \code{\link{is.gg}(x)} (includes ggplot and gtable)
##'   \item \code{list}: checked via \code{is.list(x) && !is.gg(x)}
##'   \item If none match: throws an error
##' }
##'
##' This function is primarily used internally by \code{\link{writer}} and
##' \code{\link{traceit.traceit}} to determine which specific writer function
##' to call (ggwrite, ftwrite, or datwrite).
##'
##' The function ignores the "traceit" class that may be prepended by
##' \code{\link{traceit}}, allowing it to classify traced objects by their
##' underlying type.
##'
##' @return A character string indicating the object class type:
##' \itemize{
##'   \item \code{"flextable"}: for flextable objects
##'   \item \code{"data.frame"}: for data frame objects
##'   \item \code{"gg"}: for ggplot or gtable objects
##'   \item \code{"list"}: for list objects (that are not plots)
##' }
##'
##' @seealso
##' \code{\link{writer}} for the main writer function that uses this classification,
##' \code{\link{traceit.traceit}} for traceit method that uses this,
##' \code{\link{is.gg}} for plot object detection,
##' \code{\link{is.listnotplot}} for list detection logic
##'
##' @keywords internal
##'
##' @examples
##' \dontrun{
##' library(ggplot2)
##' library(flextable)
##'
##' # Classify different object types
##' trclass(mtcars)  # Returns: "data.frame"
##'
##' p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
##' trclass(p1)  # Returns: "gg"
##'
##' ft1 <- flextable(head(mtcars))
##' trclass(ft1)  # Returns: "flextable"
##'
##' trclass(list(p1, ft1))  # Returns: "list"
##'
##' # Works with traced objects too
##' p1_traced <- traceit(p1, canvas = "wide")
##' trclass(p1_traced)  # Still returns: "gg"
##'
##' # Unknown types throw an error
##' trclass(matrix(1:4, 2))  # Error: x is an unknown format
##' }
trclass <- function(x){
  
  class_x <- "unknown"
  if("flextable" %in% class(x)) {
    class_x <- "flextable"
  }
  if( class_x == "unknown"&&is.data.frame(x)){
    class_x <- "data.frame"
  }
  if( class_x == "unknown"&& is.gg(x)){
    class_x <- "gg"
  }
  if( class_x == "unknown"&&is.list(x) && !is.gg(x)){
    class_x <- "list"
  }
  if( class_x == "unknown"){
    stop("x is an unknown format",paste(class(x),collapse=", "))
  }

  class_x
}
