##' Check if an object is a ggplot or gtable
##'
##' This function tests whether an object is a ggplot2 plot object or a gtable
##' (grid table) object.
##'
##' @param x An object to test
##'
##' @return Logical. TRUE if x inherits from "ggplot" or "gtable", FALSE otherwise.
##'
##' @import flextable
##' @import ggplot2
##'
##' @examples
##' \dontrun{
##' library(ggplot2)
##' p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
##' is.gg(p)  # TRUE
##' is.gg(mtcars)  # FALSE
##' }
##'
##' @export
is.gg <- function(x){
    inherits(x,"ggplot") ||
        inherits(x,"gtable") 
}

##' Check if an object is a list but not a plot, data frame, or flextable
##'
##' This function tests whether an object is a list that is NOT a ggplot/gtable,
##' data frame, or flextable object. This is useful for identifying nested list
##' structures that contain multiple outputs.
##'
##' @param x An object to test
##'
##' @return Logical. TRUE if x is a list but not a ggplot, gtable, data.frame,
##'   or flextable object; FALSE otherwise.
##'
##' @import flextable
##' @import ggplot2
##'
##' @examples
##' \dontrun{
##' library(ggplot2)
##' p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
##' is.listnotplot(p)  # FALSE (it's a ggplot)
##' is.listnotplot(mtcars)  # FALSE (it's a data.frame)
##' is.listnotplot(list(a = 1, b = 2))  # TRUE (it's a plain list)
##' }
##'
##' @export
is.listnotplot <- function(x) {
    !is.gg(x) && 
        !is.data.frame(x) &&
         !inherits(x,"flextable") &&
         is.list(x)
}
