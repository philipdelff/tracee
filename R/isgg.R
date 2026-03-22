##' @import flextable
##' @import ggplot2

is.gg <- function(x){
    inherits(x,"ggplot") ||
        inherits(x,"gtable") 
}

##' @import flextable
##' @import ggplot2
is.listnotplot <- function(x) {
    !is.gg(x) && 
        !is.data.frame(x) &&
         !inherits(x,"flextable") &&
         is.list(x)
}
