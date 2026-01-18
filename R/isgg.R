is.gg <- function(x){
    inherits(x,"ggplot") ||
        inherits(x,"gtable") 
}
