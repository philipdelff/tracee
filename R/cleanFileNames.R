##' Drop spaces and odd characters. Use to ensure generated file names
##' are usable.
##' @param x a string to clean
##' @return A character vector
##' @keywords internal
## don't export - belongs in NMdata


cleanFileNames <- function(x,allow.slash=FALSE){


    x <- gsub("[ +!?#:;<>&,\\{\\}\\|=\\(\\)]", "",x) 
    if(!allow.slash){
        x <- gsub("/", "",x) 
    }
    x <- gsub(pattern="-",replacement="",x=x,perl=TRUE) 
    x <- gsub(pattern="\n",replacement="",x=x)
    
    x
}
