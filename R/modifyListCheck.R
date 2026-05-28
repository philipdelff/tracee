##' Check if list element names are allowed
##'
##' Internal function to validate that all names in a list are within a set of
##' allowed element names. Throws an error if any disallowed names are found.
##'
##' @param x A named list to check.
##' @param elems.allowed Character vector of allowed element names. If NULL, 
##'   all names are allowed and the function returns x unchanged.
##' @param name.x Character string used in error messages to identify the 
##'   object being checked. Default is "x".
##'
##' @return Returns x unchanged if all names are allowed.
##'
##' @details
##' This function is used internally to validate argument lists before passing
##' them to other functions. It helps catch typos or invalid arguments early.
##'
##' @keywords internal
checkNames <- function(x,elems.allowed=NULL,name.x="x"){
    if(is.null(elems.allowed)) return(x)

    newnames <- setdiff(names(x),elems.allowed)
    if(length(newnames)) {
        stop(paste0("elements not allowed in",name.x,": ", paste(newnames,collapse=", ")))
    }
    x
}

##' Modify a list with validation of allowed elements
##'
##' Internal function that combines two lists using \code{\link[utils]{modifyList}}
##' while validating that all element names are within a set of allowed names.
##'
##' @param x A named list with default values.
##' @param y A named list with values to override those in x. Can be NULL.
##' @param elems.allowed Character vector of allowed element names for both x and y.
##'
##' @return If y is NULL, returns x (after validation). Otherwise returns the 
##'   result of modifyList(x, y) after validating both lists.
##'
##' @details
##' This function first validates that x contains only allowed element names.
##' If y is not NULL, it merges y into x using \code{\link[utils]{modifyList}},
##' then validates that the resulting list also contains only allowed names.
##' This ensures that both default arguments and user-provided overrides are valid.
##'
##' @seealso \code{\link{checkNames}}, \code{\link[utils]{modifyList}}
##'
##' @keywords internal
modifyListCheck <- function(x,y,elems.allowed){
    checkNames(x,elems.allowed=elems.allowed)
    if(is.null(y)){
        return(x)
    } else {
        y <- modifyList(x,y)
        checkNames(x=y,elems.allowed=elems.allowed,name.x="y")
    }

    y
}
