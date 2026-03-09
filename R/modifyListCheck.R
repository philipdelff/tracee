##' @keywords internal
checkNames <- function(x,elems.allowed=NULL,name.x="x"){
    if(is.null(elems.allowed)) return(x)

    newnames <- setdiff(names(x),elems.allowed)
    if(length(newnames)) {
        stop(paste0("elements not allowed in",name.x,": ", paste(newnames,collapse=", ")))
    }
    x
}

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
