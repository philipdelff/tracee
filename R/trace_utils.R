untrace <- function(x){

    setattr(x,"class",setdiff(class(x),"trace"))
    x

}
