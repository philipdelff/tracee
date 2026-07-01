untrace <- function(x){

  setattr(x,"args",NULL)
  setattr(x,"class",setdiff(class(x),"trace"))
  invisible(x)

}
