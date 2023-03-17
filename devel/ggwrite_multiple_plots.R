library(devtools)
load_all("~/wdirs/tracee")
library(data.table)

formats <- c("png","pdf")

canvas <- list("standard",square=list(height=8,width=8))

## formats
## canvas

## allcombs <- egdt(data.table(format=formats),
##      data.table(canvas=canvas))


## allcombs
## allcombs[4,canvas][[1]] |>
##     canvasSize()

## canvas
## canvasSize(canvas)

is.chars <- sapply(canvas,is.character)
## if more than one canvas is given, lists must be named
if(
    length(canvas)>1&&is.null(names(canvas)) && any(!is.chars) ){
    stop("If more than one canvas is requested, non-character elements must be named.")
}
## character elements do not need to be named. If they are not, we use the canvas name
nms <- names(canvas)
## get rid of special characters
nms <- gsub(" ","",nms)
nms <- gsub("[[:punct:]]","",nms)
nms[is.chars&nms==""] <- do.call(c,canvas[is.chars&nms==""])
names(canvas) <- nms
## check that names are unique
if(any(duplicated(nms))) stop("canvas names must be unique")
canvas



dt.canvas <- do.call(rbind,
                  lapply(canvasSize(canvas,simplify=FALSE),as.data.table)
                  )
dt.canvas[,name.canvas:=names(canvas)]
dt.canvas

allcombs <- egdt(data.table(format=formats),
                 dt.canvas)
allcombs


## do a for loop through the rows of allcombs
p
