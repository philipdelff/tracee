
if(F){
    ## This was used to check stamp with output file name. 
    library(devtools)
    load_all("c:/Users/delff/working_copies/pmxtricks")
    library(ggplot2)
    getwd()
    data(pksim1,package="pmxtricks")
    p1 <- ggplot(pksim1,aes(TIME,DV,colour=ID))+geom_point()
    ggwrite(p1)  ## view plot on screen
    stamp <- "note"
    ggwrite(p1,stamp=stamp,canvas="wide",file="myplot1.png",save=TRUE)
}
