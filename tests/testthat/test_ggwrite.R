context("ggwrite")


if(F){

### These are ready to be turned into tests

    p1 <- ggplot(data.table(TIME=0,DV=1),aes(TIME,DV))+geom_point()
    ggwrite(p1)  ## view plot on screen
    stamp <- "note"
}


test_that("Basic",{
    
    p1 <- ggplot(data.table(TIME=0,DV=1),aes(TIME,DV))+geom_point()
    ## ggwrite(p1)  ## view plot on screen
    stamp <- "note"

    fileRes <- "testOutput/myplot1.png"
    fileRef <- "testReference/myplot1.png"
    ggwrite(p1,script=stamp,file=fileRes,save=TRUE,time="test")

    local_edition(3)
    expect_snapshot_file(fileRes)

})

if(F){
    ggwrite(p1,script=stamp,file="testOutput/myplot2.png",formats=cc(png,pdf),save=TRUE,time="test")

    ggwrite(p1,script=stamp,file="testOutput/myplot3.png",formats=cc(png,pdf),
            canvas=c("standard","wide-screen"),time="test")

}

p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
    geom_point()

class(p1)

ggwrite(p1,file="testOutput/ggwrite_03.png",canvas="wide")

traceit(p1,canvas=list(namecanv=list(height=4,width=12)))
## traceit(p1,canvas="wide")

ggwrite(p1,file="testOutput/ggwrite_04.png")

ggwrite(p1,file="testOutput/ggwrite_05.png",canvas="wide")

