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
