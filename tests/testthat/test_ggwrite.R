context("ggwrite")


if(F){
    fileRes <- "testOutput/myplot1.png"
    stamp <- "test_ggwrite.R"
    
    p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
        geom_point()

### These are ready to be turned into tests
    ggwrite(p1,script=stamp,file=fileRes,save=TRUE,time="test")

    local_edition(3)
    expect_snapshot_file(fileRes)

})


if(F){

    ggwrite(p1,script=stamp,file="testOutput/myplot2.png",formats=cc(png,pdf),save=TRUE,time="test")

    ggwrite(p1,script=stamp,file="testOutput/myplot3.png",formats=cc(png,pdf),
            canvas=c("standard","wide-screen"),time="test")

}


test_that("traceit settings",{
    p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
        geom_point()

    class(p1)

    ggwrite(p1,file="testOutput/ggwrite_03.png",canvas="wide")

    traceit(p1,canvas=list(namecanv=list(height=4,width=12)))
    ## traceit(p1,canvas="wide")

    ggwrite(p1,file="testOutput/ggwrite_04.png")

    ## ggwrite arguments override traceit arguments
    ggwrite(p1,file="testOutput/ggwrite_05.png",canvas="wide")

})

####### NOT WORKING  
test_that("multiple plots in list",{
    fileRes <- "testOutput/myplot_list.png"
    stamp <- "test_ggwrite.R"
    
    p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
        geom_point()+
        labs(title="plot 1")

    p2 <- p1 +
        labs(title="plot 2")

    plots <- list("plot 1"=p1,
                  "plot 2"=p2)

### These are ready to be turned into tests
    ggwrite(plots,script=stamp,file=fileRes,save=TRUE,time="test")

    local_edition(3)
    expect_snapshot_file(fileRes)

})
