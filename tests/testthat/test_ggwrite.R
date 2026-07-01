if(F){
library(ggplot2)
library(testthat)
library(withr)
}

# Use testthat edition 3 for snapshot testing
options(testthat.edition = 3)

## found.files <- list.files(path="testOutput",pattern="ggwrite.+\\..+",full.names=TRUE)
## if(length(found.files)) unlink(found.files)

p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
    geom_point()


stamp <- "test_ggwrite.R"

test_that("single plot, single format",{
    fileRes <- "testOutput/ggwrite_01.png"

    ggwrite(p1,script=stamp,file=fileRes,save=TRUE,time="test")
    expect_snapshot_file(fileRes)
})

test_that("single plot, multiple formats",{
    file <- "testOutput/ggwrite_02.png"
    ggwrite(p1,script=stamp,file=file,formats=c("png","pdf"),save=TRUE,time="test")
    expect_snapshot_file("testOutput/ggwrite_02.png")
    ##expect_snapshot_file("testOutput/ggwrite_02.pdf")
    expect_true(file.exists("testOutput/ggwrite_02.pdf"))
})

test_that("single plot, multiple formats and canvases",{
    file <- "testOutput/ggwrite_03.png"
    ggwrite(p1,script=stamp,file=file,formats=c("png","pdf"),
            canvas=c("standard","wide-screen"),time="test")
    expect_snapshot_file("testOutput/ggwrite_03_standard.png")
    ## expect_snapshot_file("testOutput/ggwrite_03_standard.pdf")
    expect_true(file.exists("testOutput/ggwrite_03_standard.pdf"))
    expect_snapshot_file("testOutput/ggwrite_03_widescreen.png")
    ## expect_snapshot_file("testOutput/ggwrite_03_widescreen.pdf")
    expect_true(file.exists("testOutput/ggwrite_03_widescreen.pdf"))
})

test_that("traceit settings",{
    p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
        geom_point()

    class(p1)

    ggwrite(p1,file="testOutput/ggwrite_04.png",canvas="wide")
    expect_snapshot_file("testOutput/ggwrite_04.png")

    traceit(p1,canvas=list(namecanv=list(height=4,width=12)))
    
    ## traceit(p1,canvas="wide") # This line is commented in original, keeping it that way
## class(p1)
    ggwrite(p1,file="testOutput/ggwrite_05.png")
    expect_snapshot_file("testOutput/ggwrite_05.png")

    ## ggwrite arguments override traceit arguments
    ggwrite(p1,file="testOutput/ggwrite_06.png",canvas="wide")
    expect_snapshot_file("testOutput/ggwrite_06.png")
})

test_that("multiple plots in list",{
    fileRes <- "testOutput/ggwrite_list_01.png"
    stamp <- "test_ggwrite.R"
    
    p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
        geom_point()+
        labs(title="plot 1")

    p2 <- p1 +
        labs(title="plot 2")

    plots <- list("plot 1"=p1,
                  "plot 2"=p2)

    ggwrite(plots,script=stamp,file=fileRes,save=TRUE,time="test")

    expect_snapshot_file("testOutput/ggwrite_list_01_1.png")
    expect_snapshot_file("testOutput/ggwrite_list_01_2.png")
})

test_that("multiple plots in list to multiple devices",{
    fileRes <- "testOutput/ggwrite_list_02.png"
    stamp <- "test_ggwrite.R"
    
    p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
        geom_point()+
        labs(title="plot 1")

    p2 <- p1 +
        labs(title="plot 2")

    plots <- list("plot 1"=p1,
                  "plot 2"=p2)

    ggwrite(plots,script=stamp,file=fileRes,save=TRUE,time="test",canvas=c("standard","wide"),use.names=TRUE)

    expect_snapshot_file("testOutput/ggwrite_list_02_plot1_standard.png")
    expect_snapshot_file("testOutput/ggwrite_list_02_plot1_wide.png")
    expect_snapshot_file("testOutput/ggwrite_list_02_plot2_standard.png")
    expect_snapshot_file("testOutput/ggwrite_list_02_plot2_wide.png")
})

test_that("multiple plots in list to pdf with multiple canvases (onefile=TRUE should error)",{
    fileRes <- "testOutput/ggwrite_list_03_onefile.pdf"
    stamp <- "test_ggwrite.R"
    
    p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
        geom_point()+
        labs(title="plot 1")

    p2 <- p1 +
        labs(title="plot 2")

    plots <- list("plot 1"=p1,
                  "plot 2"=p2)

    ggwrite(plots,script=stamp,file=fileRes,save=TRUE,time="test",canvas=c("standard","wide"),use.names=TRUE,onefile=TRUE)

    ## expect_snapshot_file("testOutput/ggwrite_list_03_onefile_standard.pdf")
    ## expect_snapshot_file("testOutput/ggwrite_list_03_onefile_wide.pdf")
    expect_true(file.exists(fnAppend(fileRes,"standard")))
    expect_true(file.exists(fnAppend(fileRes,"wide")))

})


test_that("use.names with only one plot",{
    fileRes <- "testOutput/ggwrite_07.png"
    stamp <- "test_ggwrite.R"
    
    p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
        geom_point()+
        labs(title="plot 1")


    plots <- list("plot 1"=p1)

    ggwrite(plots,script=stamp,file=fileRes,save=TRUE,use.names=TRUE)

    expect_true(file.exists(fnAppend(fileRes,"plot1")))

    ## expect_snapshot_file("testOutput/ggwrite_list_03_onefile_standard.pdf")
    ## expect_snapshot_file("testOutput/ggwrite_list_03_onefile_wide.pdf")
    ## expect_true(file.exists(fnAppend(fileRes,"standard")))
    ## expect_true(file.exists(fnAppend(fileRes,"wide")))

})


######## what happens if a list of lists is passed

######## what happens if a list of traceit lists is passed

