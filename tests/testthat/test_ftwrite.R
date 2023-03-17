## library(data.table)
## library(devtools)
## load_all(export_all=FALSE)

context("ftwrite")

test_that("general use",{

    ## library(data.table)
    ## .datatable.aware = TRUE

    
    data(mtcars)


    ## mtc <- as.data.table(mtcars)
    mtc <- mtcars
    
    ft <- flextable(mtc)

    ft <- autofit(ft)
    ## ft

    ## fn <- "a_file.png"
    ## ft2 <- stampFlextab(ft,file=fn,script="testscript.R")

    ## sudo apt install r-cran-webshot
    ## webshot::install_phantomjs()

    ## save_as_image(ft2,path="testOutput/flextab1.png")

    time <- as.POSIXct("2022-02-01 07:09:21",tz="UTC")
    ftwrite(ft,file="testOutput/flextab1.png",script="test_ftwrite.R",formats=".png",time=time)

    ftwrite(ft,file="testOutput/flextab2.png",script="test_ftwrite.R",formats=c(".png",".pptx",".html",".docx"),time=time)

    local_edition(3)
    expect_snapshot_file("testOutput/flextab2.png","testReference/flextab2.png")
})

test_that("unsupported format",{

    data(mtcars)

    ft <- flextable(mtcars)

    expect_error(
        ftwrite(ft,file="testOutput/flextab3.jpg",script="test_ftwrite.R",time=time)
    )

})

test_that("without script arg",{

    fileRes <- "testOutput/flextab3.png"
    fileRef <- "testReference/flextab3.png"
    data(mtcars)

    ft <- flextable(mtcars)

    ftwrite(ft,file=fileRes,formats=c(".png"))

    local_edition(3)
    expect_snapshot_file(fileRes,fileRef)
    
})

