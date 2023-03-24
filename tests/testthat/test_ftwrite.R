## library(data.table)
## library(devtools)
## load_all(export_all=FALSE)

context("ftwrite")

test_that("general use",{

    ## library(data.table)
    ## .datatable.aware = TRUE
    fileRes <- "testOutput/ftwrite1.png"
    
    data(mtcars)


    ## mtc <- as.data.table(mtcars)
    mtc <- mtcars
    
    ft <- flextable(mtc)

    ft <- autofit(ft)

    ## sudo apt install r-cran-webshot
    ## webshot::install_phantomjs()

    time <- as.POSIXct("2022-02-01 07:09:21",tz="UTC")

    ftwrite(ft,file=fileRes,script="test_ftwrite.R",formats=c(".png",".pptx",".html",".docx"),time=time)

    local_edition(3)
    expect_snapshot_file(fileRes)
})

test_that("unsupported format",{

    data(mtcars)

    ft <- flextable(mtcars)

    expect_error(
        ftwrite(ft,file="testOutput/ftwrite2.jpg",script="test_ftwrite.R",time=time)
    )

})

test_that("without script arg",{

    fileRes <- "testOutput/ftwrite3.png"
    ## fileRef <- "testReference/flextab3.png"
    data(mtcars)

    ft <- flextable(mtcars)

    ## ftwrite(ft,file=fileRes,formats=c(".png"),time="")
    ftwrite(ft,file=fileRes,formats=c(".png"))

    local_edition(3)
    expect_snapshot_file(fileRes)
    
})

