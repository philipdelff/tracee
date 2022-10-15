## library(data.table)
## library(devtools)
## load_all(export_all=FALSE)

context("writeFlextab")

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
    writeFlextab(ft,file="testOutput/flextab1.png",script="test_writeFlextab.R",formats=".png",time=time)

    writeFlextab(ft,file="testOutput/flextab2.png",script="test_writeFlextab.R",formats=c(".png",".pptx",".html",".docx"),time=time)

      local_edition(3)
    expect_snapshot_file("testOutput/flextab2.png","testReference/flextab2.png")
})

