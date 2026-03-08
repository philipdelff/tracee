## library(data.table)
## library(devtools)
## load_all(export_all=FALSE)
## library(testthat)

if(F){
myprint <- function(ft){
    mytmpfile <- path.expand("~/tmp/myhtml.html")
    save_as_html(ft,path=mytmpfile)
    browseURL(mytmpfile)
}
}

time <- as.POSIXct("2022-02-01 07:09:21",tz="UTC")

test_that("general use",{

    ## library(data.table)
    ## .datatable.aware = TRUE
    fileRes <- "testOutput/ftwrite_01.png"
    
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
        ftwrite(ft,file="testOutput/ftwrite_02.jpg",script="test_ftwrite.R",time=time)
    )

})

test_that("without script arg",{

    fileRes <- "testOutput/ftwrite_03.png"

    data(mtcars)

    ft <- flextable(mtcars)

    ## ftwrite(ft,file=fileRes,formats=c(".png"),time="")
    ftwrite(ft,file=fileRes,formats=c(".png"))

    local_edition(3)
    expect_snapshot_file(fileRes)
    
})


test_that("",{

    fileRes <- "testOutput/ftwrite_04.png"
    data(mtcars)

    ft <- flextable(mtcars)
    ## ft <- set_caption(ft,"a first caption line")
    ft <- add_footer_lines(ft, "a first caption line")

    ftwrite(ft,file=fileRes,script="test_ftwrite.R",time=time)
    expect_snapshot_file(fileRes)    

})
