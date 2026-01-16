context("writer")

time <- as.POSIXct("2022-02-01 07:09:21",tz="UTC")

local_edition(3)
script <- "test_writer.R"

test_that("general ft",{

    fileRes <- "testOutput/writer_01.png"
    
#### flextable
    data(mtcars)

    mtc <- mtcars
    
    ft <- flextable(mtc)
    ft <- autofit(ft)

    ## sudo apt install r-cran-webshot
    ## webshot::install_phantomjs()


    time <- as.POSIXct("2022-02-01 07:09:21",tz="UTC")

    writer(ft,file=fileRes,script=script,formats.ft=c(".png",".pptx"),time=time)

    expect_snapshot_file(fileRes)

#### plots
    p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
        geom_point()

    writer(p1,file=fnAppend(fileRes,"plot"),script=script,formats.gg=c("png","pdf"),time=time)

#### data.frame

    writer(mtc,file=fnAppend(fileRes,"data"),script=script,
           formats.gg=c("png","pdf"),formats.data=c("rds","csv")
          ,time=time)
    res1 <- readRDS("testOutput/writer_01_data.rds")
    res1
    NMinfo(res1)

})
