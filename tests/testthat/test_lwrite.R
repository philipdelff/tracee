script <- "test_lwrite.R"

test_that("general ft",{

    mtc <- mtcars
    
    ft <- flextable(mtc) |>
        autofit()

    ## sudo apt install r-cran-webshot
    ## webshot::install_phantoms()


    
    time <- as.POSIXct("2022-02-01 07:09:21",tz="UTC")

    ## writer(ft,file=fileRes,script=script,formats.ft=c(".png",".pptx"),time=time)

    ## expect_snapshot_file(fileRes)

#### plots
    p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
        geom_point()


    lsave <- list(a_dataset=mtc,
                  a_ft=ft,
                  a_plot=p1)

    lwrite(lsave,dir="testOutput",model="model1",script=script,time="model1")
    
})

lwrite(lsave,dir="testOutput",model="model1",script=script,time="model1")
lwrite(lsave,dir="testOutput",model="model1",script=script,time="model1label",canvas=list(mycanvas=list(height=9,width=21)))

names(formals(NMwriteData))
names(formals(ggwrite))
names(formals(ftwrite))


class(p1)
class(ft)
class(mtc)
