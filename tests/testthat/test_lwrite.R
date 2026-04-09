script <- "test_lwrite.R"

if(!dir.exists("testOutput")) dir.create("testOutput")

test_that("general ft, data, and gg",{

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


    lsave <- list(
        ## a_dataset=mtc,
        a_ft=ft,
        a_plot=p1)
    
### dir is not being used

    lwrite(lsave,dir="testOutput",model="model1",script=script,time="time1")

## test subdir
   
})

## lwrite(lsave,dir="testOutput",model="model1",script=script,time="model1")
lwrite(lsave,dir="testOutput",model="model1",script=script,time="model1label",canvas=list(mycanvas=list(height=9,width=21)))

names(formals(NMwriteData))
names(formals(ggwrite))
names(formals(ftwrite))


class(p1)
class(ft)
class(mtc)

library(devtools)
unloadNamespace("tracee")
unloadNamespace("NMdata")
load_all("~/wdirs/NMdata")
load_all("~/wdirs/tracee")


test_that("general ft, data, and gg",{

    mtc <- mtcars
    
    ft <- flextable(mtc) |>
        autofit()

    time <- as.POSIXct("2022-02-01 07:09:21",tz="UTC")


#### plots
    p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
        geom_point()

    p2 <- p1
    traceit(p2,canvas="wide")
    lsave <- list(a_dataset=mtc,
                  t2a_ft=ft,
                  t2plot1=p1,
                  t2plot2=p2)

    lwrite(lsave,dir="testOutput/test2",model="model1",script=script,time="time1")
    list.files("testOutput/test2/model1")
    lwrite(lsave,dir="testOutput",model="model1",script=script,time="time1",onefile=TRUE,formats.gg="pdf")
    list.files("testOutput/model1")
    
})


test_that("sublist",{

    lsave <- list(
        ## a_dataset=mtc,
        ##           a_ft=ft,
        ##           a_plot=p1,
                  alist=list(plot2=p1,
                             ft2=ft)
                  )

    dir.out <- "testOutput/lwrite_02" 
    unlink(dir.out,recursive = T)
    dir.create(dir.out,showWarnings = FALSE)

## png and flat structure - no alist subdir
    lwrite(lsave,dir=dir.out,model="model1",script=script,time="model1")

    ## ft output does not contain "alist"
    list.files(dir.out,recursive = T)
})

test_that("sublist - pdf",{

    lsave <- list(
        ## a_dataset=mtc,
        ##           a_ft=ft,
        a_plot=p1,
                  alist=list(plot2=p1,
                             ft2=ft)
                  )

    dir.out <- "testOutput/lwrite_03"
    unlink(dir.out,recursive = T)
    dir.create(dir.out,showWarnings = FALSE)

## pdf and flat structure - no alist subdir
    lwrite(lsave,dir=dir.out,model="model1",script=script,time="model1",formats.gg="pdf")

## alist is not in flextable name.
    list.files(dir.out,recursive = T)
})



test_that("",{

})
