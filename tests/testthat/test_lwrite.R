 ## library(devtools)
## unloadNamespace("tracee")
## unloadNamespace("NMdata")
## load_all("~/wdirs/NMdata")
## load_all("~/wdirs/tracee")


## sudo apt install r-cran-webshot
## webshot::install_phantoms()

emptyDir <- function(dir,create=TRUE){
  unlink(dir,recursive = TRUE)
  if(create) dir.create(dir)
}

script <- "test_lwrite.R"

if(!dir.exists("testOutput")) dir.create("testOutput")

#### plots
p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
  geom_point()

p2 <- p1
p2 <- traceit(p2,canvas="wide")

mtc1 <- mtcars

ft1 <- flextable(mtc1) |>
  autofit()


## class(p1)
## class(ft1)
## class(mtc)

time1 <- as.POSIXct("2022-02-01 07:09:21",tz="UTC")


lsave.flat <- list(
  a_dataset=mtc1,
  a_ft=ft1,
  a_plot=p1)


test_that("general ft, data, and gg",{

  dir.out <- "testOutput/model1"
  ## emptyDir(dir.out,create=FALSE)
  unlink(dir.out,recursive = TRUE)

  lsave <- lsave.flat
  names(lsave)  
  ###


  lwrite(lsave,dir="testOutput",model="model1",script=script,time=time1)

  ## test subdir
  ## *** TODO ggplot output names - remove "main"
  list.files(dir.out)  
})

test_that("pass arg - custom canvas",{

  dir.out <- "testOutput/model1_args"
  unlink(dir.out,recursive = TRUE)
  dir.create(dir.out)
  
  lsave <- lsave.flat
  ## lwrite(lsave,dir="testOutput",model="model1",script=script,time=time1)
  lwrite(lsave,dir=dir.out,model="model1",script=script,time=time1,canvas=list(mycanvas=list(height=9,width=21)))


  
})



## names(formals(NMwriteData))
## names(formals(ggwrite))
## names(formals(ftwrite))



test_that("general ft, data, and two gg",{

  #### plots
  lsave <- list(a_dataset=mtc1,
                t2a_ft=ft1,
                t2plot1=p1,
                t2plot2=p2)

  lwrite(lsave,dir="testOutput/test2",model="model1",script=script,time=time1)
  list.files("testOutput/test2/model1")
  lwrite(lsave,dir="testOutput",model="model1",script=script,time=time1,onefile=TRUE,formats.gg="pdf")
  list.files("testOutput/model1")
  
})


test_that("sublist",{

  lsave <- list(
    ## a_dataset=mtc1,
    ##           a_ft=ft1,
    ##           a_plot=p1,
    alist=list(plot2=p1,
               ft2=ft1)
  )

  dir.out <- "testOutput/lwrite_02" 
  unlink(dir.out,recursive = T)
  dir.create(dir.out,showWarnings = FALSE)

  ## png and flat structure - no alist subdir
  lwrite(lsave,dir=dir.out,model="model1",script=script,time=time1)

  ## ft output does not contain "alist"
  list.files(dir.out,recursive = T)
})

test_that("sublist no subdir - pdf",{

  lsave <- list(
    ## a_dataset=mtc1,
    ##           a_ft=ft1,
    a_plot=p1,
    alist=list(plot2=p1,
               ft2=ft1)
  )

  dir.out <- "testOutput/lwrite_03"
  unlink(dir.out,recursive = T)
  dir.create(dir.out,showWarnings = FALSE)

  #### pdf and flat structure - no alist subdir
  lwrite(lsave,dir=dir.out,model="model1",script=script,time=time1,formats.gg="pdf")

  ## alist is not in flextable name.
  list.files(dir.out,recursive = T)

  ###

})

test_that("sublist with subdir - png",{
  dir.out <- "testOutput/lwrite_04"
  emptyDir(dir.out,create=TRUE)

  lsave <- list(
    ## a_dataset=mtc1,
    a_ft=ft1,
    ##  a_plot=p1,
    alist=list(plot2=p1,
               ft2=ft1)
  )

  ## getting  testOutput/lwrite_04/model1/alist/1_model1.png - should be ft2 instead of 1.
  unlink(file.path(dir.out,"model1"),recursive = T)
  lwrite(lsave,dir=dir.out,model="model1",script=script,time=time1,formats.gg="png",lists.as.subdirs = T)
  lwrite(lsave,dir=dir.out,model="model1",script=script,time=time1,formats.gg="png",lists.as.subdirs = F)
  list.files(file.path(dir.out,"model1"))
  list.files(file.path(dir.out,"model1","alist"))
  
})

test_that("lwrite with traceit to multiple canvases",{
p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
  geom_point()

class(p1)
attr(p1,"args")

  p2 <- p1
  p2 <- traceit(p2,canvas=c("wide","standard"))
  attr(p2,"args")
class(p1)
    attr(p1,"args")



lsave.flat2 <- list(
  a_ft=ft1,
  a_tplot=p2)

  lwrite(lsave.flat2,dir="testOutput",model="model2",script=script,time=time1)

## todo: test that both wide and standard were saved

})


