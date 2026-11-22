
if(F){
  unloadNamespace("tracee")
  unloadNamespace("NMdata")
  load_all("~/wdirs/NMdata")
  load_all("~/wdirs/tracee")

  library(ggplot2)
  library(testthat)
  library(withr)
}

# Use testthat edition 3 for snapshot testing
options(testthat.edition = 3)

source("compare_image_tolerance.R")

## found.files <- list.files(path="testOutput",pattern="ggwrite.+\\..+",full.names=TRUE)
## if(length(found.files)) unlink(found.files)

p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
  geom_point()


stamp <- "test_ggwrite.R"

test_that("single plot, single format",{
  skip_on_os(c("windows", "mac"))
  skip_if_not_installed("ragg")

  fileRes <- "testOutput/ggwrite_01.png"

  ggwrite(p1,script=stamp,file=fileRes,save=TRUE,time="test",method.png="png")

  expect_snapshot_file(fileRes,
                       compare=compare_image_tolerance)
})

test_that("single plot, multiple formats",{
  skip_on_os(c("windows", "mac"))
  skip_if_not_installed("ragg")

  file <- "testOutput/ggwrite_02.png"
  ggwrite(p1,script=stamp,file=file,formats=c("png","pdf"),save=TRUE,time="test",method.png="png")
  expect_snapshot_file("testOutput/ggwrite_02.png",
                       compare=compare_image_tolerance)
  ##expect_snapshot_file("testOutput/ggwrite_02.pdf")
  expect_true(file.exists("testOutput/ggwrite_02.pdf"))
})

test_that("single plot, multiple formats and canvases",{
  skip_on_os(c("windows", "mac"))
  skip_if_not_installed("ragg")

  file <- "testOutput/ggwrite_03.png"
  ggwrite(p1,script=stamp,file=file,formats=c("png","pdf"),
          canvas=c("standard","wide-screen"),time="test",method.png="png")
  expect_snapshot_file("testOutput/ggwrite_03_standard.png",
                       compare=compare_image_tolerance)
  ## expect_snapshot_file("testOutput/ggwrite_03_standard.pdf")
  expect_true(file.exists("testOutput/ggwrite_03_standard.pdf"))
  expect_snapshot_file("testOutput/ggwrite_03_widescreen.png",
                       compare=compare_image_tolerance)
  ## expect_snapshot_file("testOutput/ggwrite_03_widescreen.pdf")
  expect_true(file.exists("testOutput/ggwrite_03_widescreen.pdf"))
})

test_that("traceit settings",{
  skip_on_os(c("windows", "mac"))
  skip_if_not_installed("ragg")

  p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
    geom_point()

  class(p1)

  ggwrite(p1,file="testOutput/ggwrite_04.png",canvas="wide",method.png="png")
  expect_snapshot_file("testOutput/ggwrite_04.png",
                       compare=compare_image_tolerance)

  traceit(p1,canvas=list(namecanv=list(height=4,width=12)))
  
  ## traceit(p1,canvas="wide") # This line is commented in original, keeping it that way
  ## class(p1)
  ggwrite(p1,file="testOutput/ggwrite_05.png",method.png="png")
  expect_snapshot_file("testOutput/ggwrite_05.png",
                       compare=compare_image_tolerance)

  ## ggwrite arguments override traceit arguments
  ggwrite(p1,file="testOutput/ggwrite_06.png",canvas="wide",method.png="png")
  expect_snapshot_file("testOutput/ggwrite_06.png",
                       compare=compare_image_tolerance)
})

test_that("multiple plots in list",{
  skip_on_os(c("windows", "mac"))
  skip_if_not_installed("ragg")

  fileRes <- "testOutput/ggwrite_list_01.png"
  stamp <- "test_ggwrite.R"
  
  p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
    geom_point()+
    labs(title="plot 1")

  p2 <- p1 +
    labs(title="plot 2")

  plots <- list("plot 1"=p1,
                "plot 2"=p2)

  ggwrite(plots,script=stamp,file=fileRes,save=TRUE,time="test",method.png="png")

  expect_snapshot_file("testOutput/ggwrite_list_01_1.png",
                       compare=compare_image_tolerance)
  expect_snapshot_file("testOutput/ggwrite_list_01_2.png",
                       compare=compare_image_tolerance)
})

test_that("multiple plots in list to multiple devices",{
  skip_on_os(c("windows", "mac"))
  skip_if_not_installed("ragg")

  fileRes <- "testOutput/ggwrite_list_02.png"
  stamp <- "test_ggwrite.R"
  
  p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
    geom_point()+
    labs(title="plot 1")

  p2 <- p1 +
    labs(title="plot 2")

  plots <- list("plot 1"=p1,
                "plot 2"=p2)

  ggwrite(plots,script=stamp,file=fileRes,save=TRUE,time="test",canvas=c("standard","wide"),use.names=TRUE,method.png="png")

  expect_snapshot_file("testOutput/ggwrite_list_02_plot1_standard.png",
                       compare=compare_image_tolerance)
  expect_snapshot_file("testOutput/ggwrite_list_02_plot1_wide.png",
                       compare=compare_image_tolerance)
  expect_snapshot_file("testOutput/ggwrite_list_02_plot2_standard.png",
                       compare=compare_image_tolerance)
  expect_snapshot_file("testOutput/ggwrite_list_02_plot2_wide.png",
                       compare=compare_image_tolerance)
})

test_that("multiple plots in list to pdf with multiple canvases (onefile=TRUE should error)",{
  skip_on_os(c("windows", "mac"))
  skip_if_not_installed("ragg")

  fileRes <- "testOutput/ggwrite_list_03_onefile.pdf"
  stamp <- "test_ggwrite.R"
  
  p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
    geom_point()+
    labs(title="plot 1")

  p2 <- p1 +
    labs(title="plot 2")

  plots <- list("plot 1"=p1,
                "plot 2"=p2)

  ggwrite(plots,script=stamp,file=fileRes,save=TRUE,time="test",canvas=c("standard","wide"),use.names=TRUE,onefile=TRUE,method.png="png")

  ## expect_snapshot_file("testOutput/ggwrite_list_03_onefile_standard.pdf")
  ## expect_snapshot_file("testOutput/ggwrite_list_03_onefile_wide.pdf")
  expect_true(file.exists(fnAppend(fileRes,"standard")))
  expect_true(file.exists(fnAppend(fileRes,"wide")))

})


test_that("use.names with only one plot",{
  skip_on_os(c("windows", "mac"))
  skip_if_not_installed("ragg")
  
  fileRes <- "testOutput/ggwrite_07.png"
  stamp <- "test_ggwrite.R"
  
  p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
    geom_point()+
    labs(title="plot 1")


  plots <- list("plot 1"=p1)

  ggwrite(plots,script=stamp,file=fileRes,save=TRUE,use.names=TRUE,method.png="png")

  expect_true(file.exists(fnAppend(fileRes,"plot1")))

  ## expect_snapshot_file("testOutput/ggwrite_list_03_onefile_standard.pdf")
  ## expect_snapshot_file("testOutput/ggwrite_list_03_onefile_wide.pdf")
  ## expect_true(file.exists(fnAppend(fileRes,"standard")))
  ## expect_true(file.exists(fnAppend(fileRes,"wide")))

})


test_that("plots in list, unnamed file",{  
  skip_on_os(c("windows", "mac"))
  skip_if_not_installed("ragg")
  
  fileRes <- "testOutput/.png"
  stamp <- "test_ggwrite.R"
  
  p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
    geom_point()+
    labs(title="plot 1")

  p2 <- p1 +
    labs(title="plot 2")

  plots <- list("plot 1"=p1,
                "plot 2"=p2)

  ggwrite(plots,script=stamp,file=fileRes,save=TRUE,time="test",use.names=TRUE,method.png="png")

  expect_snapshot_file("testOutput/plot1.png",
                       compare=compare_image_tolerance)
  expect_snapshot_file("testOutput/plot2.png",
                       compare=compare_image_tolerance)
})


### playing around with stamp format and styline
if(FALSE) {

  fileRes <- "testOutput/rightjust_stamp.png"
  p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+
    geom_point()+
    labs(title="plot 1")


  
  ## ggwrite(p1,script=stamp,file=fileRes,save=TRUE,use.names=TRUE,method.png="png",stamp.gg.style=list(hjust=0))
  ggwrite(p1,file=fileRes,script=stamp,model="models/model1.lst",save=TRUE,use.names=TRUE,method.png="png",stamp.gg.style=list(hjust=0),
          stamp.format="Output:%file\nSource: %script\nModel: %model\nTime: %time")

  ggwrite(p1,file=fileRes,script=stamp,model="models/model1.lst",save=TRUE,use.names=TRUE,method.png="png",stamp.gg.style=list(hjust=0),
          stamp.format="%file\n%script\n%model\n%time",
          stamp.funs=list(time=function(x)paste("Time:",x),
                          file=function(x)paste("Output:",x),
                          script=function(x)paste("Script:",x),
                          model=function(x)paste("Model:",x)
                          ))

  ggwrite(p1,file=fileRes,script=stamp,model="models/model1.lst",save=TRUE,use.names=TRUE,method.png="png",stamp.gg.style=list(hjust=0),
          ## stamp.format="%file\n%script\n%model\n%time",
          ## stamp.funs=list(time=function(x)paste("Time:",x),
          ##                 file=function(x)paste("Output:",x),
          ##                 script=function(x)paste("Script:",x),
          ##                 model=function(x)paste("Model:",x)
          ## )
          )

  
  ggwrite(p1,file=fileRes,script=stamp,model="models/model1.lst",save=TRUE,use.names=TRUE,method.png="png",stamp.gg.style=list(hjust=0),
          ## stamp.format="%file\n%script\n%model\n%time",
          ## stamp.funs=list(time=function(x)paste("Time:",x),
          ##                 file=function(x)paste("Output:",x),
          ##                 script=function(x)paste("Script:",x),
          ##                 model=function(x)paste("Model:",x)
          ## )
          )
ggwrite(p1,file=fileRes,script=stamp,model="models/model1.lst",save=TRUE,use.names=TRUE,method.png="png",stamp.gg.style=list(hjust=0,position="left")
          )

ggwrite(p1,script=stamp,model="models/model1.lst",save=TRUE,use.names=TRUE,method.png="png",stamp.gg.style=list(hjust=0))


}
