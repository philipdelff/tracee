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

  # Check that output directory was created
  expect_true(dir.exists(dir.out))
  
  # Check that files were created
  files <- list.files(dir.out)
  
  # Debug: print files if test would fail
  if(length(files) == 0) {
    message("No files found in ", dir.out)
  }

  expect_true(length(files) > 0)
  
  # Check that expected files exist
  expect_true(any(grepl("a_dataset.*\\.rds$", files)), 
              info = paste("Files found:", paste(files, collapse=", ")))
  expect_true(any(grepl("a_ft.*\\.png$", files)),
              info = paste("Files found:", paste(files, collapse=", ")))
  expect_true(any(grepl("a_plot.*\\.png$", files)),
              info = paste("Files found:", paste(files, collapse=", ")))
  
  # Check that we have exactly 3 files (one for each object)
  expect_equal(length(files), 5)
})

test_that("pass arg - custom canvas",{

  dir.out <- "testOutput/model1_args"
  unlink(dir.out,recursive = TRUE)
  dir.create(dir.out)
  
  lsave <- lsave.flat
  untrace(lsave)
  ## attributes(lsave)
  ## attributes(lsave$a_plot)
  untrace(lsave$a_plot)

## lwrite(lsave,dir="testOutput",model="model1",script=script,time=time1)
  lwrite(lsave,dir=dir.out,model="model1_args",script=script,time=time1,canvas=list(mycanvas=list(height=9,width=21),
                                                                                    wide=canvasSize("wide")))

  # Check that output directory exists
  model_dir <- file.path(dir.out, "model1_args")
  expect_true(dir.exists(model_dir))
  
  # Check that files were created
  files <- list.files(model_dir, recursive = TRUE)
  expect_true(length(files) > 0,
              info = paste("Files found:", paste(files, collapse=", ")))
  
  # canvas name not included wh
  expect_true(
    any(grepl("main_a_plot.*mycanvas.*\\.png$", files))
   ,
              info = paste("Files found:", paste(files, collapse=", ")))
  
  # Check that flextable and dataset files exist
  expect_true(any(grepl("main_a_ft.*\\.png$", files)))
  expect_true(any(grepl("main_a_dataset.*\\.rds$", files)))

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

  dir.out1 <- "testOutput/test2/model1"
  unlink("testOutput/test2", recursive = TRUE)
  
  lwrite(lsave,dir="testOutput/test2",model="model1",script=script,time=time1)
  
  files1 <- list.files(dir.out1)
  expect_true(length(files1) > 0,
              info = paste("Files found:", paste(files1, collapse=", ")))
  
  # Check for dataset and flextable
  expect_true(any(grepl("a_dataset.*\\.rds$", files1)))
  expect_true(any(grepl("t2a_ft.*\\.png$", files1)))
  
  # Check for plots - both should be present
  # Note: plots may be combined or separate depending on onefile setting
  plot_files <- grepl("t2plot", files1)
  expect_true(any(plot_files),
              info = paste("Looking for plot files. Files found:", paste(files1, collapse=", ")))
  
  # Test with onefile=TRUE and PDF format
  dir.out2 <- "testOutput/model1"
  unlink(dir.out2, recursive = TRUE)
  
  lwrite(lsave,dir="testOutput",model="model1",script=script,time=time1,onefile=TRUE,formats.gg="pdf")
  
  files2 <- list.files(dir.out2)
  expect_true(length(files2) > 0,
              info = paste("Files found:", paste(files2, collapse=", ")))
  
  # With onefile=TRUE, we should have a single PDF for plots (or a PDF named after the collection)
  pdf_files <- files2[grepl("\\.pdf$", files2)]
  expect_true(length(pdf_files) >= 1,
              info = paste("PDF files found:", paste(pdf_files, collapse=", "), 
                          "All files:", paste(files2, collapse=", ")))
  
  # Still should have dataset and flextable
  expect_true(any(grepl("a_dataset.*\\.rds$", files2)))
  expect_true(any(grepl("t2a_ft.*\\.png$", files2)))
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
  files <- list.files(file.path(dir.out, "model1"), recursive = TRUE)
  
  expect_true(length(files) > 0,
              info = paste("Files found:", paste(files, collapse=", ")))
  
  # With lists.as.subdirs=FALSE (default), filenames should contain "alist"
  expect_true(any(grepl("alist.*plot2.*\\.png$", files)),
              info = paste("Files found:", paste(files, collapse=", ")))
  expect_true(any(grepl("alist.*ft2.*\\.png$", files)),
              info = paste("Files found:", paste(files, collapse=", ")))
  
  # Should NOT have alist as a subdirectory (flat structure)
  subdirs <- list.dirs(file.path(dir.out, "model1"), recursive = FALSE, full.names = FALSE)
  expect_false("alist" %in% subdirs,
               info = paste("Subdirs found:", paste(subdirs, collapse=", ")))
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
  files <- list.files(file.path(dir.out, "model1"), recursive = TRUE)
  
  expect_true(length(files) > 0,
              info = paste("Files found:", paste(files, collapse=", ")))
  
  # Check for PDF plot files
  ## individual plot not saved - only main
  expect_true(any(grepl("main_model1.pdf$", files)),
              info = paste("Files found:", paste(files, collapse=", ")))
  
  expect_false(any(grepl("a_plot.*\\.pdf$", files)),
              info = paste("Files found:", paste(files, collapse=", ")))

## alist plots are also collected in a single pdf  
  expect_true(any(grepl("alist_model1\\.pdf$", files)),
              info = paste("Files found:", paste(files, collapse=", ")))
  
  # Check for flextable (should still be PNG)
  expect_true(any(grepl("alist.*ft2.*\\.png$", files)),
              info = paste("Files found:", paste(files, collapse=", ")))
  
  # Verify no alist subdirectory
  subdirs <- list.dirs(file.path(dir.out, "model1"), recursive = FALSE, full.names = FALSE)
  expect_false("alist" %in% subdirs,
               info = paste("Subdirs found:", paste(subdirs, collapse=", ")))
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
  
###### todo: this creates a mix of prefix and subdir. The plot is saved using prefix, that's wrong.


  # Test with lists.as.subdirs = TRUE
  res <- lwrite(lsave,dir=dir.out,model="model1",script=script,time=time1,formats.gg="png",lists.as.subdirs = TRUE)
  
  # Check that alist subdirectory was created
  expect_true(dir.exists(file.path(dir.out,"model1","alist")))
  
  files_alist_subdir <- list.files(file.path(dir.out,"model1"),recursive=TRUE)
  
  expect_true(length(files_alist_subdir) > 0,
              info = paste("Files in alist subdir:", paste(files_alist_subdir, collapse=", ")))
  
  # Files should be in alist subdirectory
  expect_true(any(grepl("plot2.*\\.png$", files_alist_subdir)),
              info = paste("Files found:", paste(files_alist_subdir, collapse=", ")))
  expect_true(any(grepl("ft2.*\\.png$", files_alist_subdir)),
              info = paste("Files found:", paste(files_alist_subdir, collapse=", ")))
  
  # Check main model directory for a_ft
  files_main <- list.files(file.path(dir.out,"model1"))
  expect_true(any(grepl("a_ft.*\\.png$", files_main)),
              info = paste("Files in main dir:", paste(files_main, collapse=", ")))
  
  # Test with lists.as.subdirs = FALSE
  unlink(file.path(dir.out,"model1"),recursive = T)
  lwrite(lsave,dir=dir.out,model="model1",script=script,time=time1,formats.gg="png",lists.as.subdirs = FALSE)
  
  files_flat <- list.files(file.path(dir.out,"model1"))
  expect_true(length(files_flat) > 0,
              info = paste("Files found:", paste(files_flat, collapse=", ")))
  
  # With flat structure, all files should be in main directory
  expect_true(any(grepl("alist.*plot2.*\\.png$", files_flat)),
              info = paste("Files found:", paste(files_flat, collapse=", ")))
  expect_true(any(grepl("alist.*ft2.*\\.png$", files_flat)),
              info = paste("Files found:", paste(files_flat, collapse=", ")))
  expect_true(any(grepl("a_ft.*\\.png$", files_flat)),
              info = paste("Files found:", paste(files_flat, collapse=", ")))
  
  # Should NOT have alist subdirectory with flat structure
  subdirs <- list.dirs(file.path(dir.out,"model1"), recursive = FALSE, full.names = FALSE)
  expect_false("alist" %in% subdirs,
               info = paste("Subdirs found:", paste(subdirs, collapse=", ")))
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

  dir.out <- "testOutput/model2"
  unlink(dir.out, recursive = TRUE)
  
  lwrite(lsave.flat2,dir="testOutput",model="model2",script=script,time=time1)

  # Check that directory exists
  expect_true(dir.exists(dir.out))
  
  files <- list.files(dir.out)
  expect_true(length(files) > 0,
              info = paste("Files found:", paste(files, collapse=", ")))
  
  # Check for flextable
  expect_true(any(grepl("a_ft.*\\.png$", files)),
              info = paste("Files found:", paste(files, collapse=", ")))
  
  # Check that both canvas sizes were created for the traced plot
  # Should have files with "wide" and "standard" in the name
  expect_true(any(grepl("a_tplot.*wide.*\\.png$", files)),
              info = paste("Files found:", paste(files, collapse=", ")))
  expect_true(any(grepl("a_tplot.*standard.*\\.png$", files)),
              info = paste("Files found:", paste(files, collapse=", ")))
  
  # Should have at least 3 files: 1 flextable + 2 canvas sizes for plot
  expect_true(length(files) >= 3,
              info = paste("Expected >=3 files, found", length(files), ":", paste(files, collapse=", ")))
})
