
test_that("basic",{

    fileRef <- "testReference/createStamp01.rds"
     res <- createStamp(script="ex.R",file="out.R",time="a test")

    ## expect_equal_to_reference(res,fileRef)
    expect_snapshot_value(res)

})




test_that("stamp.format",{
  t1 <- "2026-01-01 10:00"
  expect_equal(
    createStamp(script="ex.R",file="out.png",time=t1,model="run001"),
    "Output: out.png\nScript: ex.R\nModel: run001\nTime: 2026-01-01 10:00")

  ## empty placeholders are removed
  expect_equal(
    createStamp(script="ex.R",file="out.png",time=""),
    "Output: out.png\nScript: ex.R")

  expect_equal(
    createStamp(script="ex.R",file="out.png",time=t1,model="run001",
                stamp.format="%model | %script (%time)"),
    "Model: run001 | Script: ex.R (Time: 2026-01-01 10:00)")

  ## addto is kept above the stamp
  expect_equal(
    createStamp(script="ex.R",time="",addto="My caption",stamp.format="%script"),
    "My caption\nScript: ex.R")

  expect_error(createStamp(script="ex.R",stamp.format="%scrpt"))
})

test_that("stamp.funs",{
  expect_equal(
    createStamp(script="path/to/ex.R",file="out.png",time="",
                stamp.funs=list(script=basename)),
    "Output: out.png\nex.R")

  expect_equal(
    createStamp(script="ex.R",time=as.POSIXct("2026-01-01 10:00:00"),
                stamp.format="%script %time",
                stamp.funs=list(time=function(x) format(x,"%Y-%m-%d"))),
    "Script: ex.R 2026-01-01")

  ## components without a value are left out, also with prefixes
  expect_equal(createStamp(script="ex.R",time=""),"Script: ex.R")

  expect_error(createStamp(script="ex.R",stamp.funs=list(foo=identity)))
  expect_error(createStamp(script="ex.R",stamp.funs=list(script="a")))
})

test_that("stamp.format passed from ggwrite and ftwrite",{
  library(ggplot2)
  p1 <- ggplot(data.frame(x=1:3,y=1:3),aes(x,y))+geom_point()
  ps <- ggstamp(p1,script="ex.R",time="",stamp.format="Made by %script")
  caption <- if(exists("get_labs", asNamespace("ggplot2"))) {
    ggplot2::get_labs(ps)$caption
  } else {
    ps$labels$caption
  }
  expect_equal(caption,"Made by Script: ex.R")

  ft <- flextable::flextable(data.frame(a=1))
  fts <- ftstamp(ft,script="ex.R",time="",stamp.format="Made by %script")
  expect_true(any(grepl("Made by Script: ex.R",
                        unlist(fts$footer$dataset))))
})

test_that("stampPosition",{
  expect_equal(stampPosition(NULL),1)
  expect_equal(stampPosition("left"),0)
  expect_equal(stampPosition("center"),0.5)
  expect_equal(stampPosition(0.3),0.3)
  expect_error(stampPosition(2))
  expect_error(stampPosition("top"))
})
