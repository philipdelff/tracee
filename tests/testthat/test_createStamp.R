
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
    "ex.R\n2026-01-01 10:00 run001 out.png")

  ## empty placeholders are removed
  expect_equal(
    createStamp(script="ex.R",file="out.png",time=""),
    "ex.R\nout.png")

  expect_equal(
    createStamp(script="ex.R",file="out.png",time=t1,model="run001",
                stamp.format="%model | %script (%time)"),
    "run001 | ex.R (2026-01-01 10:00)")

  ## addto is kept above the stamp
  expect_equal(
    createStamp(script="ex.R",time="",addto="My caption",stamp.format="%script"),
    "My caption\nex.R")

  expect_error(createStamp(script="ex.R",stamp.format="%scrpt"))
})

test_that("stamp.funs",{
  expect_equal(
    createStamp(script="path/to/ex.R",file="out.png",time="",
                stamp.funs=list(script=basename)),
    "ex.R\nout.png")

  expect_equal(
    createStamp(script="ex.R",time=as.POSIXct("2026-01-01 10:00:00"),
                stamp.format="%script %time",
                stamp.funs=list(time=function(x) format(x,"%Y-%m-%d"))),
    "ex.R 2026-01-01")

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
  expect_equal(caption,"Made by ex.R")

  ft <- flextable::flextable(data.frame(a=1))
  fts <- ftstamp(ft,script="ex.R",time="",stamp.format="Made by %script")
  expect_true(any(grepl("Made by ex.R",
                        unlist(fts$footer$dataset))))
})
