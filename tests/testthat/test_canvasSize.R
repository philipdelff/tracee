context("canvasSize")

test_that("named canvas",{

    fileRef <- "testReference/canvasSize1.rds"
    canv1 <- canvasSize(canvas="wide")

    expect_equal_to_reference(canv1,fileRef)
    
})

test_that("custom",{
    fileRef <- "testReference/canvasSize2.rds"
    canv2 <- canvasSize(canvas=list(height=10,width=22))
    expect_equal_to_reference(canv2,fileRef)
})


