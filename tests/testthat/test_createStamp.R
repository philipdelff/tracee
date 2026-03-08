
test_that("basic",{

    fileRef <- "testReference/createStamp01.rds"
     res <- createStamp(script="ex.R",file="out.R",time="a test")

    ## expect_equal_to_reference(res,fileRef)
    expect_snapshot_value(res)

})



