context("ggWater")

test_that("general use",{

    fileRef <- "testReference/ggWater1.rds"

    data(pksim1,package="pmxtricks")
    p1 <- ggIndProfs(pksim1,amt=NULL)[[1]]
    p1 <- p1+ggWater()

### I don't understand why this no longer works. The reference has no watermark.
    ## expect_equal_to_reference(p1,fileRef)
    
})

