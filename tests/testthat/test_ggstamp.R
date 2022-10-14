context("ggstamp")

test_that("general use",{

    fileRef <- "testReference/ggstamp1.rds"

    p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+geom_point()
    stamp <- "testthat_ggstamp.R"
    p1 <- ggstamp(p1,stamp,time=as.POSIXct("2011-04-03 01:40:33"))

    expect_equal_to_reference(p1,fileRef)
    
})


test_that("general use",{

    fileRef <- "testReference/ggstamp2.rds"

    p1 <- ggplot(data.frame(x=1,y=1),aes(x,y))+geom_point()
    stamp <- "testthat_ggstamp.R"
    p1 <- ggstamp(p1,stamp,file="test_ggstamp.png",time=as.POSIXct("2011-04-03 01:40:33"))

    expect_equal_to_reference(p1,fileRef)
    
})
