test_that("trclass correctly identifies data.frame", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_equal(trclass(df), "data.frame")
})

test_that("trclass correctly identifies flextable", {
  skip_if_not_installed("flextable")
  library(flextable)
  
  ft <- flextable(data.frame(a = 1:3))
  expect_equal(trclass(ft), "flextable")
})

test_that("trclass correctly identifies ggplot", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  expect_equal(trclass(p), "gg")
})

test_that("trclass correctly identifies gtable", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")
  library(ggplot2)
  library(gridExtra)
  
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  gt <- ggplotGrob(p)
  expect_equal(trclass(gt), "gg")
})

test_that("trclass correctly identifies list", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  
  # List of mixed objects
  lst <- list(
    df = data.frame(a = 1:3),
    num = 1:5
  )
  expect_equal(trclass(lst), "list")
})

test_that("trclass works with traced objects", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  p_traced <- traceit(p, canvas = "wide")
  
  # Should still identify as "gg" despite having "traceit" class prepended
  expect_equal(trclass(p_traced), "gg")
  expect_true("traceit" %in% class(p_traced))
})

test_that("trclass works with traced data.frame", {
  df <- data.frame(a = 1:3)
  df_traced <- traceit(df, file = "test.csv")
  
  expect_equal(trclass(df_traced), "data.frame")
  expect_true("traceit" %in% class(df_traced))
})

test_that("trclass works with traced flextable", {
  skip_if_not_installed("flextable")
  library(flextable)
  
  ft <- flextable(data.frame(a = 1:3))
  ft_traced <- traceit(ft, formats = "png")
  
  expect_equal(trclass(ft_traced), "flextable")
  expect_true("traceit" %in% class(ft_traced))
})

test_that("trclass throws error for unknown object types", {
  # Numeric vector
  expect_error(trclass(1:10), "x is an unknown format")
  
  # Character vector
  expect_error(trclass(c("a", "b", "c")), "x is an unknown format")
  
  # Matrix
  expect_error(trclass(matrix(1:4, 2)), "x is an unknown format")
  
  # Function
  expect_error(trclass(function(x) x + 1), "x is an unknown format")
})

test_that("trclass prioritizes flextable over data.frame", {
  skip_if_not_installed("flextable")
  library(flextable)
  
  # flextable objects are also data.frames in some implementations
  # trclass should identify as "flextable" first
  ft <- flextable(data.frame(a = 1:3))
  expect_equal(trclass(ft), "flextable")
})

test_that("trclass identifies list of plots as list, not gg", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  
  p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  p2 <- ggplot(mtcars, aes(hp, mpg)) + geom_point()
  
  # List of ggplots should be "list", not "gg"
  plot_list <- list(p1, p2)
  expect_equal(trclass(plot_list), "list")
})

test_that("trclass is consistent with is.gg behavior", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  
  # If is.gg returns TRUE, trclass should return "gg"
  if (is.gg(p)) {
    expect_equal(trclass(p), "gg")
  }
  
  # If is.gg returns FALSE for a list, trclass should return "list"
  lst <- list(p)
  if (!is.gg(lst)) {
    expect_equal(trclass(lst), "list")
  }
})

test_that("trclass handles NULL gracefully or throws appropriate error", {
  # NULL is not a recognized type, should error
  expect_error(trclass(NULL), "x is an unknown format")
})

test_that("trclass handles empty list", {
  # Empty list is still a list
  expect_equal(trclass(list()), "list")
})
