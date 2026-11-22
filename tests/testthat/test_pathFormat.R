

## load_all()

test_that("basic",{

  fun.path <- pathFormat()
  expect_error(fun.path())

  expect_equal(fun.path(name="scatter_wt.png"),
               "scatter_wt.png")

  fun.path2 <- pathFormat("%model/%name_%model")
  expect_error(fun.path2(name="scatter_wt.png"))
  expect_equal(fun.path2(model="run001",name="scatter_wt.png"),
               "run001/scatter_wt_run001.png")


})

test_that("general use",{
  fun1 <- pathFormat(path.format="%model/%name")

  expect_equal(fun1(name="hey.txt",model="mod"),"mod/hey.txt")

  expect_error(
    fun1(name="hey.txt")
  )
})


test_that("structure with file only",{
  fun1 <- pathFormat(path.format="%name")

  expect_equal(
    fun1(name="hey.txt")
   ,"hey.txt")

  expect_message(fun1(name="hey.txt",model="mod"))
  res <- fun1(name="hey.txt",model="mod")

  expect_equal(
    res
   ,"hey.txt")
})


test_that("path.funs",{
  struct1 <- pathFormat(path.format="%script/%name_%model",
                        path.funs = list(script = function(x) substr(basename(x), 1, 2)))

expect_equal(
  struct1(name="file",script="path/01_myscript",model="mymodel.lst")
 ,
  "01/file_mymodel")

})


test_that("deprecated structure argument and pathStruct()",{
  expect_message(fun1 <- pathFormat(structure="%model/%name"),
                 "structure argument is deprecated")
  expect_equal(fun1(name="hey.txt",model="mod"),"mod/hey.txt")

  expect_error(pathFormat(path.format="%name",structure="%name"))

  expect_message(fun2 <- pathStruct(path.format="%model/%name"),
                 "pathStruct\\(\\) is deprecated")
  expect_equal(fun2(name="hey.txt",model="mod"),"mod/hey.txt")

  dir.out <- tempfile()
  expect_message(
    lwrite(list(a=data.frame(x=1)),model="run001",dir=dir.out,structure="%name_%model",quiet=TRUE),
    "structure argument is deprecated")
  expect_true(file.exists(file.path(dir.out,"a_run001.rds")))
})

test_that("specify default values",{
  fun.path <- pathFormat("%model/%name_%model",model="model1",dir="outputs")
  
  expect_equal(
    fun.path(name="scatter_wt.png")
   ,"outputs/model1/scatter_wt_model1.png")

  expect_equal(
    fun.path(model="model2",name="scatter_wt.png"),
    "outputs/model2/scatter_wt_model2.png"  
  )
})
