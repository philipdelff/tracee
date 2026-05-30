OutputDirCreate <- function(dir.out){
  if(file.exists(dir.out) && !dir.exists(dir.out)){
    stop("A file with the name of the output dir exists. tracee should not have deleted this file. Inspect and either delete file manually, or change output directory name.")
  }
  if(!dir.exists(dir.out)) dir.create(dir.out,recursive=TRUE)

}
