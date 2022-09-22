library(data.table)
library(devtools)
load_all()

data(mtcars)

mtc <- as.data.table(mtcars)
mtc
dt.sum <- mtc[,.(mean.hp=mean(hp)),by=.(cyl)]

library(flextable)
ft <- flextable(dt.sum)
ft

fn <- "a_file.png"
ft2 <- stampFlextab(ft,file=fn,script="testscript.R")

## sudo apt install r-cran-webshot
## webshot::install_phantomjs()

save_as_image(ft2,path="testOutput/flextab1.png")

writeFlextab(ft2,file="testOutput/flextab1.png")
