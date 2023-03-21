# tracee
`tracee` is an R package that provides convenient wrappers for saving
traceable plots and tables for reporting.

## Install
`tracee` is on CRAN and MPN.
```{r}
install.packages("tracee")
```

## Save plots with script and output paths included
Device type will be decided based on file name extension. png and pdf are supported.
```{r}
ggwrite(p,file="path/to/myplot.png",script="path/to/script")
```

## Save tables with script and output paths included
Only flextable objects can be saved.
```{r}
ftwrite(ft,file="path/to/mytable.png",script="path/to/script")
```
