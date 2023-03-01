# tracee
`tracee` is an R package that generates .

## Install
Easiest way to install NMexec is using the remotes package to install with R:

    library(remotes)
    install_github("philipdelff/NMexec")

`NMexec` makes extensive use of functionality provided by the `NMdata`
package. For most recent features of `NMexec` to work, make sure to at
least keep `NMdata` updated to latest CRAN or MPN realease. In case
you need a very recent feature, you may need to install `NMdata` from
github too:

    install_github("philipdelff/NMdata")
    install_github("philipdelff/NMexec")
    library("NMexec")

## Simulate a Nonmem model from R
