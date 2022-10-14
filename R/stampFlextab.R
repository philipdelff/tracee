##' Stamp and write flextab objects to one or multiple formats
##' @param ft a flextable object
##' @param script name of script - will be pasted as caption
##' @param file The file that the flextable will be written to (no file is written by this function)
##' @param bg Default bacground colour is #ffffff.
##' @import flextable
##' @export

## put stamps on tables plus a little tailoring of visuals
stampFlextab <- function(ft,file,script,bg="#ffffff",time){

    if(missing(file)){
        file <- NULL
    } else {
        file=basename(file)
    }
    if(missing(bg)||is.null(bg)) bg <- "#ffffff"
    if(missing(time)) time <- NULL
    
    ## stamp.full <- paste(format(Sys.time(), "%d-%b-%Y %H:%M"),script, file)
    stamp.full <- createStamp(script=script,file=file,time=time)
    
    ft <- theme_vanilla(ft)
    ft <- add_footer_lines(ft, stamp.full)
    ft <- color(ft, part = "footer", color = "#666666")
    ft <- fontsize(ft, part = "footer", size=6)
    ft <- align(ft, part="footer", align = "right")
    ft <- line_spacing(ft, space = .9, part = "footer")
    if(!is.null(bg)){
        ft <- bg(ft, part = "all", bg = bg)
    }
    ft
}

## ft <- flextable(tab.rep)
## ft <- width(ft,j=2,2)
## ft <- add_header_row(ft,
##                      colwidths = c(1,1,ncol(tab.rep)-3,1),
##                      values = c("","","Dosing day","")
##                      )

## fn <- fnAppend("table_simPK_q12h_load_ss.png",sprintf("%s_%smg",name.pars,dose.ss))
## ft <- stamp.flextab(ft,stamp.31,file=fn)
## ft <- align(ft,i=1:4,j=3:12,"right")
## ft <- align(ft,part="header",i=2,j=3:12,align="right")
## ## ft <- align(ft,part="header","center")
## ##ft <- align(ft,part="all",align="right")


## if(writeOutput){
##     save_as_image(ft,file.res(fn))
## }
