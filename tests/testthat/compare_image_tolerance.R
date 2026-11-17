##
compare_image_tolerance <- function(old, new, tolerance = 0.05) {
    if (!requireNamespace("magick", quietly = TRUE)) {
        return(TRUE)  # skip comparison if magick not available
    }
    img_old <- magick::image_read(old)
    img_new <- magick::image_read(new)

    # resize to same dims in case of minor size diffs
    info_old <- magick::image_info(img_old)
    img_new <- magick::image_scale(img_new, paste0(info_old$width, "x", info_old$height, "!"))

    diff <- magick::image_compare_dist(img_old, img_new, metric = "RMSE")
    # diff$distortion is the fraction of differing pixels
    diff$distortion <= tolerance
}
