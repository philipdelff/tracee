#' Pad Numbers with Leading Zeros
#'
#' Converts numeric values to character strings with leading zeros to achieve
#' a specified total width.
#'
#' @param x A numeric vector of integers to be padded with leading zeros.
#' @param nchars An integer specifying the total number of characters in the
#'   output string (including leading zeros).
#'
#' @return A character vector of the same length as \code{x}, with each element
#'   padded with leading zeros to reach \code{nchars} characters.
#'
#' @examples
#' # Pad single digit numbers to 3 characters
#' padZeros(5, 3)
#' # Returns: "005"
#'
#' # Pad a vector of numbers
#' padZeros(c(1, 10, 100), 4)
#' # Returns: c("0001", "0010", "0100")
#'
#' # Useful for creating sequential file names
#' paste0("file_", padZeros(1:5, 3), ".txt")
#' library(NMdata)
#' NMdata::fnAppend(paste0("file.txt"),padZeros(1:5, 3),collapse=NULL)
#' # Returns: c("file_001.txt", "file_002.txt", ..., "file_005.txt")
#'
#' @export
padZeros <- function(x,nchars){

  sprintf(fmt=paste("%0",nchars,"d",sep=""),x)

}
