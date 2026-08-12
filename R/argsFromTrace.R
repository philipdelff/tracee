##' Extract arguments from a traceit object
##'
##' This function retrieves stored arguments from a traceit object's attributes
##' and returns those that match the formal arguments of a given function,
##' excluding arguments that were already supplied in the current call.
##'
##' @param fun A function whose formal arguments will be matched against the
##'   stored arguments in the traceit object
##' @param x An object to extract arguments from. Must inherit from "traceit"
##'   class, otherwise an empty list is returned
##' @param attr Character string specifying the name of the attribute containing
##'   the stored arguments. Default is "args"
##'
##' @return A named list of arguments that:
##'   \itemize{
##'     \item Match formal arguments of \code{fun} (excluding "x")
##'     \item Were stored in the traceit object's attributes
##'     \item Were NOT already supplied in the current function call
##'   }
##'   Returns an empty list if \code{x} does not inherit from "traceit" or if
##'   the specified attribute is not a list.
##'
##' @details
##' This function is used internally to retrieve default arguments that were
##' previously stored in a traceit object (via \code{traceit()} functions) and
##' apply them to subsequent function calls. It ensures that explicitly provided
##' arguments take precedence over stored defaults.
##'
##' @examples
##' \dontrun{
##' # Assuming a traceit object with stored arguments
##' p <- traceit(ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point(),
##'              canvas = "wide", formats = "png")
##' # argsFromTrace would extract canvas and formats if not already specified
##' args <- argsFromTrace(ggwrite, p)
##' }
##'
##' @keywords internal
argsFromTrace <- function(fun, x, attr = "args") {

  if (!inherits(x, "traceit")) return(list())

  defs <- attr(x, attr)
  if (!is.list(defs)) return(list())

  arg_names <- setdiff(names(formals(fun)), "x")
  mc <- match.call(definition = fun, expand.dots = FALSE)

  ## only defaults for args that were not supplied
  defs[names(defs) %in% arg_names & !(names(defs) %in% names(mc))]
}
