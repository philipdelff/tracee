argsFromTrace <- function(fun, x, attr = "args") {

  if (!inherits(x, "traceit")) return(list())

  defs <- attr(x, attr)
  if (!is.list(defs)) return(list())

  arg_names <- setdiff(names(formals(fun)), "x")
  mc <- match.call(definition = fun, expand.dots = FALSE)

  ## only defaults for args that were not supplied
  defs[names(defs) %in% arg_names & !(names(defs) %in% names(mc))]
}
