#' Easy quote characters
#'
#' @param ... Unquoted variables (seperated by commas) that you wish to quote
#' @param x Character vector which you wish to split on spaces
#'
#' @export
cq <- function(...) {
    x <- substitute(list(...))
    x <- vapply(x, deparse, character(1))
    x[-1]
}

#' @rdname cq
#' @export
ccq <- function(x) {
  out <- strsplit(x, " ")
  if (length(out) == 1L) out <- unlist(x)
  out
}
