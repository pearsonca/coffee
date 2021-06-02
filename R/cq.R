#' Easy quote characters
#'
#' @param ... Unquoted variables (seperated by commas) that you wish to quote
#' @param x Character vector which you wish to split on spaces
#'
#' @importFrom utils capture.output
#' @export
cq <- function(...) {
    x <- substitute(list(...))
    x <- vapply(x, deparse, character(1))
    res <- x[-1]
    tmp <- capture.output(dput(res, control = "all"))
    clipr::write_clip(tmp)
    res
}

#' @rdname cq
#' @export
ccq <- function(x) {
  res <- sub("^[[:space:]]+", "", x)
  res <- strsplit(res, "[[:space:]]+")
  if (length(res) == 1L) res <- unlist(res)
  tmp <- capture.output(dput(res, control = "all"))
  clipr::write_clip(tmp)
  res
}
