#' Easy quote characters
#'
#' @param x Character vector which you wish to split on spaces
#' @param ... Unquoted variables (seperated by commas) that you wish to quote
#'
#' @importFrom utils capture.output
#' @export
cq <- function(x, ...) {
  if (is.character(substitute(x))) {
    res <- trimws(x)
    res <- strsplit(res, "[[:space:]]+")
    if (length(res) == 1L) res <- unlist(res)
  } else {
    pr <- substitute(list(x, ...))
    pr <- vapply(pr, deparse, character(1))
    res <- pr[-1]
  }
  tmp <- capture.output(dput(res, control = "all"))
  clipr::write_clip(tmp)
  res
}
