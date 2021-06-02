#' Easy quote characters
#'
#' @param x either an unquoted variable or a quoted string which you wish to split on spaces;
#'   all white space will be trimmed, then normalized to a single space
#' @param ... unquoted variables (seperated by commas) that you wish to quote;
#'   empty arguments (e.g. third item in `one,two,,four`) will be returned as blanks
#'
#' @return a character vector
#'
#' @examples
#' cq("  dale    audrey   laura hawk ")
#' cq(dale, audrey, laura, hawk)
#'
#' @importFrom utils capture.output
#' @export
cq <- function(x, ...) {
  (if (is.character(substitute(x)))
     unlist(strsplit(trimws(x), "[[:space:]]+"))
   else
     vapply(substitute(list(x, ...)), deparse, character(1))[-1]) -> res
  clipr::write_clip(capture.output(dput(res, control = "all")))
  res
}
