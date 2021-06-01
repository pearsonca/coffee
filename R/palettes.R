#' Plot colours from names
#'
#' @param values character vector of named or hex colours.
#' @param label boolean. Do you want to label the plot or not?
#' @param square boolean. Display palette as square?
#'
#' @references Code inspired by hrbrmstr (https://stackoverflow.com/a/25726442)
#'
#' @importFrom graphics axis image text
#' @export
plot_palette <- function(values, label = TRUE, square = FALSE) {

  n <- length(values)
  if (square) {
    dimension <- ceiling(sqrt(n))
    total <- dimension ^ 2
    remainder <- total - n
    cols <- c(values, rep("white", remainder))
    lbls <- c(values, rep("", remainder))
    x = apply(matrix(total:1, dimension), 2, rev)
    image(x, col = matrix(cols, dimension), axes = FALSE, lab.breaks = NULL)
    if (label) {
      e <- expand.grid(seq(0, 1, length = dimension), seq(1, 0, length = dimension))
      text(e, labels = lbls, col = bw_contrast(cols))
    }
  } else {
    image(
      1:n, 1, as.matrix(1:n), col = values,
      xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n"
    )
    if (label) axis(1, at = 1:n, labels = values, tick = FALSE)
  }
  invisible(values)
}


#' What colour should you use for text
#'
#' Should you use black or white text for the given background colour
#'
#' @references
#' https://stackoverflow.com/a/3943023
#'
#' @importFrom grDevices col2rgb
#' @keywords internal
bw_contrast <- function(x) {
  dat <- as.data.frame(t(col2rgb(x))) / 255
  index <- dat <= 0.03928
  dat[index] <- dat[index] / 12.92
  dat[!index] <- ((dat[!index] + 0.055) / 1.055) ^ 2.4
  l <- 0.2126 * dat$red + 0.7152 * dat$green + 0.0722 * dat$blue
  condition <- (l + 0.05) / (0.0 + 0.05) > (1.0 + 0.05) / (l + 0.05)
  ifelse(condition, "black", "white")
}
