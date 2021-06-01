#' Where are you in the newbies queue
#'
#' `where_am_i()` tells you where you are in the CRAN newbies queue.
#'
#' @note inspired by cransays but limited only to the newbies queue and with a
#'   more minimal feel (and graphics) :-). Currently not exported as EXPERIMENTAL.
#'
#' @importFrom utils read.table
#' @importFrom graphics points
#' @importFrom grDevices adjustcolor
#'
#' @noRd
where_am_i <- function(pkg = NULL) {
  url = "ftp://cran.r-project.org/incoming/newbies/"
  timestamp <- as.POSIXct(Sys.time())
  dat <- read.table(url(url))
  pkgs <- substring(dat$V9, 1, regexpr("_", dat$V9)-1)
  dates <- paste(format(Sys.Date(), "%Y"), do.call(paste, dat[6:8]))
  dates <- as.POSIXct(dates, format="%Y %b %d %H:%M", tz="Europe/Vienna")
  minus1 <- as.POSIXlt(dates)
  minus1$year <- minus1$year - 1
  dates[timestamp - dates < 0] <- as.POSIXct(minus1[timestamp - dates < 0])

  plot(
    x = dates, y = rep(0, length(dates)),
    pch = 19, col = adjustcolor("black", 0.1), cex = 2, yaxt='n', ann = FALSE
  )

  dat <- data.frame(pkgs, dates)[order(dates), ]
  rownames(dat) <- NULL
  if (!is.null(pkg)) {
    idx <- which(dat$pkgs == pkg)
    points(dat$dates[idx], 0, pch = 19, col = "red", cex = 2)
    cat(sprintf("%s is number %d in the queue out of %d\n\n", pkg, idx, nrow(dat)))
  }
  dat
}

