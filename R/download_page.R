#' Download a webpage
#'
#' Downloads a single webpage using wget and some hardcoded options.
#'
#' @param url Webpage to download with.
#' @param destination Folder to store webpage.
#' @param history Where to save the download history csv.
#'
#' @details
#'   `download_page()` calls wget with the following options:
#'
#'     * adjust-extension:  save HTML/CSS documents with proper extensions
#'     * span-hosts:        go to foreign hosts when recursive
#'     * convert-links:     make links in downloaded HTML or CSS point to local files
#'     * backup-converted:  before converting file X, back up as X.orig
#'     * page-requisites:   get all images, etc. needed to display HTML page
#'
#' @returns
#'
#'   * download_page()  - result (invisibly) of call to system2; 0 = successful.
#'   * get_dl_history() - data frame of downloaded webpages.
#'
#' @references
#' https://www.gnu.org/software/wget/manual/wget.html#Recursive-Retrieval-Options
#'
#' @export
download_page <- function(url, destination = Sys.getenv("WEBPAGE_DIR"),
						  history = Sys.getenv("WEBPAGE_DOWNLOAD_HISTORY")) {

	old_wd <- setwd(destination)
	on.exit(setwd(old_wd))

	url <- sub("/+$","", url) # remove trailing slashes from url

	options <- c(
		"--adjust-extension", # -E  save HTML/CSS documents with proper extensions
		"--span-hosts",       # -H  go to foreign hosts when recursive
		"--convert-links",    # -k  make links in downloaded HTML or CSS point to local files
		"--backup-converted", # -K  before converting file X, back up as X.orig
		"--page-requisites"   # -p  get all images, etc. needed to display HTML page
	)

	out <- system2("wget", args = c(options, url))
	if (!file.exists(history)) {
		if (!dir.exists(dirname(history))) dir.create(dirname(history))
		file.create(history)
		writeLines(text = c("url, time, success"), con = history)
	}
	cat(paste(url, Sys.time(), out, sep = ","), "\n", file = history, append = TRUE)
	invisible(out)
}

#' @rdname download_page
#' @export
get_dl_history <- function(history = Sys.getenv("WEBPAGE_DOWNLOAD_HISTORY")) {
	read.table(
		history,
		header = TRUE,
		sep = ",",
		colClasses = c(url = "character", time = "POSIXct", success = "integer")
	)
}
