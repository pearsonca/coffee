#' Create basic package sans test setup
#'
#' `np()` (new package) creates a minimal package with description.  It
#' borrows somewhat from Dirk Eddelbuettel's pkgKitten
#' [pkgKitten](https://cran.r-project.org/web/packages/pkgKitten/index.html)
#' but changed to fit my workflow.
#'
#' Authorship details will be grabbed from the options FIRSTNAME, SURNAME, EMAIL
#' and ORCID.
#'
#' @param pkgname The package name.
#' @param dir The directory to use for the package.
#' @param license What license to use for the package. Defaults to MIT.
#' @param firstname Your firstname.
#' @param surname Your surname.
#' @param email Your email.
#' @param orcid Your ORCID (optional).
#' @param open Open package directory after creation.
#' @param rstudio Use RStudio projects (.Rproj)?
#'
#' @importFrom utils packageVersion
#'
#' @export
np <- function(pkgname = "mypackage", dir, license = "MIT",
               firstname = getOption("FIRSTNAME", "Joe"),
               surname = getOption("SURNAME", "Bloggs"),
               email = getOption("EMAIL", "joe@bloggs.com"),
               orcid = getOption("ORCID"),
               open = TRUE, rstudio = TRUE) {

  # package directory
  root <- file.path(dir, pkgname)
  if (dir.exists(root)) {
    stop("package directory '", root, "' already exists. Aborting.", call. = FALSE)
  } else {
    dir.create(root, recursive = TRUE)
  }

  orig <- setwd(root)
  if (!open) on.exit(setwd(orig))

	# copy skeleton description
	desc_orig <- system.file("skeletons", "DESCRIPTION", package = "coffee")
	desc_target <- file.path(root, "DESCRIPTION")
	if (!file.exists(desc_target)) file.copy(desc_orig, desc_target)

	# edit description
	x <- read.dcf(desc_target)
	x[, "Package"] <- pkgname
	x[, "Authors@R"] <- deparse(
		bquote(utils::person(
			given = .(firstname),
			family = .(surname),
			email = .(email),
			role = c("aut", "cre"),
			comment = c(ORCID = .(orcid))
			)
		),
		width.cutoff = 500
	)
	roxy_version <- tryCatch(packageVersion("roxygen2"), error = function(e) NULL)
	if (!is.null(roxy_version)) {
	  x[, "RoxygenNote"] <-  as.character(roxy_version)
	} else {
	  x[, "RoxygenNote"] <-  "7.1.0"
	}
	x[, "License"] <- license

	# if mit licence add the relevant licence.md file
	if (tolower(license) == "mit") {
	  x[, "License"] <- "MIT + file LICENSE"
	  mit <- system.file("skeletons", "MIT_LICENSE.md", package = "coffee")
	  target_mit <- file.path(root, "LICENSE.md")
	  if (!file.exists(target_mit)) file.copy(mit, target_mit)
	  license_file <- file.path(root, "LICENSE")
	  if (!file.exists(license_file)) {
	    file.create(license_file)
	    cat(sprintf("YEAR: %s\n", format.Date(Sys.Date(), "Y")), file = license_file)
	    cat(sprintf("COPYRIGHT HOLDER: %s authors", pkgname), file = license_file, append = TRUE)
	  }
	}

	# write DESCRIPTION out
	write.dcf(x, file = desc_target)

	# copy over skeleton .gitignore
	.gitignore <- system.file("skeletons", "R.gitignore", package = "coffee")
	target_gitignore <- file.path(root, ".gitignore")
	if (!file.exists(target_gitignore)) file.copy(.gitignore, target_gitignore)

	# copy over skeleton .Rbuildignore
	.Rbuildignore <- system.file("skeletons", "dot.Rbuildignore", package = "coffee")
	target_Rbuildignore <- file.path(root, ".Rbuildignore")
	if (!file.exists(target_Rbuildignore)) file.copy(.Rbuildignore, target_Rbuildignore)

	# crete R folder
	dir.create("R")

	# initialise git
	gert::git_init()

	# open in rstudio if chosen and available
	if (rstudio && rstudioapi::isAvailable()) {
		rstudioapi::initializeProject()
	  if (open) rstudioapi::openProject(root)
	}

	invisible(NULL)
}