#' Test for Lato import and registration
#'
#' \code{lato_test} tests to see if Lato is imported and registered.
#'
#' Import and register Lato in R with lato_install().
#'
#' @md
#' @export
lato_test <- function() {
  if (sum(grepl("[Ll]ato$", extrafont::fonts())) > 0) {
    "Lato is imported and registered."
  } else {
    "Lato isn't imported and registered. Install from Google Fonts and import and register using lato_install()."
  }
}
