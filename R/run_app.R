#' Launch the easyanalysis web app
#'
#' Starts a Shiny app for building a summary table from an uploaded dataset
#' (CSV or Excel) and downloading it as a Word document.
#'
#' @param ... Arguments passed on to [shiny::runApp()], e.g. `port` or
#'   `launch.browser`.
#'
#' @return Called for its side effect of running the app; does not return
#'   until the app is stopped.
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#'
#' @export
run_app <- function(...) {
  app_dir <- system.file("app", package = "easyanalysis")
  if (app_dir == "") {
    stop("Could not find the app directory. Try re-installing 'easyanalysis'.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
