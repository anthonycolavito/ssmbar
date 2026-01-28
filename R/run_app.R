#' Launch the Benefit Explorer Shiny App
#'
#' Launches an interactive Shiny application for exploring Social Security
#' benefit calculations. The app allows users to visualize benefits,
#' replacement rates, lifetime present values, and benefit-tax ratios
#' for various worker configurations.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#'   Useful options include:
#'   \itemize{
#'     \item \code{port}: TCP port for the app (default is random available port)
#'     \item \code{host}: IP address to listen on (default "127.0.0.1")
#'     \item \code{launch.browser}: Whether to open browser automatically (default TRUE)
#'   }
#'
#' @return This function normally does not return; interrupt R to stop the
#'   application (usually by pressing Ctrl+C or Escape).
#'
#' @details
#' The Benefit Explorer app includes four main tabs:
#' \describe{
#'   \item{Benefits}{Benefit amounts over time in real and nominal dollars}
#'   \item{Replacement Rates}{Multiple replacement rate calculations from \code{rep_rates()}}
#'   \item{Lifetime Value}{Present value of lifetime benefits and taxes}
#'   \item{Ratios}{Benefit-tax ratios for workers and couples}
#' }
#'
#' The sidebar allows configuration of worker parameters including:
#' \itemize{
#'   \item Worker type (very_low, low, medium, high, max, or custom)
#'   \item Birth year (1940-2010)
#'   \item Sex (male, female, or gender-neutral)
#'   \item Claim age (62-70)
#'   \item Custom earnings input (when type = "custom")
#'   \item Spouse configuration (optional)
#' }
#'
#' @examples
#' \dontrun{
#' # Launch the app with default settings
#' run_app()
#'
#' # Launch on a specific port
#' run_app(port = 3838)
#'
#' # Launch without opening browser
#' run_app(launch.browser = FALSE)
#' }
#'
#' @seealso \code{\link{calculate_benefits}}, \code{\link{pv_lifetime_benefits}},
#'   \code{\link{pv_lifetime_taxes}}, \code{\link{benefit_tax_ratio}}
#'
#' @export
run_app <- function(...) {
  # Check if shiny is available

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run this app. Please install it with: install.packages('shiny')")
  }

  # Find the app directory

  app_dir <- system.file("shiny", "benefit_explorer", package = "ssmbar")

  if (app_dir == "") {
    stop("Could not find the Benefit Explorer app. Try re-installing the 'ssmbar' package.")
  }

  # Launch the app
  shiny::runApp(app_dir, ...)
}
