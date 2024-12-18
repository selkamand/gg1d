#' Format error messages as html
#'
#' @description Format error messages as html
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
err2html <- function(err){
  tags$code(HTML(cli::ansi_html(as.character(err))))
}
