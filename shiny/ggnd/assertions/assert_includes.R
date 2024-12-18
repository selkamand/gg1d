#' Assert object includes required
#'
#' Assert x includes required elements
#'
#' @include assert_create.R
#' @include set_operations.R
#' @param x An object
#' @param msg A character string describing the error message if `x` does not include `required` elements
#' @inheritParams common_roxygen_params
#' @inheritParams includes_advanced
#'
#' @return invisible(TRUE) if `x` includes all `required` elements, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_directory(system.file("package = assertions"))
#' assert_directory("foo") # Throws Error
#' })
#'
#' @concept assert_includes
#' @export
assert_includes <- assert_create(
  includes_advanced
)

#' Assert object does not include any illegal values
#'
#' Assert x does not include illegal elements
#'
#' @include assert_create.R
#' @include set_operations.R
#' @param x An object
#' @param msg A character string describing the error message if `x` includes any `illegal` elements
#' @inheritParams common_roxygen_params
#' @inheritParams excludes_advanced
#'
#' @return invisible(TRUE) if `x` includes any `illegal` elements, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_directory(system.file("package = assertions"))
#' assert_directory("foo") # Throws Error
#' })
#'
#' @concept assert_includes
#' @export
assert_excludes <- assert_create(
  excludes_advanced
)
