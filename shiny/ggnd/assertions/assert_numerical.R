# For assert_number see assert_type. All other numerical assertions are here

#' Assert that the input object is a whole number
#'
#' Check if `x` is a whole number (no decimal)
#'
#' @param x An object
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is a whole number, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_whole_number(24) # Passes
#' assert_whole_number(2.5) # Throws error
#' })
#'
#' @concept assert_numerical
#' @export
assert_whole_number <- assert_create_chain(
  assert_number,
  assert_create(
    func = is_whole_number,
    default_error_msg = "'{.strong {arg_name}}' is not a {.strong whole} number")
)
