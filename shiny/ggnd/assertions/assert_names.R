#' Check if a named object has all specified names
#'
#' This function returns a logical value indicating whether the object `x` has all the names specified in `names`.
#'
#' @param x a named object
#' @param names A character vector of names to check for in `x`.
#' @return A logical value indicating whether `x` has all the names specified in `names`
has_all_names <- function(x, names){
  is_subset(names, names(x))
}



#' Assert that the input object includes a specified name
#'
#' @param x An object to check for the presence of specific names
#' @param names A character vector of names to check for in `x`
#' @param msg A character string containing the error message to display if any of the `names` are not present in `x`
#' @inheritParams common_roxygen_params
#' @inheritParams has_all_names
#' @return invisible(TRUE) if all `names` are present in `x`, otherwise aborts with the error message specified by `msg`
#' @include is_functions.R
#' @examples
#' try({
#' x <- list(a = 1, b = 2, c = 3)
#'
#' assert_includes_name(x, "a") # Passes
#' assert_includes_name(x, c("a", "b")) # Passes
#' assert_includes_name(x, c("a", "b", "d")) # Throws default error message
#'
#' assert_includes_name(x, c("a", "b", "d"), "Custom error message") # Throws custom error message
#' })
#'
#' @export
assert_names_include <- assert_create(
  has_all_names,
  "'{.strong {arg_name}}' is missing {.strong {setopts_count_exlusive_to_first(names, names(arg_value))}} required name{?s}:
  {.strong `{setopts_exlusive_to_first(names, names(arg_value))}`}"
)
