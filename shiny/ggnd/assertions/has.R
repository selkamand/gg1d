# Has Functions -----------------------------------------------------------

#' Check if a vector has duplicates
#'
#' This function returns a logical value indicating whether the input vector contains
#' duplicated elements.
#'
#' @param x A vector.
#' @return A logical value indicating whether the input vector contains duplicated elements.
#' @examples
#' if(interactive()){
#' has_duplicates(c(1, 2, 3))  # returns FALSE
#' has_duplicates(c(1, 2, 2))  # returns TRUE
#' }
has_duplicates <- function(x){
  anyDuplicated(x) != 0
}

#' Check if a vector has no duplicates
#'
#' This function returns a logical value indicating whether the input vector contains
#' no duplicated elements.
#'
#' @param x A vector.
#' @return A logical value indicating whether the input vector contains no duplicated elements.
#' @examples
#' if(interactive()){
#' has_no_duplicates(c(1, 2, 3))  # returns TRUE
#' has_no_duplicates(c(1, 2, 2))  # returns FALSE
#' }
has_no_duplicates <- function(x){
  !has_duplicates(x)
}

#' Check if a vector has missing values
#'
#' This function returns a logical value indicating whether the input vector contains
#' missing values (`NA`).
#'
#' @param x A vector.
#' @return A logical value indicating whether the input vector contains missing values.
#' @examples
#' if(interactive()){
#' has_missing_values(c(1, 2, 3))  # returns FALSE
#' has_missing_values(c(1, NA, 2))  # returns TRUE
#' }
has_missing_values <- function(x){
  anyNA(x)
}

#' Check if a vector has no missing values
#'
#' This function returns a logical value indicating whether the input vector contains
#' no missing values (`NA`).
#'
#' @param x A vector.
#' @return A logical value indicating whether the input vector contains no missing values.
#' @examples
#' if(interactive()){
#' has_no_missing_values(c(1, 2, 3))  # returns TRUE
#' has_no_missing_values(c(1, NA, 2))  # returns FALSE
#' }
has_no_missing_values <- function(x){
  !has_missing_values(x)
}

# Utils -------------------------------------------------------------------
#' Count the number of missing values in a vector
#'
#' This function returns the number of missing values (`NA`) in the input vector.
#'
#' @param x A vector.
#' @return The number of missing values in the input vector.
#' @examples
#' if(interactive()){
#' util_count_missing(c(1, 2, 3))  # returns 0
#' util_count_missing(c(1, NA, 2))  # returns 1
#' }
util_count_missing <- function(x){
  sum(is.na(x))
}

#' Count the number of duplicated values in a vector
#'
#' This function returns the number of duplicated values in the input vector.
#'
#' @param x A vector.
#' @return The number of duplicated values in the input vector.
#' @examples
#' if(interactive()) {
#' util_count_duplicates(c(1, 2, 2))  # returns 1
#' util_count_duplicates(c(1, 2, 3))  # returns 0
#' }
util_count_duplicates <- function(x){
  sum(duplicated(x))
}

#' Get the duplicated values in a vector
#'
#' This function returns a vector of the duplicated values in the input vector.
#'
#' @param x A vector.
#' @return A vector of the duplicated values in the input vector.
#' @examples
#' if(interactive()) {
#' util_get_duplicated_values(c(1, 2, 2))  # returns 2
#' util_get_duplicated_values(c(1, 2, 3))  # returns NULL
#' }
util_get_duplicated_values <- function(x){
  unique(x[duplicated(x)])
}


#' Check object is some class
#'
#' This function checks whether object is a specific class
#'
#' @param x A value to check.
#' @param class checks if `x` belongs to `class`. If multiple values of `class` are supplied, returns whether `x` belongs to any of them (character)
#' @return A logical scalar indicating `x` belongs to `class`
#' @examples
#' if(interactive()) {
#'   has_class(1, "numeric") # TRUE
#'   has_class(1, "character") # FALSE
#' }
#' @concept is_type
#'
has_class <- function(x, class){
  return(inherits(x, what = class))
}


# Assertions --------------------------------------------------------------
#' Assert that the input vector has no missing values
#'
#' This function asserts that the input vector has no missing values (`NA`) and aborts
#' with an error message if it does.
#'
#' @param x A vector.
#' @param msg A character string containing the error message to display if `x` has missing values.
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` has no missing values (NA), otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_no_missing(c(1, 2, 3))  # Passes
#' assert_no_missing(c(1, NA, 2))  # Throws default error
#'
#' assert_no_missing(c(1, 2, 3), msg = "Custom error message")  # Passes
#' assert_no_missing(c(1, NA, 2), msg = "Custom error message")  # Throws custom error
#' })
#'
#' @concept assert_has
#' @export
assert_no_missing <- assert_create(
  has_no_missing_values,
  default_error_msg = "'{.strong {arg_name}}' must have {.strong no missing values}! Found {.strong {util_count_missing(arg_value)}}"
)




#' Assert that the input vector has no duplicates
#'
#' Assert the input vector has no duplicated elements
#'
#' @param x A vector.
#' @param msg A character string containing the error message to display if `x` has duplicates.
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` has no duplicates, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_no_duplicates(c(1, 2, 3))  # Passes
#' assert_no_duplicates(c(1, 2, 2))  # Throws default error
#'
#' assert_no_duplicates(c(1, 2, 3), msg = "Custom error message")  # Passes
#' assert_no_duplicates(c(1, 2, 2), msg = "Custom error message")  # Throws custom error
#' })
#'
#' @concept assert_has
#' @export
assert_no_duplicates <- assert_create(
  has_no_duplicates,
  "'{.strong {arg_name}}' must have {.strong no duplicates}! Found {.strong {util_count_duplicates(arg_value)}} duplicated value{?s}: {.emph {util_get_duplicated_values(arg_value)}}"
)


#' Assert object belongs to class
#'
#' This function asserts that the input object belongs to `class`
#'
#' @param x An input object
#' @param msg A character string containing the error message to display if `x` does not belong to `class`
#' @inheritParams common_roxygen_params
#' @inheritParams has_class
#' @return invisible(TRUE) if `x` belongs to `class`, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_has_class(1, "numeric")  # Passes
#' assert_has_class(1, "character")  # Throws default error
#' })
#'
#' @concept assert_has
#' @export
assert_class <- assert_create(
  has_class,
  "'{.strong {arg_name}}' must belong to class {.strong '{class}'}, not {.strong {class(arg_value)}}"
)
