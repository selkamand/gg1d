#' Assert input is greater than a specified minimum value
#'
#' Assert all elements in a numeric vector/matrix are above some minimum value.
#'
#' @include assert_create.R
#' @include assert_type.R
#' @include is_functions.R
#' @include is_comparisons.R
#' @param x An object to check
#' @param minimum The minimum value to compare against (number)
#' @param msg A character string containing the error message to display if `x` is not greater than the specified minimum value (string)
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is greater than the specified minimum value, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_all_greater_than(3, 2) # Passes
#' assert_all_greater_than(c(2,3,4), 1) # Passes
#' assert_all_greater_than(c(2,3,4), 2) # Passes
#' assert_all_greater_than(c(2,3,1), 3) # Throws default error
#' assert_all_greater_than(c(2,3,1), 3, msg = "custom error message") # Throws custom error
#' })
#'
#' @concept assert_comparison
#' @export
assert_all_greater_than <- assert_create_chain(
  assert_numeric,
  assert_create(
    is_greater_than,
    default_error_msg = "{.strong {arg_name}} must {ifelse(length(arg_value) > 1, 'all ', '')}be {.strong greater than} `{.strong {minimum}}`."
  )
)

#' Assert input is greater than some minimum value
#'
#' Assert a number is greater than a specified minimum value.
#' To check all numbers in a vector / matrix are above a minimum value, see [assert_all_greater_than()]
#'
#' @include assert_create.R
#' @include assert_type.R
#' @include is_functions.R
#' @include is_comparisons.R
#' @param x An object to check
#' @param minimum The minimum value to compare against (number)
#' @param msg A character string containing the error message to display if `x` is not greater than the specified minimum value (string)
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is greater than the specified minimum value, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_greater_than(3, 2) # Passes
#' assert_greater_than(3, 2) # Passes
#' assert_greater_than(c(2,3,4), 1) # Throws error (Must be a number)
#' assert_greater_than('A', 1) # Throws error (Must be a number)
#' assert_greater_than(2, 3, msg = "custom error message") # Throws custom error
#' })
#'
#' @concept assert_comparison
#' @export
assert_greater_than <- assert_create_chain(
  assert_number,
  assert_all_greater_than
)

#' Assert input is greater than or equal to a specified minimum value
#'
#' Assert all elements in a numeric vector/matrix are above some minimum value.
#'
#' @param x An object to check
#' @param minimum The minimum value to compare against
#' @param msg A character string containing the error message to display if `x` is not greater than or equal to the specified minimum value (string)
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is greater than or equal to the specified minimum value, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_greater_than_or_equal_to(3, 2) # Passes
#' assert_greater_than_or_equal_to(c(3, 4, 5), 2) # Passes
#' assert_greater_than_or_equal_to(2, 3) # Throws error
#' })
#' @export
assert_all_greater_than_or_equal_to <- assert_create_chain(
  assert_numeric,
  assert_create(
    is_greater_than_or_equal_to,
    default_error_msg = "{.strong {arg_name}} must {ifelse(length(arg_value) > 1, 'all ', '')}be {.strong greater than or equal to} `{.strong {minimum}}`."
  )
)

#' Assert input is greater than or equal to a specified minimum value
#'
#' Assert all elements in a numeric vector/matrix are above or equal to some minimum value.
#' For vectorized version see [assert_all_greater_than_or_equal_to()]
#'
#' @param x An object to check
#' @param minimum The minimum value to compare against
#' @param msg A character string containing the error message to display if `x` is not greater than or equal to the specified minimum value (string)
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is greater than or equal to the specified minimum value, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_greater_than_or_equal_to(3, 2) # Passes
#' assert_greater_than_or_equal_to(c(3, 4, 5), 2) # Throws error
#' assert_greater_than_or_equal_to(2, 3) # Throws error
#' })
#' @export
assert_greater_than_or_equal_to <- assert_create_chain(
  assert_number,
  assert_all_greater_than_or_equal_to
)

#' Assert that the input object is identical to a specified value
#'
#' @param x An object to check
#' @param y The value to compare against
#' @param msg A character string containing the error message to display if `x` is not identical to the specified value
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is identical to the specified value, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_identical(3, 3) # Passes
#' assert_identical(c(3, 3, 3), 3) # Throws error
#' assert_identical(2, 3) # Throws error
#' })
#' @export
assert_identical <- assert_create(is_identical, default_error_msg = "{.strong {arg_name}} must be identical to {.strong {deparse(substitute(y))}}")


#' Assert that the input objects are equal
#'
#' Is `x` equal to `y`. powered by the [all.equal()] function.
#'
#' @param x An object to check
#' @param y The value to compare against
#' @param msg A character string containing the error message to display if `x` is not equal to `y`
#' @inheritParams common_roxygen_params
#' @inheritParams is_equal
#' @return invisible(TRUE) if `x` is equal to the specified value, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_equal(3, 3) # Passes
#' assert_equal(c(3, 3, 3), 3, ) # Fails
#' assert_equal(2, 3) # Throws error
#' })
#' @export
assert_equal <- assert_create(is_equal, default_error_msg = "{.strong {arg_name}} must be equal to {.strong {deparse(substitute(y))}}")



# #' Assert that the input have the same type are equal
# #'
# #' Assert `x` is the same type as `y`. powered by the [is_same_type()] function.
# #'
# #' @param x First object to compare
# #' @param y Second object to compare
# #' @param msg A character string containing the error message to display if `x` is not the same type as `y`
# #' @inheritParams common_roxygen_params
# #' @inheritParams is_same_type
# #' @return invisible(TRUE) if `x` is equal to the specified value, otherwise aborts with the error message specified by `msg`
# #'
# #' @examples
# #' try({
# #' assert_type_identical(c(3, 3, 3), 19) # Passes
# #' assert_type_identical(c(3, 3, 3), "bob") # Throws error
# #' })
# #' @export
#assert_type_identical <- assert_create(is_same_type, "{.strong {arg_name}} ({.strong {typeof(arg_value)}}) must be the same {aname(x)} type as {aname(y)} ({typeof(y)})")

#' Assert input is less than a specified maximum value
#'
#' Assert all elements in a numeric vector/matrix are below some maximum value.
#'
#' @include assert_create.R
#' @include assert_type.R
#' @include is_functions.R
#' @include is_comparisons.R
#' @param x An object to check
#' @param maximum The maximum value to compare against (number)
#' @param msg A character string containing the error message to display if `x` is not less than the specified maximum value (string)
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is less than the specified maximum value, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_all_less_than(1, 2) # Passes
#' assert_all_less_than(c(1,2,3), 4) # Passes
#' assert_all_less_than(c(1,2,3), 2) # Throws default error
#' assert_all_less_than(c(1,2,3), 2, msg = "custom error message") # Throws custom error
#' })
#'
#' @concept assert_comparison
#' @export
assert_all_less_than <- assert_create_chain(
  assert_numeric,
  assert_create(
    is_less_than,
    default_error_msg = "{.strong {arg_name}} must {ifelse(length(arg_value) > 1, 'all ', '')}be {.strong less than} `{.strong {maximum}}`."
  )
)

#' Assert input is less than some maximum value
#'
#' Assert a number is less than a specified maximum value.
#' To check all numbers in a vector / matrix are below a maximum value, see [assert_all_less_than()]
#'
#' @include assert_create.R
#' @include assert_type.R
#' @include is_functions.R
#' @include is_comparisons.R
#' @param x An object to check
#' @param maximum The maximum value to compare against (number)
#' @param msg A character string containing the error message to display if `x` is not less than the specified maximum value (string)
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is less than the specified maximum value, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_less_than(1, 2) # Passes
#' assert_less_than(1, 2) # Passes
#' assert_less_than(c(1,2,3), 4) # Throws error (Must be a number)
#' assert_less_than('A', 1) # Throws error (Must be a number)
#' assert_less_than(3, 2, msg = "custom error message") # Throws custom error
#' })
#'
#' @concept assert_comparison
#' @export
assert_less_than <- assert_create_chain(
  assert_number,
  assert_all_less_than
)

#' Assert input is less than or equal to a specified maximum value
#'
#' Assert all elements in a numeric vector/matrix are below or equal to some maximum value.
#'
#' @param x An object to check
#' @param maximum The maximum value to compare against
#' @param msg A character string containing the error message to display if `x` is not less than or equal to the specified maximum value (string)
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is less than or equal to the specified maximum value, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_less_than_or_equal_to(1, 2) # Passes
#' assert_less_than_or_equal_to(c(1, 2, 3), 3) # Passes
#' assert_less_than_or_equal_to(3, 2) # Throws error
#' })
#' @export
assert_all_less_than_or_equal_to <- assert_create_chain(
  assert_numeric,
  assert_create(
    is_less_than_or_equal_to,
    default_error_msg = "{.strong {arg_name}} must {ifelse(length(arg_value) > 1, 'all ', '')}be {.strong less than or equal to} `{.strong {maximum}}`."
  )
)

#' Assert input is less than or equal to a specified maximum value
#'
#' Assert a number is less than or equal to a specified maximum value.
#' For vectorized version see [assert_all_less_than_or_equal_to()]
#'
#' @param x An object to check
#' @param maximum The maximum value to compare against
#' @param msg A character string containing the error message to display if `x` is not less than or equal to the specified maximum value (string)
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is less than or equal to the specified maximum value, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_less_than_or_equal_to(1, 2) # Passes
#' assert_less_than_or_equal_to(c(1, 2, 3), 3) # Throws error
#' assert_less_than_or_equal_to(3, 2) # Throws error
#' })
#' @export
assert_less_than_or_equal_to <- assert_create_chain(
  assert_number,
  assert_all_less_than_or_equal_to
)
