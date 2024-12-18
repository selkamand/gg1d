#' Check if a vector is a subset of another
#'
#' This function checks that `x` is a subset of `y`
#'
#' @param x A vector to check
#' @param y the acceptable values that x can take
#' @inheritParams common_roxygen_params
#'
#' @return Returns invisible(TRUE) if `x` is a subset of `y`, otherwise throws an error
#' @examples
#' try({
#' assert_subset(1:3, 1:5) # Passes
#' assert_subset(c("A", "B", "C"), c("A", "B")) # Throws error since "C" is not present in first vector
#' })
#' @export
assert_subset <- assert_create(
  func = is_subset,
  default_error_msg = c(
    x = "'{.strong {arg_name}}' {cli::qty(length(setopts_exlusive_to_first(arg_value, y)))} contain{?s} {?an/} invalid value{?s}: {.strong {setopts_exlusive_to_first(arg_value, y)}}. Valid values include: {unique(y)}"
  )
)


#' Check if a scalar value is one of the acceptable values
#'
#' Assert `x` is one of the values of `y`.
#'
#' @param x A scalar value to check
#' @param y A vector of acceptable values that `x` can take
#' @inheritParams common_roxygen_params
#'
#' @return Returns invisible(TRUE) if `x` is a scalar and is one of the values in `y`, otherwise throws an error
#' @examples
#' assert_one_of(3, 1:5) # Passes because 3 is in 1:5
#' assert_one_of("A", c("A", "B", "C")) # Passes because "A" is in the vector
#'
#' try({
#'   assert_one_of("D", c("A", "B", "C")) # Throws error because "D" is not in the vector
#' })
#' @export
assert_one_of <- assert_create_chain(
  assert_scalar,
  assert_create(
    func = is_subset,
    default_error_msg = c(
      x = "'{.strong {arg_name}}' must be one of {.or {cli::cli_vec(y)}}, not {arg_value}."
    )
  )
)


#' Check if two sets are identical
#'
#' This function checks that `x` and `y` contain exactly the same elements, ignoring order and duplicates.
#'
#' @param x A vector to compare
#' @param y Another vector to compare with `x`
#' @inheritParams common_roxygen_params
#'
#' @return Returns `invisible(TRUE)` if `x` and `y` contain all the same elements (ignoring order and duplicates), otherwise throws an error.
#' @examples
#' # Passes because elements are the same, order doesn't matter
#' assert_set_equal(c(1, 2, 3), c(3, 2, 1))
#'
#' # Passes because elements are identical
#' assert_set_equal(c("A", "B", "C"), c("C", "A", "B"))
#'
#' try({
#'   # Throws error because elements are not identical
#'   assert_set_equal(c(1, 2, 3), c(1, 2))
#'
#'   # Throws error because elements differ
#'   assert_set_equal(c("A", "B"), c("A", "B", "C"))
#' })
#' @export
assert_set_equal <- assert_create(
  func = sets_are_equivalent
)
