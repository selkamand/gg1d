
# Utils -------------------------------------------------------------------
msg_helper_assert_type <- function(expected_type, a = TRUE, an =FALSE){
  a <- ifelse(a, "a ", "")
  a <- ifelse(an, "an ", a)

  paste0("'{.strong {arg_name}}' must be ",a,"{.strong ",expected_type,"}, not a {.strong {class(arg_value)}}")

}

# Dataframe ---------------------------------------------------------------
#' Assert input is a data frame
#'
#' @include assert_create.R
#' @include is_functions.R
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not a data frame
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is a data frame, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_dataframe(mtcars) # Passes
#' assert_dataframe(data.frame()) # Passes
#'
#' assert_dataframe(1:10) # Throws default error
#' assert_dataframe(matrix(1:6, 2, 3)) # Throws default error
#' assert_dataframe(c(1, 2, 3)) # Throws default error: "Error
#' assert_dataframe(list(a = 1, b = 2)) # Throws default error
#' assert_dataframe(factor(c(1, 2, 3))) # Throws default error
#'
#' assert_dataframe(1:10, msg = "Custom error message") # Throws custom error
#' })
#'
#'
#' @concept assert_type
#' @export
assert_dataframe <- assert_create(
  func = is.data.frame,
  default_error_msg = msg_helper_assert_type("data.frame")
)


# Matrix ------------------------------------------------------------------


#' Assert input is a matrix
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not a matrix
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is a matrix, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_matrix(matrix(1:9, 3)) # Passes
#' assert_matrix(matrix(1:9, 3, 3)) # Passes
#' assert_matrix(c(1, 2, 3)) # Throws default error
#' assert_matrix(1:10, "Custom error message") # Throws custom error
#' })
#'
#' @concept assert_type
#' @export
assert_matrix <- assert_create(
  func = is.matrix,
  default_error_msg = msg_helper_assert_type("matrix")
)



# Vector ------------------------------------------------------------------
#' Assert input is a vector
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not a vector
#' @inheritParams is_vector
#' @inheritParams common_roxygen_params
#' @return invisible(TRUE) if `x` is a vector, otherwise aborts with the error message specified by `msg`
#'
#' @note
#' By default, lists are not considered vectors (i.e. `include_lists = FALSE`) to align with what end-users will expect, in spite of these objects technically being vectors.
#'
#' @examples
#' try({
#' assert_vector(c(1, 2, 3)) # Passes
#' assert_vector(matrix(1:6, 2, 3)) # Throws default error message
#' assert_vector(1:3) # Passes
#'
#' assert_vector(list(1, 2, 3)) # Throws default error message
#' assert_vector(list(1, 2, 3), include_lists = TRUE) # Passes
#'
#' assert_vector(c("a", 1, "b"), "Custom error message") # Throws custom error message
#' assert_vector(factor(c(1, 2, 3)), "Custom error message") # Throws custom error message
#' })
#'
#'
#' @concept assert_type
#' @export
assert_vector <- assert_create(func = is_vector, msg_helper_assert_type("vector"))


# assert_vector <- function(x, msg = NULL, include_lists = FALSE, call = rlang::caller_env()) {
#   string_argname <- deparse(substitute(x))
#   if (!is_vector(x) || (inherits(x, what = "list") && !include_lists)) {
#     if (is.null(msg)) {
#       msg = "'{.strong {string_argname}}' must be a vector, not a {class(x)}"
#     }
#     cli::cli_abort(msg, call = call)
#   }
#   invisible(TRUE)
# }

# Numerics ----------------------------------------------------------------


## factor -----------------------------------------------------------
#' Assert input is a factor
#'
#' Assert an R object is a factor.
#' Note that no assert_factor function exists since in R factors are always vector quantities (never scalar / in matrices)
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not a factor
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is a factor, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_factor_vector(factor(c("a", "b", "c"))) # Passes
#' assert_factor_vector(c("a", "b", "c")) # Throws default error
#' assert_factor_vector(factor(c("a", "b", "c")), "Custom error message") # Passes
#' assert_factor_vector(1:3, "Custom error message") # Throws custom error
#' })
#'
#' @details
#' Technically this function name is misleading, since \code{is.vector(factor(1)) == FALSE}
#' but since they act exactly like vectors to end users, I think this name is the most suitable
#'
#'
#' @concept assert_type
#' @export
assert_factor_vector <- assert_create(is.factor, default_error_msg = msg_helper_assert_type("factor"))


## numeric -----------------------------------------------------------
#' Assert input is numeric
#'
#' Assert an R object is numeric
#' Works for \strong{vector} and \strong{matrix} objects.
#' To assert an object is specifically a \strong{numeric vector} see [assert_numeric_vector()]
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not numeric
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is numeric, otherwise aborts with the error message specified by `msg`
#'
#'
#' @examples
#' try({
#' assert_numeric(1:3) # Passes
#' assert_numeric(1.5:5.5) # Passes
#' assert_numeric(c("a", "b", "c")) # Throws default error
#' assert_numeric(c("a", 1, "b"), "Custom error message") # Throws custom error
#' })
#'
#'
#' @concept assert_type
#' @export
assert_numeric <- assert_create(is.numeric, default_error_msg = msg_helper_assert_type("numeric", a = FALSE))

## numeric vector -----------------------------------------------------------
#' Assert input is a numeric vector
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not a numeric vector
#' @inheritParams common_roxygen_params
#' @inheritParams  is_numeric_vector
#' @return invisible(TRUE) if `x` is a numeric vector, otherwise aborts with the error message specified by `msg`
#'
#'
#'
#' @concept assert_type
#' @export
assert_numeric_vector <- assert_create(is_numeric_vector, default_error_msg = msg_helper_assert_type("numeric vector"))

## number -----------------------------------------------------------
#' Assert input is a number
#'
#' A number is a length 1 numeric vector.
#' Numbers can be either integers or doubles.
#'
#' @param x An object
#' @param msg A character string containing the error message to display if x is not a number
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if x is a number, otherwise aborts with the error message specified by msg
#'
#' @examples
#' assert_number(2) # Passes
#' try({
#' assert_number(c(2, 3)) # Throws default error
#' assert_number("a") # Throws default error
#' assert_number(c("a", 1, "b"), "Custom error message") # Throws custom error
#' })
#'
#'
#' @concept assert_type
#' @export
assert_number <- assert_create_chain(
  assert_create(is.numeric, "'{.strong {arg_name}}' is not a number! (class is {.strong {class(arg_value)}}, not numeric)"),
  assert_create(is_scalar, "'{.strong {arg_name}}' is not a number! (length is {.strong {length(arg_value)}}, not 1)")
)


#' Assert input is an integer
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not an integer
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is an integer, otherwise aborts with the error message specified by `msg`
#'
#' @note
#' In R, integers are whole numbers.
#' Both integers and doubles (numbers with decimals) are considered numeric.
#' This function checks that `x` specifically belong to the integer class.
#'
#' @examples
#' try({
#' assert_int(1) # Passes
#' assert_int(1:10) # Passes
#' assert_int(c(1, 2, 3)) # Passes
#' assert_int("a") # Throws default error
#' assert_int(1.5, msg = "Custom error message") # Throws custom error
#' })
#'
#'
#' @concept assert_type
#' @export
assert_int <- assert_create(is.integer, msg_helper_assert_type("integer", an = TRUE))


# Logicals ----------------------------------------------------------------

## logical -----------------------------------------------------------
#' Assert input is logical
#'
#' Assert an R object is 'logical' (TRUE/FALSE).
#' Works for \strong{vector} and \strong{matrix} objects.
#' To assert an object is specifically a \strong{logical vector} see [assert_logical_vector()]
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not logical
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is logical, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_logical(TRUE) # Passes
#' assert_logical(c(TRUE, FALSE, TRUE)) # Passes
#' assert_logical(c("a", "b")) # Throws default error
#' assert_logical(1:3, "Custom error message") # Throws custom error
#' })
#'
#'
#'
#' @concept assert_type
#' @export
assert_logical <- assert_create(is.logical, msg_helper_assert_type("logical", a = FALSE))


## logical vector -----------------------------------------------------------
#' Assert input is an atomic logical vector
#'
#'
#'
#' @param x An object
#' @param msg A character string containing the error message to display if x is not an atomic logical vector
#' @inheritParams common_roxygen_params
#' @inheritParams is_logical_vector
#' @return invisible(TRUE) if x is an atomic logical vector, otherwise aborts with the error message specified by msg
#'
#' @examples
#' try({
#' assert_logical_vector(c(TRUE, TRUE, TRUE)) # Passes
#' assert_logical_vector("a") # Throws default error
#' assert_logical_vector(c(1, 0, 1), "Custom error message") # Throws custom error
#' })
#'
#'
#' @concept assert_type
#' @export
assert_logical_vector <- assert_create(is_logical_vector, msg_helper_assert_type("logical vector"))


## flag -----------------------------------------------------------
#' Assert input is a scalar logical
#'
#' Assert input is a flag (a logical of length 1: `TRUE` or `FALSE`)
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not a scalar logical
#' @inheritParams common_roxygen_params
#' @return invisible(TRUE) if `x` is a scalar logical, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_flag(TRUE) # Passes
#' assert_flag(FALSE) # Passes
#' assert_flag(c(TRUE, FALSE)) # Throws default error
#' assert_flag(1, "Custom error message") # Throws custom error
#' })
#'
#'
#' @concept assert_type
#' @export
assert_flag <- assert_create_chain(
  assert_create(is.logical, "'{.strong {arg_name}}' is not a flag! (class is {.strong {class(arg_value)}}, not logical)"),
  assert_create(is_scalar, "'{.strong {arg_name}}' is not a flag! (length is {.strong {length(arg_value)}}, not 1)")
)


# Characters --------------------------------------------------------------

## character -----------------------------------------------------------
#' Assert input is a character vector
#'
#' Assert an R object is a 'character' type.
#' Works for \strong{vector} and \strong{matrix} objects.
#' To assert an object is specifically a \strong{character vector} see [assert_character_vector()]
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not a character vector
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is a character vector, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_character("a") # Passes
#' assert_character("a") # Passes
#' assert_character(c("a", "b", "c")) # Passes
#' assert_character(matrix(c('A', 'B', 'C', 'D')))  # Passes
#' assert_character(1:3) # Throws default error
#' assert_character(c("a", 1, "b"), "Custom error message") # Throws custom error
#' })
#'
#'
#' @concept assert_type
#' @export
assert_character <- assert_create(is.character, msg_helper_assert_type("character"))

## character vector -----------------------------------------------------------
#' Assert input is a character vector
#'
#' Assert an object is a character vector. Length 1 character vectors (strings) are considered vectors.
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not a character vector
#' @inheritParams common_roxygen_params
#' @inheritParams is_character_vector
#' @return invisible(TRUE) if `x` is a character vector, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_character_vector(c("a", "b", "c")) # Passes
#' assert_character_vector(c("a", 1, "b")) # Throws default error
#' assert_character_vector(matrix(c('A', 'B', 'C', 'D')))  # Throws error since type = matrix
#' assert_character_vector(c("a", 1, "b"), "Custom error message") # Throws custom error
#' assert_character_vector(glue::glue('A')) # Throws error
#' })
#'
#'
#' @concept assert_type
#' @export
assert_character_vector <- assert_create(is_character_vector, msg_helper_assert_type("character vector"))

## character vector or glue -----------------------------------------------------------
#' Assert input is a character vector / glue vector
#'
#' Assert an object is a character vector (or a glue vector). Length 1 character vectors (strings) are considered vectors.
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not a character vector
#' @inheritParams common_roxygen_params
#' @inheritParams is_character_vector
#' @return invisible(TRUE) if `x` is a character vector, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_character_vector_or_glue(c("a", "b", "c")) # Passes
#' assert_character_vector_or_glue(glue::glue('A')) # Passes
#' assert_character_vector_or_glue(c("a", 1, "b")) # Throws default error
#' assert_character_vector_or_glue(matrix(c('A', 'B', 'C', 'D')))  # Throws error since type = matrix
#' assert_character_vector_or_glue(c("a", 1, "b"), "Custom error message") # Throws custom error
#' })
#'
#'
#' @concept assert_type
#' @export
assert_character_vector_or_glue <- assert_create(is_character_vector_or_glue, msg_helper_assert_type("character vector"))

## string -----------------------------------------------------------
#' Assert input is a character string
#'
#' @param x An object
#' @param msg A character string containing the error message to display if x is not a string
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if x is a string, otherwise aborts with the error message specified by msg
#'
#' @examples
#' try({
#' assert_string("a") # Passes
#' assert_string(c("a", "b", "c")) # Throws default error
#' assert_string(1:3) # Throws default error
#' assert_string(c("a", 1, "b"), "Custom error message") # Throws custom error
#' })
#'
#'
#' @concept assert_type
#' @export
assert_string <- assert_create_chain(
  assert_create(is.character, "'{.strong {arg_name}}' is not a string! (class is {.strong {class(arg_value)}}, not character)"),
  assert_create(is_scalar, "'{.strong {arg_name}}' is not a string! (length is {.strong {length(arg_value)}}, not 1)")
  )

#' Assert input is a  non empty character string
#'
#' Asserts input is a string, and nonempty (i.e. not equal to '')
#'
#' @param x An object
#' @param msg A character string containing the error message to display if x is not a
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if x is a character vector, otherwise aborts with the error message specified by msg
#'
#' @examples
#' try({
#' assert_non_empty_string("a") # Passes
#' assert_non_empty_string("") # Fails
#' })
#'
#'
#' @concept assert_type
#' @export
assert_non_empty_string <- assert_create(
    func = is_non_empty_string_advanced
  )


# Functions ---------------------------------------------------------------

## function ----------------------------------------------------------------
#' Assert input is a function
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not a function
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is a function, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' # Assert that a variable is a function
#' x <- function(a, b) { a + b }
#' assert_function(x)  # does nothing
#'
#' # Assert that a variable is not a function
#' x <- "not a function"
#' assert_function(x)  # stops execution and prints an error message
#' })
#'
#'
#' @concept assert_type
#' @export
assert_function <- assert_create(is.function, msg_helper_assert_type(expected_type = "function"))



# Lists -------------------------------------------------------------------

#' Assert input is a list
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not a list
#' @inheritParams common_roxygen_params
#' @inheritParams is_list
#' @return invisible(TRUE) if `x` is a list, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' # Assert that a variable is a list
#' x <- list(1, 2, 3)
#' assert_list(x)  # does nothing
#'
#' # Assert that a variable is not a list
#' x <- "not a list"
#' assert_list(x)  # stops execution and prints an error message
#' })
#'
#' @concept assert_type
#' @export
assert_list <- assert_create(is_list, msg_helper_assert_type(expected_type = "list"))

#' Assert that x is reactive
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not reactive
#' @inheritParams common_roxygen_params
#' @inheritParams is_list
#' @return invisible(TRUE) if `x` is a reactive, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' # Assert that a variable is reactive
#' x <- shiny::reactive(1)
#' assert_reactive(x)  # does nothing
#'
#' # Assert that a variable is not a list
#' x <- 1
#' assert_reactive(x)  # stops execution and prints an error message
#' })
#'
#' @concept assert_type
#' @export
assert_reactive <- assert_create(func = is_reactive, default_error_msg = msg_helper_assert_type(expected_type = "reactive"))


## Scalar -----------------------------------------------------------
#' Assert input is a scalar
#'
#' Assert that an object is a scalar, meaning it is a length 1 atomic vector (such as \code{numeric(1)}, \code{character(1)} or \code{logical(1)}).
#' Note lists, data.frames and matrices are never considered scalar objects, even if they have only one element.
#'
#' @param x An object
#' @param msg A character string containing the error message to display if `x` is not a scalar
#' @inheritParams common_roxygen_params
#'
#' @return invisible(TRUE) if `x` is a scalar, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#'
#' # Pass when value is scalar
#' assert_scalar(5) # Passes
#' assert_scalar("single string") # Passes
#' assert_scalar(TRUE) # Passes
#'
#' # Fail when value is not
#' try({
#' assert_scalar(c(1, 2, 3)) # Throws default error
#' assert_scalar(matrix(1:4, 2, 2)) # Throws default error
#' })
#'
#'
#' @concept assert_type
#' @export
assert_scalar <- assert_create(
  func = is_scalar,
  default_error_msg = msg_helper_assert_type("scalar")
)

# Connections -----------------------------------------------------------------

#' Assert input is a database connection
#'
#' Assert the input object is a database connection, specifically of the "DBIConnection" class,
#' which is the standard virtual class used by the DBI package for database connections.
#' Note this assertion does not test if the database connection is valid and/or active.
#'
#' @param x An object to assert is a database connection
#' @param msg A custom error message displayed if `x` is not a valid database connection.
#' @inheritParams common_roxygen_params
#'
#' @return `invisible(TRUE)` if `x` is a valid database connection, otherwise aborts with an error message.
#'
#' @examples
#' try({
#'   # Assuming a valid DBI connection `conn`:
#'   assert_connection(conn) # Passes if `conn` is a DBI connection
#'
#'   assert_connection(42) # Fails with error message
#' })
#'
#' @details
#' This function is designed for use with objects inheriting from the "DBIConnection" class, which is used widely across database connection implementations in R.
#' As other database interface packages are introduced, additional checks may be added to support other connection classes.
#'
#' @concept assert_type
#' @export
assert_connection <- assert_create(
  func = is_connection,
  default_error_msg = msg_helper_assert_type("database connection")
)

