# Assert Length -----------------------------------------------------------

#' Assert Length
#'
#' Assert object has a specific length
#'
#' @param x object to check length of
#' @param length expected length (number)
#' @param msg custom error message
#' @param call (logical) whether to preserve call in error message
#' @param arg_name (character) name of argument being tested
#'
#' @return invisible(TRUE)
#' @export
assert_length <- assert_create(
  func = function(x, length) {
    if(!is.numeric(length)) return("'length' must be numeric")
    if(length(length) != 1) return("'length' must be a single number")
    if(!is_whole_number(length)) return("'length' must be a whole number")
    if(length < 0) return("'length' must be non-negative")
    
    length(x) == length
  },
  default_error_msg = "'{arg_name}' must have length {length}, not {length(arg_value)}"
)

#' Assert Length Greater Than
#'
#' Assert object length is greater than a threshold
#'
#' @inheritParams assert_length
#' @return invisible(TRUE)
#' @export
assert_length_greater_than <- assert_create(
  func = function(x, length) {
    if(!is.numeric(length)) return("'length' must be numeric")
    if(length(length) != 1) return("'length' must be a single number")
    if(!is_whole_number(length)) return("'length' must be a whole number")
    if(length < 0) return("'length' must be non-negative")
    
    length(x) > length
  },
  default_error_msg = "'{arg_name}' must have length greater than {length}, not {length(arg_value)}"
)

#' Assert Length Greater Than or Equal To
#'
#' Assert object length is greater than or equal to a threshold
#'
#' @inheritParams assert_length
#' @return invisible(TRUE)
#' @export
assert_length_greater_than_or_equal_to <- assert_create(
  func = function(x, length) {
    if(!is.numeric(length)) return("'length' must be numeric")
    if(length(length) != 1) return("'length' must be a single number")
    if(!is_whole_number(length)) return("'length' must be a whole number")
    if(length < 0) return("'length' must be non-negative")
    
    length(x) >= length
  },
  default_error_msg = "'{arg_name}' must have length greater than or equal to {length}, not {length(arg_value)}"
)

#' Assert Length Less Than
#'
#' Assert object length is less than a threshold
#'
#' @inheritParams assert_length
#' @return invisible(TRUE)
#' @export
assert_length_less_than <- assert_create(
  func = function(x, length) {
    if(!is.numeric(length)) return("'length' must be numeric")
    if(length(length) != 1) return("'length' must be a single number")
    if(!is_whole_number(length)) return("'length' must be a whole number")
    if(length < 0) return("'length' must be non-negative")
    
    length(x) < length
  },
  default_error_msg = "'{arg_name}' must have length less than {length}, not {length(arg_value)}"
)

#' Assert Length Less Than or Equal To
#'
#' Assert object length is less than or equal to a threshold
#'
#' @inheritParams assert_length
#' @return invisible(TRUE)
#' @export
assert_length_less_than_or_equal_to <- assert_create(
  func = function(x, length) {
    if(!is.numeric(length)) return("'length' must be numeric")
    if(length(length) != 1) return("'length' must be a single number")
    if(!is_whole_number(length)) return("'length' must be a whole number")
    if(length < 0) return("'length' must be non-negative")
    
    length(x) <= length
  },
  default_error_msg = "'{arg_name}' must have length less than or equal to {length}, not {length(arg_value)}"
)
