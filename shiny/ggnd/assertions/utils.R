
#' Common Parameter Descriptions
#'
#' @param call Only relevant when pooling assertions into multi-assertion helper functions. See \link[cli]{cli_abort} for details.
#' @param arg_name Advanced use only. Name of the argument passed (default: NULL, will automatically extract arg_name).
#' @param msg The error message thrown if the assertion fails (string)
#' @param ... Used to pass any arguments to assertion function
#'
#'
common_roxygen_params <- function(call, arg_name, msg, ...){

}


# Cli formatting helpers --------------------------------------------------

#' Preprocess character vectors for cli::cli_abort()
#'
#' The \code{format_as_bullets} function is used for preprocessing character vectors by adding names.
#' These names are used to denote bullet points when the character vector is passed to \code{cli::cli_abort()}.
#' This allows for the easy creation of bullet point lists in error messages.
#' The bullet argument allows the user to specify the desired bullet point symbol.
#' The default bullet point symbols are: *, >, , x, v, i, and !.
#'
#' @param x A list of character strings
#' @param bullet One of '', '>', ' ', 'x', 'v', 'i', '!' (default: '')
#' The character to use as the bullet point for each element of x.
#'
#' @return A character string with each element of x formatted as a bullet point
#'
format_as_bullets <- function(x, bullet = c('*', '>', ' ', 'x', 'v', 'i', '!')){
  bullet <- rlang::arg_match(bullet)
  names(x) <- rep(bullet, times = length(x))
  return(x)
}


#' Preprocess character vectors for cli package functions
#'
#' @param x A character vector
#' @param inline_tag A character vector of inline tag names (e.g. "strong", "emph", "code", "arg")
#'
#' @return A character vector with inline tags applied to each element
#'
#'
format_inline <- function(x, inline_tag = c('strong', 'emph', 'code', 'arg')){
  inline_tag <- rlang::arg_match(inline_tag)
  x <- paste0('{.', inline_tag, ' ', x, '}')
  return(x)
}

format_escape_curly <- function(x){
  x <- gsub(x = x, pattern = "{", replacement = "{{", fixed = TRUE)
  x <- gsub(x = x, pattern = "}", replacement = "}}", fixed = TRUE)
}


# Function assertions ---------------------------------------------------------------
func_arg_names <- function(func){
  names(formals(args(func)))
}

func_supports_variable_arguments <- function(func){
  arg_names <- func_arg_names(func)
  any(grepl(x = arg_names, pattern = "...", fixed = TRUE))
}

func_args_as_pairlist <- function(func){
  formals(args(func))
}
#
# func_args_as_alist <- function(func){
#   a= unlist(func_args_as_pairlist(func))
# }

# func_arg_remove_defaults <- function(func, n){
#   #foo <- as.pairlist(alist(foo=)) ; names(foo) <- names(formals(f))[1]; formals(f)[1] <- foo; f
#   formals(func)[[1]] <- substitute()
#   return(func)
# }


func_arg_count <- function(func, dots = c("throw_error", "count_as_0", "count_as_1", "count_as_inf")){

  dots <- rlang::arg_match(dots)

  param_names <- func_arg_names(func)
  param_count <- length(param_names)

  supports_varargs <-  func_supports_variable_arguments(func)

  if(supports_varargs){
    if (dots == "throw_error") { cli::cli_abort("Cannot count number of arguments if there are dots (...) present. Can explicitly set how we should deal with this problem via the dots argument") }
    else if (dots == "count_as_0") { param_count <- param_count - 1 }
    else if (dots == "count_as_1") { param_count <- param_count }
    else if (dots == "count_as_inf") { param_count <- Inf }

  }

  return(param_count)
}


# func_arg_has_dots <- function(func){
#   any(func_arg_names(func) == "...")
# }
#
# func_arg_default_status <- function(func){
#   args <- func_args_as_pairlist(func)
#   lgl <- unlist(args) == substitute()
#   return(lgl)
# }

required_args_are_missing <- function(fun = sys.function(-1), ncall = 3) {
  f_args <- formals(fun)
  f_args <- f_args[vapply(f_args, is.symbol, FUN.VALUE = TRUE)]
  f_args <- names(f_args)
  f_args <- setdiff(f_args, "...")
  test <- vapply(f_args,
                 function(x) methods::missingArg(as.name(x), envir = parent.frame(ncall), eval = TRUE),
                 FUN.VALUE = TRUE)
  return(any(test))
}
