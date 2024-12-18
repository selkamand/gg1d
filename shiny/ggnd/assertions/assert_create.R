#' Create an assertion function
#'
#' This function creates an assertion function that can be used to check the
#' validity of an input.
#' All assertions provided with this package are created using either [assert_create()] or [assert_create_chain()]
#'
#' @param func A function defining the assertion criteria. This function should
#'   return a logical value (`TRUE` when assertion is passed or `FALSE` when it fails).
#'   Alternatively, instead of returning FALSE, you can return a string which will act as the error message.
#'   In this latter case, you don't need to supply a `default_error_msg`
#' @param default_error_msg A character string providing an error message in case
#' the assertion fails. Must be supplied if function `func` returns `FALSE` when assertion fails (as opposed to a string)
#' Can include the following special terms
#`
#' 1. `{arg_name}` to refer to the name of the variable supplied to the assertion.
#'
#' 2. `{arg_value}` to refer to the value of the variable supplied to the assertion
#'
#' 3. `{code_to_evaluate}` to evaluate the code within the error message.
#' Replace `code_to_evaluate` with your code
#'
#' 4. `{.strong bold_text}` to perform inline formatting. Replace `bold_text` with your text.
#' See [cli documentation](https://cli.r-lib.org/reference/inline-markup.html) for details
#'
#'
#' @return An assertion function.
#'
#' @examples
#' #' # Create an assertion function that checks that a character string is all
#' # lower case
#' assert_character <- assert_create(
#'   is.character,
#'   "{arg_name} must be a character vector, not a {class(arg_value)}"
#' )
#'
#' # Use the assertion function
#' try({
#' is_lower("hello") # Returns invisible TRUE
#' is_lower("Hello") # Aborts the function with the error message
#'})
#' @include utils.R
#' @concept assert_create
#' @export
assert_create <- function(func, default_error_msg = NULL){

  # Check arguments
  function_name <- paste0(deparse(substitute(func)), collapse = "")


  # Ensure func is a valid function
  if(!is.function(func)){
    cli::cli_abort("`{function_name}` must be a function, not a {class(func)}")
  }

  # Ensure func has at least 1 argument
  if(func_arg_count(func, dots = "count_as_0") == 0){
    if (func_supports_variable_arguments(func))
      additional_note = " (Note '...' does NOT count as an argument)"
    else additional_note = ""

    cli::cli_abort("`{function_name}` must have at least 1 paramater to be used in `assert_create`{additional_note}")
  }

  # Ensure default_error_msg is a string
  if(!is.null(default_error_msg) & !is_string(default_error_msg)){
    default_error_msg_deparsed <- deparse(substitute(default_error_msg))
    cli::cli_abort("{default_error_msg_deparsed} must be a string (length 1 character vector). Class: {class(default_error_msg)}; Length: {length(default_error_msg)}")
  }

  # Get arguments of user supplied function
  args <- func_args_as_pairlist(func)

  # Assert that function has no arguments named 'msg' or 'call', 'arg_name', since we need to add our own
  if(any(c('msg', 'call', 'arg_name') %in% names(args))){
    cli::cli_abort("Function supplied to `func` argument of `assert_create` cannot include paramaters named 'msg', 'call', or 'arg_name', since assert_create adds these arguments to every assertion")
  }

  # Change add 'msg', 'call' and 'arg_name' arguments at the end
  #args <- append(args, as.pairlist(alist(x = )), after = 0)
  args <- append(args, as.pairlist(alist(msg = NULL, call = rlang::caller_env(), arg_name = NULL)),after = Inf)


  # Create body of assertion function
  body = quote({

    # Check mandatory arguments are all supplied
    if(required_args_are_missing())
      cli::cli_abort('mandatory argument/s were not supplied')

    # Setup some variables ( these will be useful later)
    if(is.null(arg_name))
      arg_name <- deparse(match.call()[[2]])
    else if(!is_string(arg_name))
     cli::cli_abort("{.strong arg_name} must be a string, not a {class(arg_name)}")

    # Create arg_value val
    arg_value <- eval(match.call()[[2]], envir = call)

    # Create useful functions
    #.name <- function(obj) { browser(); deparse(substitute(obj, env)) }


    # Get the list of arguments with values explicitly supplied in function call
    explicit_args <- as.list(match.call())[-1]

    # Assert all arguments without defaults are explicitly supplied in the function call
    arg_has_default <- !unlist(as.list(formals()) == substitute())
    args_without_defaults <- names(arg_has_default)[arg_has_default == FALSE]
    args_missing <- args_without_defaults[!args_without_defaults %in% names(explicit_args)]
    if(length(args_missing) > 0){
      cli::cli_abort("{args_missing} are required, with no default")
    }

    # Create list of arguments - value pairs to be called with func (using do.call)
    # Lets just supply x in position 1 (unnamed), then pass whatevers supplied by arg_has_default with its original names
    names(explicit_args)[1] <- "" # unname first argument

    # Also filter out msg & call arguments the user supplied func won't understand these
    explicit_args_for_func <- explicit_args[!names(explicit_args) %in% c("msg", "call", "arg_name")]


    # Info
    #cli::cli_alert_info("Calling `func` using the argument values: {paste0(names(explicit_args_for_func), ' = ', explicit_args_for_func)}")

    # Call supplied function
    condition <- do.call(func, args = explicit_args_for_func, envir = parent.frame())

    if(!(is.logical(condition) || is.character(condition)) || length(condition) != 1) # Change to is.flag once this method is created
      cli::cli_abort("Assertion Function `{.strong {function_name}}` must return TRUE if assertion passes and FALSE or a String if assertion should fail. Instead returned: `{condition}`")

    if(isFALSE(condition) & is.null(default_error_msg))
      cli::cli_abort("Assertion Function `{.strong {function_name}}` returned FALSE, indicating assertion should fail, however no {.arg default_error_msg} was supplied! Please add a {.arg default_error_msg} to your assert_create call, or change function to return a string describing the error instead of `FALSE`")

    # If user doesn't supply an error message, set msg to the default
    if(is.null(msg)){
      msg_evaluation_environment <- rlang::current_env() # Evaluate error message in current envrionment
     if(isFALSE(condition))
      msg <- default_error_msg

     else if(is.character(condition))
      msg <- condition
    }
    else { # If user does supply a default error message
      msg_evaluation_environment <- rlang::caller_env(n = 1) # Evaluate it in caller environment
      # Add special keywords to environment:
      # arg_name, arv_value
      assign("arg_name", value = arg_name, envir = msg_evaluation_environment)
      assign("arg_value", value = arg_value, envir = msg_evaluation_environment)
    }

    # If assertion fails, abort with error statement
    if(is.character(condition) || !condition )
      cli::cli_abort(msg, call = call, .envir = msg_evaluation_environment)
    else
      return(invisible(TRUE))
  })

  # Create assertion_function
  assertion_function <- rlang::new_function(args, body, env = rlang::env(func = func))

  return(assertion_function)
}



#' @title Create Chains of Assertions
#'
#' @description
#' Combine multiple assertion functions created by `assert_create()` into a single assertion function with diverse failure modes and error messages.
#'
#' @param ... assertion functions created by `assert_create()`.
#'
#' @return A single assertion function that calls each of the input functions in the order they are supplied.
#'
#'
#' @examples
#' # Create an assertion function that checks for both positive integers and even values
#' assert_string <- assert_create_chain(
#'   assert_create(is.character, '{{arg_name}} must be a character'),
#'   assert_create(function(x){{ length(x)==1 }}, '{{arg_name}} must be length 1')
#'   )
#'
#' # Use the assertion function to check a valid value
#' assert_string("String")
#'
#' # Use the assertion function to check an invalid value
#' try({
#' assert_string(3)
#' # Output: Error: '3' must be a character
#' })
#' @export
assert_create_chain <- function(...){

  # Get list of arguments
  dot_args <- list(...)

  # Check dot_args are all functions
  lgl_is_function <- vapply(dot_args, is.function, FUN.VALUE = logical(1))
  if(!all(lgl_is_function)){
    cli::cli_abort(
      c("Input to {.strong assert_create_chain} must must be {.strong functions} created by {.strong `assert_create()`}",
        "",
        assert_create_chain_example()
      ))
  }

  # Check functions all have the required arguments (msg, call and arg_name)
  if(!all(vapply(dot_args, function(f){ all(c('msg', 'call', 'arg_name') %in% func_arg_names(f)) }, FUN.VALUE = logical(1)))){
    cli::cli_abort(
      c("Input to {.strong assert_create_chain} must must be {.strong functions} created by {.strong `assert_create()`}",
        "",
        assert_create_chain_example()
      ))
  }

  # Check functions have at least 4 args (some_obj_to_test and officially required functions: msg, call, arg_name)
  if(!all(vapply(dot_args, function(f){ func_arg_count(f) >= 4 }, FUN.VALUE = logical(1)))){
    cli::cli_abort(
      c("Input to {.strong assert_create_chain} must must be {.strong functions} created by {.strong `assert_create()`}",
        "",
        assert_create_chain_example()
      ))
  }

  # Save assertion functons in a list
  assertion_functions <- dot_args

  # Get arguments of each assertion function
  param_pairlist_nested <- lapply(assertion_functions, func_args_as_pairlist)
  param_pairlist_flat <- unlist(param_pairlist_nested, recursive = FALSE)
  param_pairlist <- param_pairlist_flat[!duplicated(names(param_pairlist_flat))]

  # reorder arguments so that arguments to functions take precedence
  args_to_put_at_end <- c("msg", "call", "arg_name")
  param_pairlist <- c(param_pairlist[!(names(param_pairlist) %in% args_to_put_at_end)], param_pairlist[names(param_pairlist) %in% args_to_put_at_end])



  #unique_params <- unlist(unique(arglist))

  # Return a wrapper function that calls each of the functions
  chained_assertion_function <- rlang::new_function(
    args = param_pairlist,
    body = quote({


      arg_name_upperlevel <- deparse(substitute(x))

      explicit_args <- as.list(match.call())[-1]
      explicit_args <- lapply(explicit_args, function(a) {
        if(is.symbol(a))
          eval.parent(a, n = 3)
        else return(a)
      })

      # Assert all arguments without defaults are explicitly supplied in the function call
      arg_has_default <- !unlist(as.list(formals()) == substitute())
      args_without_defaults <- names(arg_has_default)[arg_has_default == FALSE]
      args_missing <- args_without_defaults[!args_without_defaults %in% names(explicit_args)]
      if(length(args_missing) > 0){
        cli::cli_abort("{args_missing} are required, with no default")
      }


      # If user doesn't override `explicit_args`, set to variable name
      # This lets downstream functions use the correct variable name as {arg_name}
      if(!"arg_name" %in% names(explicit_args)){
        explicit_args <- append(explicit_args, list(arg_name = arg_name_upperlevel), after = Inf)
      }

      # Do a simliar thing with call, since if you just let the assertions use the default call, it will be the assertion functions parent env, not the caller
      if(! "call" %in% names(explicit_args)){
        explicit_args <- append(explicit_args, list(call = call), after = Inf)
      }

      for (f in assertion_functions) {
        # filter explicit_args for only the functions args that 'f' understands.
        args_relevant_to_function_f <- explicit_args[names(explicit_args) %in% func_arg_names(f)]
        do.call(what = f, args = args_relevant_to_function_f, envir = rlang::caller_env())
      }
      return(invisible(TRUE))
      }),
    env = rlang::env(assertion_functions = assertion_functions)
  )
  return(chained_assertion_function)
}


assert_create_chain_example <- function(){
  c(
  "{.strong For example:}",
  "assert_string <- assert_create_chain(",
  ' ' = "assert_create(is.character, '{{arg_name}} must be a character'),",
  ' ' = "assert_create(function(s){{ length(s)==1 }}, '{{arg_name}} must be length 1')",
  ")"
)
}
