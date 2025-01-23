#' Format error messages as html
#'
#' @description Format error messages as html
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
err2html <- function(err){
  tags$code(HTML(cli::ansi_html(as.character(err))))
}


if_not_numeric_return_default <- function(x, default){
 if(!is.numeric(x))
   return(default)
  else return(x)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# If LHS is NA, NULL, or an empty string return RHS, otherwise return LHS
`%|?|%` <- function(x, y) {
  if (is.null(x))
    return(y)
  else if (length(x) > 1 || length(x) == 0 )
    return(x)
  else if (is.na(x))
    return(y)
  else if(nchar(x) == 0)
    return(y)
  else
    return(x)

}


# Returns colname if its a valid column name of dataset, otherwise returns NULL
validate_column <- function(colname, dataset){
  if(is.null(colname))
    return(NULL)
  else if(all(nzchar(colname)) & all(colname %in% colnames(dataset)))
   return(colname)
  else
    return(NULL)

}
