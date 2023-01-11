
#' Parse a tibble and ensure it meets standards
#'
#' @inheritParams gg1d_plot
#'
#' @return tibble with the following columns:
#' 1) colnames
#' 2) coltype (categorical/numeric/tooltip/invalid)
#' 3) ndistinct (number of distinct values)
#' 4) plottable (should this column be plotted)
#'
#' @examples
#' path_gg1d <- system.file('testdata/testinput.csv', package = 'gg1d')
#' df <- read.csv(path_gg1d, header = TRUE)
#' gg1d:::column_info_table(df)
column_info_table <- function(.data, maxlevels = 6, col_id){
  # Assertions
  assertions::assert_string(col_id)
  assertions::assert_names_include(.data, col_id)

  # Create Column Info Data
  df_column_info <- data.frame(
    colnames = colnames(.data),
    coltype = coltypes(.data, col_id),
    ndistinct = colvalues(.data)
  )
  #df_column_info <- dplyr::as_tibble(df_column_info)


  # Warn if unknown file type in table
  if(c("invalid") %in% df_column_info[["coltype"]]) cli::cli_warn('The following columns will not be plotted due to invalid column types: {df_column_info$colnames[df_column_info$coltype=="invalid"]}')

  # Warn if too many levels in categorical variable
  lgl_too_many_levels <- df_column_info$coltype == "categorical" & df_column_info$ndistinct > maxlevels
  df_column_info[["plottable"]] <- !lgl_too_many_levels & !df_column_info$coltype %in% c("invalid", "id", "tooltip")

  if(sum(lgl_too_many_levels) > 0){
    char_cols_with_too_many_levels <- df_column_info$colnames[lgl_too_many_levels]
    char_cols_with_too_many_levels <- paste0(char_cols_with_too_many_levels, ' (', df_column_info$ndistinct[lgl_too_many_levels], ")")
    cli::cli_alert_warning('{.strong Categorical columns} must have {.strong <= {maxlevels} unique values} to be visualised. Columns with too many unique values: {.strong {char_cols_with_too_many_levels}}')
  }

  # Return table describing
  return(df_column_info)
}



coltypes <- function(.data, col_id){

  # First Pass of coltypes
  char_coltypes <- vapply(.data, FUN = function(vec){
    if(is.character(vec) | is.factor(vec) | is.logical(vec)) return('categorical')
    else if(is.numeric(vec)) return('numeric')
    else return('invalid')
  }, FUN.VALUE = character(1))

  # Overwrite coltype to tooltip if `_tooltip` suffix is found
  char_colnames <- colnames(.data)
  tooltip_suffix = "_tooltip"
  has_tooltip_in_name <- grepl(x = char_colnames, pattern = tooltip_suffix, ignore.case = TRUE)
  without_tooltip_has_matched_name <- gsub(x = char_colnames, pattern = tooltip_suffix, replacement = "") %in% char_colnames
  is_tooltip_col <- has_tooltip_in_name & without_tooltip_has_matched_name

  char_coltypes <- ifelse(is_tooltip_col, yes = "tooltip", no = char_coltypes)

  # Overwrite coltype to 'id' if colname == col_id
  char_coltypes <- ifelse(char_colnames == col_id, yes = "id", no = char_coltypes)

  return(char_coltypes)
}

colvalues <- function(.data){
  vapply(.data, FUN = function(vec){
    length(unique(vec))
  }, FUN.VALUE = numeric(1))
}


#' AutoPlot an entire data.frame
#'
#' Visualize all columns in a data frame with gg1d's vertically aligned plots
#' and automatic plot selection based on variable type.
#' Plots are fully interactive, and custom tooltips can be added.
#'
#' @param .data data.frame to autoplot (data.frame)
#' @param maxlevels for categorical variables, what is the maximum number of distinct values to allow (too many will make it hard to find a palette that suits). (number)
#' @param verbose verbosity level
#' @param col_id name of column to use for
#' @param col_sort_by name of column to sort on
#' @return ggiraph interactive visualisation
#'
#' @examples
#' path_gg1d <- system.file('testdata/testinput.csv', package = 'gg1d')
#' df <- read.csv(path_gg1d, header = TRUE)
#' gg1d_plot(df)
#'
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_tile theme %+replace% element_blank element_text
#' @export
#'
gg1d_plot <- function(.data, col_id = NULL, col_sort_by = character(0), maxlevels = 6, verbose = TRUE, debug_return_col_info = FALSE){

  # Assertions --------------------------------------------------------------
  assertions::assert_dataframe(.data)
  assertions::assert_number(maxlevels)


  # Preprocessing -----------------------------------------------------------
  # Add col_id column if it user hasn't supplied one
  if(is.null(col_id)){
    col_id <- "DefaultID"
    .data[[col_id]] <- seq_len(nrow(.data))
  }
  else{
    assertions::assert_string(col_id)
    assertions::assert_names_include(.data, names = col_id)
  }

  df_col_info <- column_info_table(.data, maxlevels = maxlevels, col_id = col_id)

  # If debugging, return df_col_info
  if(debug_return_col_info)
    return(df_col_info)

  # Plot --------------------------------------------------------------------
  plottable_cols <- sum(.data$plottable==TRUE)

  gglist <-  vector(mode = "list", length = plottable_cols)
  for (i in seq_len(nrow(df_col_info))){
    colname <- df_col_info[["colnames"]][i]
    coltype <- df_col_info[["coltype"]][i]
    ndistinct <- df_col_info[["ndistinct"]][i]
    plottable <- df_col_info[["plottable"]][i]


    # Don't plot if not plottable
    if(!plottable){
      if(verbose) cli::cli_alert_info("Skipping column {.strong {colname}} since not plottable")
      next()
    }
    else{
      if(verbose) cli::cli_alert_info("Plotting column {.strong {colname}}")
    }

    # convert ID col to factor if not already then sort levels based on 'col_sort'

    # Draw the actual plot
    if(coltype == "categorical"){
      gg <- ggplot(.data, aes(x = .data[[col_id]], y = "", fill = .data[[colname]])) +
        geom_tile() +
        ggplot2::ylab(colname) +
        theme_categorical()
    }
    else if(coltype == "numeric"){
      gg <- ggplot2::ggplot(.data, aes(x = .data[[col_id]], y = .data[[colname]])) +
        geom_col() +
        theme_numeric()
    }
    else{
     cli::cli_abort("Unsure how to plot coltype: {coltype}")
    }
    gglist[[colname]] <- gg
  }

  ggpatch <- patchwork::wrap_plots(gglist, ncol = 1)

  # Return -----------------------------------------------------------
  return(ggpatch)

}

theme_categorical <- function(){
  ggplot2::theme_minimal() %+replace%

    theme(
      panel.grid = element_blank(),
      axis.text.y.left = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(angle = 0),
      legend.key.size = ggplot2::unit(0.3,"line"),
      legend.justification = "left"
    )
}

theme_numeric <- function(){
  ggplot2::theme_minimal() %+replace%

    theme(
      panel.grid = element_blank(),
      axis.text.y.left = element_blank(),
      axis.title.y.left = element_text(angle = 0, vjust = 0.5),
      axis.text.x = element_blank(),
      axis.title.x = element_blank()
    )
}
