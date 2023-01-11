
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
#'
column_info_table <- function(.data, maxlevels = 6, col_id = NULL, cols_to_plot) {
  # Assertions
  assertions::assert_string(col_id)
  assertions::assert_names_include(.data, col_id)

  # Create Column Info Data
  df_column_info <- data.frame(
    colnames = colnames(.data),
    coltype = coltypes(.data, col_id),
    ndistinct = colvalues(.data)
  )
  # df_column_info <- dplyr::as_tibble(df_column_info)


  # Warn if unknown file type in table
  if (c("invalid") %in% df_column_info[["coltype"]]) cli::cli_warn('The following columns will not be plotted due to invalid column types: {df_column_info$colnames[df_column_info$coltype=="invalid"]}')

  # Mark columns as not plottable if
  # 1) they are a categorical variable with more than `maxlevels` distinct values
  # 2) Their coltype is 'invalid', 'id', or 'tooltip'
  # 3) The`cols_to_plot` variable is TRUE and column names are NOT in the list of cols_to_plot
  lgl_too_many_levels <- df_column_info$coltype == "categorical" & df_column_info$ndistinct > maxlevels
  df_column_info[["plottable"]] <- !lgl_too_many_levels & !df_column_info$coltype %in% c("invalid", "id", "tooltip") & (is.null(cols_to_plot) | df_column_info$colnames %in% c(cols_to_plot))

  if (sum(lgl_too_many_levels) > 0) {
    char_cols_with_too_many_levels <- df_column_info$colnames[lgl_too_many_levels]
    char_cols_with_too_many_levels <- paste0(char_cols_with_too_many_levels, " (", df_column_info$ndistinct[lgl_too_many_levels], ")")
    cli::cli_alert_warning("{.strong Categorical columns} must have {.strong <= {maxlevels} unique values} to be visualised. Columns with too many unique values: {.strong {char_cols_with_too_many_levels}}")
  }

  # Return table describing
  return(df_column_info)
}



coltypes <- function(.data, col_id) {
  # First Pass of coltypes
  char_coltypes <- vapply(.data, FUN = function(vec) {
    if (is.character(vec) | is.factor(vec) | is.logical(vec)) {
      return("categorical")
    } else if (is.numeric(vec)) {
      return("numeric")
    } else {
      return("invalid")
    }
  }, FUN.VALUE = character(1))

  # Overwrite coltype to tooltip if `_tooltip` suffix is found
  char_colnames <- colnames(.data)
  tooltip_suffix <- "_tooltip"
  has_tooltip_in_name <- grepl(x = char_colnames, pattern = tooltip_suffix, ignore.case = TRUE)
  without_tooltip_has_matched_name <- gsub(x = char_colnames, pattern = tooltip_suffix, replacement = "") %in% char_colnames
  is_tooltip_col <- has_tooltip_in_name & without_tooltip_has_matched_name

  char_coltypes <- ifelse(is_tooltip_col, yes = "tooltip", no = char_coltypes)

  # Overwrite coltype to 'id' if colname == col_id
  char_coltypes <- ifelse(char_colnames == col_id, yes = "id", no = char_coltypes)

  return(char_coltypes)
}

colvalues <- function(.data) {
  vapply(.data, FUN = function(vec) {
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
#' @param col_sort name of column to sort on
#' @param drop_unused_id_levels if col_id is a factor with unused levels, should these be dropped or included in visualisation
#' @param interactive produce interactive ggiraph visualiastion (flag)
#' @param debug_return_col_info return column info instead of plots. Helpful when debugging (logical)
#' @param limit_plots throw an error when there are > 15 plottable columns in table (logical)
#' @param cols_to_plot names of columns in `.data` that should be plotted. By default plots all valid columns (character)
#' @param sort_type controls how categorical variables are sorted.
#' Numerical variables are always sorted in numerical order irrespective of the value given here.
#' Options are `alphabetical` or `frequency`
#' @param desc sort in descending order (flag)
#' @param width controls how much space is present between bars and tiles within each plot. Can be 0-1 where values of 1 makes bars/tiles take up 100% of available space (no gaps between bars)
#' @param col_sort column to sort sample order by. By default uses the supplied order of levels in col_id (order of appearance if a character type)
#'
#' @return ggiraph interactive visualisation
#'
#' @examples
#' path_gg1d <- system.file("testdata/testinput.csv", package = "gg1d")
#' df <- read.csv(path_gg1d, header = TRUE)
#' gg1d_plot(df, col_id = "ID", col_sort = "Glasses")
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_tile theme %+replace% element_blank element_text element_line
#' @export
#'
gg1d_plot <- function(.data, col_id = NULL, col_sort = NULL, maxlevels = 6, verbose = TRUE, drop_unused_id_levels = FALSE, interactive = TRUE, debug_return_col_info = FALSE, limit_plots = TRUE, cols_to_plot = NULL, sort_type = c("frequency", "alphabetical"), desc = TRUE, width = 0.9) {

  # Assertions --------------------------------------------------------------
  assertions::assert_dataframe(.data)
  assertions::assert_number(maxlevels)
  assertions::assert_flag(drop_unused_id_levels)
  assertions::assert_flag(drop_unused_id_levels)
  assertions::assert_flag(interactive)
  assertions::assert_flag(limit_plots)
  assertions::assert_flag(desc)
  assertions::assert_flag(verbose)
  if(!is.null(cols_to_plot)) assertions::assert_names_include(.data, names = cols_to_plot)
  sort_type <- rlang::arg_match(sort_type)

  # Configuration -----------------------------------------------------------
  max_plottable_cols <- 15


  # Formatting --------------------------------------------------------------
  cli::cli_div(theme = list(span.warn = list(color = "yellow", "font-weight" = "bold")))
  cli::cli_div(theme = list(span.success = list(color = "darkgreen", "font-weight" = "bold")))


  # Preprocessing -----------------------------------------------------------
  # Add col_id column if it user hasn't supplied one
  if (is.null(col_id)) {
    col_id <- "DefaultID"
    .data[[col_id]] <- seq_len(nrow(.data))
  } else {
    assertions::assert_string(col_id)
    assertions::assert_names_include(.data, names = col_id)
  }


  # Identify Plottable Columns Columns ------------------------------------------------------------
  df_col_info <- column_info_table(.data, maxlevels = maxlevels, col_id = col_id, cols_to_plot = cols_to_plot)

  # If debugging, return df_col_info
  if (debug_return_col_info) {
    return(df_col_info)
  }


  # Sort Order -------------------------------------------------------------------
  # convert ID col to factor if not already
  if (!is.factor(.data[[col_id]])) {
    .data[[col_id]] <- as.factor(.data[[col_id]])
  }

  if (verbose) cli::cli_h1("Sorting")

  if (is.null(col_sort)) {
    if (verbose) cli::cli_alert_info("Sorting X axis by: Order of appearance")
  } else {
    assertions::assert_string(col_sort)
    assertions::assert_names_include(.data, names = col_sort)

    cli::cli_bullets(c(
      "*" = "Sorting X axis by: {.strong {col_sort}}",
      "*" = "Order type: {.strong {sort_type}}",
      "*" = "Sort order: {.strong {ifelse(desc, 'descending', 'ascending')}}"
    ))
    .data[[col_id]] <- forcats::fct_reorder(.data[[col_id]], smartrank(.data[[col_sort]], sort_by = sort_type, desc = desc))
  }




  # Plot --------------------------------------------------------------------
  if (verbose) cli::cli_h1("Generating Plot")
  plottable_cols <- sum(df_col_info$plottable == TRUE)

  if (verbose) {
    cli::cli_alert_info("Found {.strong {plottable_cols}} plottable columns in {.strong .data}")
  }

  # Make sure theres not too many plottable cols
  if (limit_plots && plottable_cols > max_plottable_cols) {
    cli::cli_abort("Autoplotting > 15 fields by `gg1d_plot` is not recommended (visualisation ends up very squished). If you're certain you want to proceed, set limit_plots = `FALSE`. Alternatively, use `cols_to_plot` to specify <=15 columns within your dataset.")
  }
  gglist <- lapply(
    X = seq_len(nrow(df_col_info)),
    function(i) {
      colname <- df_col_info[["colnames"]][i]
      coltype <- df_col_info[["coltype"]][i]
      ndistinct <- df_col_info[["ndistinct"]][i]
      plottable <- df_col_info[["plottable"]][i]


      # Don't plot if not plottable
      if (!plottable) {
        if (verbose) cli::cli_alert_warning("{.warn Skipping} column {.strong {colname}}")
        return(NULL)
      } else {
        if (verbose) cli::cli_alert_success("{.success Plotting} column {.strong {colname}}")
      }

      # Draw the actual plot
      if (coltype == "categorical") {
        gg <- ggplot(.data, aes(x = .data[[col_id]], y = "", fill = .data[[colname]])) +
          geom_tile(width = width) +
          ggplot2::scale_x_discrete(drop = drop_unused_id_levels) +
          ggplot2::ylab(colname) +
          theme_categorical()
      } else if (coltype == "numeric") {
        gg <- ggplot2::ggplot(.data, aes(x = .data[[col_id]], y = .data[[colname]])) +
          geom_col(width = width) +
          ggplot2::scale_x_discrete(drop = drop_unused_id_levels) +
          ggplot2::scale_y_continuous(breaks = sensible_2_breaks(.data[[colname]])) +
          ggplot2::ylab(colname) +
          theme_numeric()
      } else {
        cli::cli_abort("Unsure how to plot coltype: {coltype}")
      }
      return(gg)
    }
  )
  names(gglist) <- df_col_info[["colnames"]]

  # Remove null columns
  gglist <- gglist[!vapply(gglist, is.null, logical(1))]

  # Align plots vertically
  if (verbose) cli::cli_alert_info("Stacking plots vertically")
  ggpatch <- patchwork::wrap_plots(gglist, ncol = 1)

  # Return -----------------------------------------------------------
  return(ggpatch)
}

theme_categorical <- function() {
  ggplot2::theme_minimal() %+replace%

    theme(
      panel.grid = element_blank(),
      axis.text.y.left = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(angle = 0),
      legend.key.size = ggplot2::unit(0.3, "line"),
      legend.justification = "left"
    )
}

theme_numeric <- function() {
  ggplot2::theme_minimal() %+replace%

    theme(
      panel.grid = element_blank(),
      axis.title.y.left = element_text(angle = 0, vjust = 0.5),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.line.y = element_line(linewidth = 0.3),
      axis.text.y.left = element_text(size = 8),
      axis.ticks.y = element_line()
    )
}


smartrank <- function(x, sort_by = c("alphabetical", "frequency"), desc = TRUE) {
  sort_by <- rlang::arg_match(sort_by)

  if (is.numeric(x)) {
    if(desc)
      ranking <- rank(-x)
    else
      ranking <- rank(x)
    return(ranking)
  }
  else if (is.character(x) || is.factor(x) || is.logical(x)) {
    if (sort_by == "alphabetical") {
      return(rank(x, ties.method = "min"))
    } else if (sort_by == "frequency") {

      x = as.character(x)

      # Create a table of the frequencies of each element
      freq_table <- as.data.frame(table(x))

      # Add an index column to the table
      freq_table$idx <- 1:nrow(freq_table)

      # Sort the table by the frequencies in descending order, and by alphabetic order then by original index in case of ties
      if(desc)
        freq  <-  -freq_table$Freq
      else
        freq <- freq_table$Freq

      freq_table <- freq_table[order(freq, freq_table$x, freq_table$idx),]

      # Get the original vector elements in the sorted order
      return(match(x, freq_table$x))
    }
    else
      cli::cli_abort("Author has forgotten to code a responce when sort_by == {sort_by}")
  }
  else {
    cli::cli_abort("Input must be a numeric, character, or factor vector.")
  }
}

smartsort <- function(x, sort_categorical_by = c("alphabetical", "frequency"), desc = TRUE){
  sort_categorical_by <- rlang::arg_match(sort_categorical_by)

  if (is.numeric(x)) {
    return(sort(x, decreasing = desc))
  }
  else if (is.character(x) || is.factor(x)) {
    if (sort_categorical_by == "alphabetical") {
      sort(x)
    } else if (sort_categorical_by == "frequency") {
      return(sort_by_frequency(x))
      #x_counts <- freq_table[match(x, names(freq_table))]
    }
    else
      cli::cli_abort("Author has forgotten to code a responce when sort_by == {sort_by}")
  }
  else {
    cli::cli_abort("Input must be a numeric, character, or factor vector.")
  }
}


# multi_rank_with_tiebreakers <- function(first, ..., ties.method = "average") {
#   # create a matrix of input vectors
#   mat <- cbind(first, ...)
#   base_ranks <- rank(mat[,1], ties.method = ties.method)
#
#   for (i in seq_len(ncol(mat))) {
#     # Reorder the values for which the base_ranks are the same
#     ties <- mat[which(duplicated(base_ranks) | duplicated(base_ranks, fromLast = TRUE)), i]
#     tie_ranks <- rank(ties, ties.method = ties.method)
#     # Replace the previous ranks by the tiebreakers
#     base_ranks[which(duplicated(base_ranks) | duplicated(base_ranks, fromLast = TRUE))] <- tie_ranks
#   }
#   return(base_ranks)
# }

multi_rank <- function(...){
  #browser()
  vectors <- list(...)
  rank_order <- rank(vectors[[1]], ties.method = "first")
  for(i in 2:length(vectors)){
    ties <- which(rank_order == rank_order[vectors[[i]]])
    rank_order[ties] <- rank(vectors[[i]][ties], ties.method = "first")
  }
  return(rank_order)
}

sort_by_frequency <- function(x) {
  # Create a table of the frequencies of each element
  freq_table <- as.data.frame(table(x))

  # Add an index column to the table
  freq_table$idx <- 1:nrow(freq_table)

  # Sort the table by the frequencies in descending order, and by alphabetic order then by original index in case of ties
  freq_table <- freq_table[order(-freq_table$Freq, freq_table$x, freq_table$idx),]


  # Get the original vector elements in the sorted order
  sorted_x <- x[order(match(x, freq_table$x))]

  return(sorted_x)
}


sensible_2_breaks <- function(vector){
  upper <- max(vector)
  lower <- min(0, min(vector))
  c(upper, lower)
}



