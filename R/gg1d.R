
#' Parse a tibble and ensure it meets standards
#'
#' @inheritParams gg1d_plot
#'
#' @return tibble with the following columns:
#' 1) colnames
#' 2) coltype (categorical/numeric/tooltip/invalid)
#' 3) ndistinct (number of distinct values)
#' 4) plottable (should this column be plotted)
#' 4) tooltip_col (the name of the column to use as the tooltip) or NA if no obvious tooltip column found
#'
#'
column_info_table <- function(.data, maxlevels = 6, col_id = NULL, cols_to_plot, tooltip_column_suffix = "_tooltip", ignore_column_regex = "_ignore$" ,palettes, colours_default, colours_default_logical, verbose) {
  # Assertions
  assertions::assert_string(col_id)
  assertions::assert_names_include(.data, col_id)

  # Create Column Info Data
  df_column_info <- data.frame(
    colnames = colnames(.data),
    coltype = coltypes(.data, col_id),
    coltooltip = coltooltip(.data, tooltip_column_suffix),
    ndistinct = colvalues(.data)
  )
  # df_column_info <- dplyr::as_tibble(df_column_info)


  # Warn if unknown file type in table
  if (c("invalid") %in% df_column_info[["coltype"]]) cli::cli_warn('The following columns will not be plotted due to invalid column types: {df_column_info$colnames[df_column_info$coltype=="invalid"]}')

  # Mark columns as not plottable if
  # 1) they are a categorical variable with more than `maxlevels` distinct values
  # 2) Their coltype is 'invalid', 'id', or 'tooltip'
  # 3) The`cols_to_plot` variable is suppplied and column names are NOT in the list of cols_to_plot
  # 4) Their colnames match the _ignore suffix
  lgl_too_many_levels <- df_column_info$coltype == "categorical" & df_column_info$ndistinct > maxlevels
  df_column_info[["plottable"]] <-
    !lgl_too_many_levels & !df_column_info$coltype %in% c("invalid", "id", "tooltip") &
    (is.null(cols_to_plot) | df_column_info$colnames %in% c(cols_to_plot)) &
    (!grepl(x=df_column_info$colnames, pattern = ignore_column_regex))



  if (sum(lgl_too_many_levels) > 0) {
    char_cols_with_too_many_levels <- df_column_info$colnames[lgl_too_many_levels]
    char_cols_with_too_many_levels <- paste0(char_cols_with_too_many_levels, " (", df_column_info$ndistinct[lgl_too_many_levels], ")")
    if(verbose) cli::cli_alert_warning("{.strong Categorical columns} must have {.strong <= {maxlevels} unique values} to be visualised. Columns with too many unique values: {.strong {char_cols_with_too_many_levels}}")
  }

  # Add palette colours
  df_column_info$palette <- choose_colours(
    .data,
    palettes = palettes,
    plottable = df_column_info$plottable,
    ndistinct = df_column_info$ndistinct,
    coltype = df_column_info$coltype,
    colours_default = colours_default,
    colours_default_logical = colours_default_logical
  )

  # Return table describing
  return(df_column_info)
}


coltooltip <- function(.data, tooltip_column_suffix){
   vapply(
     colnames(.data),
     function(name) { colnames(.data)[match(paste0(name, tooltip_column_suffix), colnames(.data))]},
     character(1)
   )

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
    length(stats::na.omit(unique(vec)))
  }, FUN.VALUE = numeric(1))
}

choose_colours <- function(data, palettes, plottable, ndistinct, coltype, colours_default, colours_default_logical){

  assertions::assert_character(colours_default)

  colors <- lapply(seq_len(ncol(data)), FUN = function(i){
    colname <- colnames(data)[[i]]
    is_plottable <- plottable[[i]]
    is_lgl <- is.logical(data[[colname]])

    if(!is_plottable | coltype[i] != "categorical"){
      return(NULL)
    }
    else if(colname %in% names(palettes)){
      colors <- unlist(palettes[[colname]])
      assertions::assert_names_include(colors, names = stats::na.omit(unique(data[[colname]])))
      return(palettes[[colname]])
    }
    else if (is_lgl){
      colors <- colours_default_logical
    }
    else{
      assertions::assert(length(colours_default) >= ndistinct[i], msg = "Too many unique values in column to assign each a colour using the default palette. Either change the default palette to one that supports colours, reduce the number of levels in this column, or exclude it from the plotting using `cols_to_plot` argument OR maxlevels")
      colors <- colours_default
    }
    })


  return(colors)

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
#' @param tooltip_column_suffix the suffix added to a column name that indicates column should be used as a tooltip (string)
#' @param relative_height_numeric how many times taller should numeric plots be relative to categorical tile plots. Only taken into account if numeric_plot_type == "bar" (number)
#' @param show_legend_titles show legend titles (flag)
#' @param show_legend show the legend (flag)
#' @param legend_position position of the legend on the plot (string, options are "right", "left", "bottom", "top")
#' @param legend_title_position position of the title of the legend on the plot (string, options are "top", "bottom", "left", "right")
#' @param legend_nrow the number of rows in the legend (number)
#' @param legend_ncol the number of columns in the legend. Set `legend_nrow = NULL` when using legend_ncol (number)
#' @param legend_title_size the size of the title of the legend (number)
#' @param legend_title_beautify beautify legend title (add spaces to snake_case / camelCase & capitalise each word) (flag)
#' @param legend_text_size the size of the text in the legend (number)
#' @param legend_key_size the size of the key in the legend (number)
#' @param palettes A list of named vectors. List names correspond to .data column names (categorical only). Vector names to levels of columns. Vector values are colours, the vector names are used to map values in .data to a colour.
#' @param colours_default default colours to use for variables. will be used to colour variables with no palette supplied.
#' @param colours_default_logical colours for binary variables (vector of 3 colors where elements represent colours of TRUE, FALSE, and NA respectively) (character)
#' @param colours_missing colour to use for values of NA (string)
#' @param na_marker what text should be added to NA values for numeric variables to indicate the value is NA, not 0 (string)
#' @param na_marker_size how large should the na_marker be (number)
#' @param vertical_spacing how large should the gap between each data row be (unit = pt) (number)
#' @param ignore_column_regex a regex string that, if matches a column name,  will cause that  column to be exclude from plotting (string)  (default: "_ignore$")
#' @param fontsize_y_text size of y axis text (number)
#' @param y_axis_position whether y axis should be on left or right side (either 'left' or 'right')
#' @param numeric_plot_type visual representation of numeric properties. One of 'bar', for bar charts, or 'heatmap' for heatmaps.
#' @param show_na_marker_categorical should a text marker of NA values (e.g. '!') be rendered on tiles with NA values (flag)
#' @param show_na_marker_heatmap should a text marker of NA values (e.g. '!') be rendered on tiles with NA values (flag)
#' @param show_values_heatmap should quantitative values be displayed on heatmap tiles (flag)
#' @param colours_heatmap_low colour of lowest value in heatmap (string)
#' @param colours_heatmap_high colour of highest value in heatmap (string)
#' @param transform_heatmap transformation to apply to values before heatmap visualisation. one of 'identity' (no transformation), 'log10', or 'log2'
#' @param fontsize_values_heatmap font size of text describing values in heatmap (number)
#' @param colours_values_heatmap colour of text describing values in heatmap (string)
#' @param legend_orientation_heatmap should legend orientation be "horizontal" or "vertical"
#' @param fontsize_barplot_y_numbers fontsize of the text describing numeric barplot max & min values (number)
#' @return ggiraph interactive visualisation
#'
#' @examples
#' path_gg1d <- system.file("testdata/testinput.csv", package = "gg1d")
#' df <- read.csv(path_gg1d, header = TRUE, na.strings = "")
#' gg1d_plot(df, col_id = "ID", col_sort = "Glasses")
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_tile theme %+replace% element_blank element_text element_line
#' @export
#'
gg1d_plot <- function(
    .data, col_id = NULL, col_sort = NULL, maxlevels = 6, verbose = TRUE,
    drop_unused_id_levels = FALSE, interactive = TRUE, debug_return_col_info = FALSE,
    palettes = NULL,
    colours_default = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F","#E5C494"),
    colours_default_logical = c("TRUE" = "#648fff", "FALSE" = "#dc267f"),
    colours_missing = "grey90",
    limit_plots = TRUE, cols_to_plot = NULL,
    sort_type = c("frequency", "alphabetical"), desc = TRUE, width = 0.9, relative_height_numeric = 4,
    tooltip_column_suffix = "_tooltip",
    ignore_column_regex = "_ignore$",
    show_legend_titles = FALSE, show_legend = !interactive, legend_position = c("right", "left", "bottom", "top"),
    legend_title_position = c("top", "bottom", "left", "right"),
    legend_title_beautify = TRUE,
    numeric_plot_type = c('bar', "heatmap"),
    legend_nrow = 4, legend_ncol = NULL,
    legend_title_size = NULL, legend_text_size = NULL, legend_key_size = 0.3,
    vertical_spacing = 0, na_marker = "!", na_marker_size = 8,
    show_na_marker_categorical = FALSE,
    show_na_marker_heatmap = FALSE,
    show_values_heatmap = TRUE,
    fontsize_y_text = 12,
    y_axis_position = c("left", "right"),
    legend_orientation_heatmap = c("horizontal", "vertical"),
    colours_heatmap_low = "purple",
    colours_heatmap_high = "seagreen",
    transform_heatmap = c("identity", "log10", "log2"),
    fontsize_values_heatmap = 3,
    colours_values_heatmap = "white",
    fontsize_barplot_y_numbers = 8
    ) {

  # Assertions --------------------------------------------------------------
  assertions::assert_dataframe(.data)
  assertions::assert_number(maxlevels)
  assertions::assert_flag(drop_unused_id_levels)
  assertions::assert_flag(drop_unused_id_levels)
  assertions::assert_flag(interactive)
  assertions::assert_flag(limit_plots)
  assertions::assert_flag(desc)
  assertions::assert_flag(verbose)
  assertions::assert_number(relative_height_numeric)
  assertions::assert_string(tooltip_column_suffix)
  assertions::assert_flag(show_legend)
  assertions::assert_flag(show_legend_titles)
  assertions::assert_number(legend_key_size)
  assertions::assert_equal(length(colours_default_logical), 2)
  assertions::assert_names_include(colours_default_logical, c("TRUE", "FALSE"))
  assertions::assert_string(colours_missing)
  assertions::assert_flag(legend_title_beautify)
  assertions::assert_number(vertical_spacing)
  assertions::assert_string(ignore_column_regex)
  assertions::assert_string(colours_heatmap_low)
  assertions::assert_string(colours_heatmap_high)
  assertions::assert_logical(show_values_heatmap)
  assertions::assert_number(fontsize_values_heatmap)
  assertions::assert_string(colours_values_heatmap)
  assertions::assert_logical(show_na_marker_categorical)
  assertions::assert_logical(show_na_marker_heatmap)
  assertions::assert_number(fontsize_barplot_y_numbers)

  # Conditional Assertions
  if (!is.null(legend_nrow)) assertions::assert_number(legend_nrow)
  if (!is.null(cols_to_plot)) assertions::assert_names_include(.data, names = cols_to_plot)
  if (!is.null(legend_title_size)) assertions::assert_number(legend_title_size)
  if (!is.null(palettes)) assertions::assert_list(palettes)
  if(!all(colnames(.data) %in% names(palettes))) assertions::assert_greater_than_or_equal_to(length(colours_default), minimum = maxlevels)
  if(!is.null(legend_nrow)) assertions::assert_number(legend_nrow)
  if(!is.null(legend_ncol)) assertions::assert_number(legend_ncol)
  if(!is.null(legend_nrow) & !is.null(legend_ncol)) cli::cli_abort("You've set both {.strong legend_nrow = {legend_nrow}} and {.strong legend_ncol = {legend_ncol}}. Please choose one or the other, and set whichever you don't use to NULL")

  # Argument Matching
  sort_type <- rlang::arg_match(sort_type)
  legend_position <- rlang::arg_match(legend_position)
  legend_title_position <- rlang::arg_match(legend_title_position)
  y_axis_position <- rlang::arg_match(y_axis_position)
  numeric_plot_type <- rlang::arg_match(numeric_plot_type)
  legend_orientation_heatmap <- rlang::arg_match(legend_orientation_heatmap)
  transform_heatmap <- rlang::arg_match(transform_heatmap)

  # Ignore relative_height_numeric if plot type is numeric
  if(numeric_plot_type == "heatmap") relative_height_numeric = 1

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
  df_col_info <- column_info_table(
    .data, maxlevels = maxlevels,
    col_id = col_id,
    cols_to_plot = cols_to_plot,
    tooltip_column_suffix = tooltip_column_suffix,
    palettes = palettes,
    colours_default = colours_default,
    colours_default_logical = colours_default_logical,
    ignore_column_regex = ignore_column_regex,
    verbose = verbose
  )

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

    if(verbose){ cli::cli_bullets(c(
      "*" = "Sorting X axis by: {.strong {col_sort}}",
      "*" = "Order type: {.strong {sort_type}}",
      "*" = "Sort order: {.strong {ifelse(desc, 'descending', 'ascending')}}"
    ))
    }
    .data[[col_id]] <- forcats::fct_reorder(.data[[col_id]], rank::smartrank(.data[[col_sort]], sort_by = sort_type, desc = desc, verbose = FALSE))
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

  # Make sure theres at least 1 plottable column
  if(plottable_cols == 0){
   cli::cli_abort("No plottable columns found")
  }


  gglist <- lapply(
    X = seq_len(nrow(df_col_info)),
    function(i) {
      colname <- df_col_info[["colnames"]][i]
      coltype <- df_col_info[["coltype"]][i]
      coltooltip <- df_col_info[["coltooltip"]][i]
      ndistinct <- df_col_info[["ndistinct"]][i]
      plottable <- df_col_info[["plottable"]][i]
      palette <- unlist(df_col_info[["palette"]][i])


      # Don't plot if not plottable
      if (!plottable) {
        if (verbose) cli::cli_alert_warning("{.warn Skipping} column {.strong {colname}}")
        return(NULL)
      } else {
        if (verbose) cli::cli_alert_success("{.success Plotting} column {.strong {colname}}")
      }

      # Create interactive geom aesthetics
      if(is.na(coltooltip)){
        aes_interactive <- aes(
          data_id = .data[[col_id]],
          tooltip = paste0(
            tag_bold(colname), ": ", .data[[colname]],
            "<br>",
            tag_bold(col_id), ": ", .data[[col_id]]
          )
        )
      }
      else
      {
        # replace values of NA with ""
        .data[[coltooltip]] <- ifelse(is.na(.data[[coltooltip]]), "",.data[[coltooltip]])

        aes_interactive <- aes(
          data_id = .data[[col_id]],
          tooltip = .data[[coltooltip]]
        )
      }

      # Draw the actual plot

      ## Categorical -------------------------------------------------------------
      if (coltype == "categorical") {
        gg <- ggplot(
          .data,
          aes(
            x = .data[[col_id]],
            y = if(legend_title_beautify) beautify(colname) else colname,
            fill = .data[[colname]]
            )
          ) +
          ggiraph::geom_tile_interactive(mapping = aes_interactive, width = width, na.rm = TRUE) +
          {if(show_na_marker_categorical) {
            ggplot2::geom_text(
            data = function(x){x[is.na(x[[colname]]), ,drop=FALSE]},  #only add text where value is NA
            aes(label = na_marker), size = na_marker_size, na.rm = TRUE, vjust=0.5
            ) }} +
          ggplot2::scale_x_discrete(drop = drop_unused_id_levels) +
          #ggplot2::ylab(if(legend_title_beautify) beautify(colname) else colname) +
          theme_categorical(
            show_legend_titles = show_legend_titles,
            show_legend = show_legend,
            legend_position = legend_position,
            legend_title_size = legend_title_size,
            legend_text_size = legend_text_size,
            legend_key_size = legend_key_size,
            vertical_spacing = vertical_spacing,
            fontsize_y_text = fontsize_y_text
            ) +
          ggplot2::guides(fill = ggplot2::guide_legend(
            title.position = legend_title_position,
            title = if(legend_title_beautify) beautify(colname) else colname,
            nrow = legend_nrow,
            ncol = legend_ncol
            )) +
          ggplot2::scale_fill_manual(values = palette, na.value = colours_missing) +
          ggplot2::scale_y_discrete(position = y_axis_position)
      }
      # Numeric Bar -------------------------------------------------------------------------
      else if (coltype == "numeric" && numeric_plot_type == "bar") {
        breaks <- sensible_3_breaks(.data[[colname]])
        labels <- sensible_3_labels(
          .data[[colname]],
          axis_label = if(legend_title_beautify) beautify(colname) else colname,
          fontsize_numbers = fontsize_barplot_y_numbers
          )

        gg <- ggplot2::ggplot(.data, aes(x = .data[[col_id]], y = .data[[colname]])) +
          ggiraph::geom_col_interactive(mapping = aes_interactive, width = width, na.rm = TRUE) +
          ggplot2::geom_text(
            data = function(x){x[is.na(x[[colname]]), ,drop=FALSE]},  #only add text where value is NA
            aes(label = na_marker, y = 0), size = na_marker_size, na.rm = TRUE, vjust=0
            ) +
          ggplot2::scale_x_discrete(drop = drop_unused_id_levels) +
          ggplot2::scale_y_continuous(
            breaks = breaks,
            #minor_breaks = mean(sensible_3_breaks(.data[[colname]])),
            labels = labels,
            position = y_axis_position,
            expand = c(0,0)
          ) +
          #ggplot2::ylab(if(legend_title_beautify) beautify(colname) else colname) +
          theme_numeric_bar(vertical_spacing = vertical_spacing, fontsize_y_text = fontsize_y_text)
      }
      # Numeric Heatmap -------------------------------------------------------------------------
      else if (coltype == "numeric" && numeric_plot_type == "heatmap") {
        gg <- ggplot2::ggplot(.data, aes(
            x = .data[[col_id]],
            y = if(legend_title_beautify) beautify(colname) else colname,
            fill = .data[[colname]])
          ) +
          ggiraph::geom_tile_interactive(mapping = aes_interactive, width = width, na.rm = TRUE) +
          ggplot2::scale_x_discrete(drop = drop_unused_id_levels) +
          ggplot2::scale_y_discrete(position = y_axis_position) +
          {if(show_na_marker_heatmap) {
            ggplot2::geom_text(
              data = function(x){x[is.na(x[[colname]]), ,drop=FALSE]},  #only add text where value is NA
              aes(label = na_marker), size = na_marker_size, na.rm = TRUE, vjust=0.5
            ) }} +
          {if(show_values_heatmap) {
            ggplot2::geom_text(
              aes(label = .data[[colname]]),
              size = fontsize_values_heatmap, color = colours_values_heatmap, na.rm = TRUE, vjust=0.5
            ) }} +
          theme_numeric_heatmap(
            show_legend_titles = show_legend_titles,
            show_legend = show_legend,
            legend_position = legend_position,
            legend_title_size = legend_title_size,
            legend_text_size = legend_text_size,
            legend_key_size = legend_key_size,
            vertical_spacing = vertical_spacing,
            fontsize_y_text = fontsize_y_text
          ) +
          ggplot2::scale_fill_gradient(
            low = colours_heatmap_low,
            high = colours_heatmap_high,
            na.value = colours_missing,
            trans = transform_heatmap,
            guide = ggplot2::guide_colorbar(
              direction = if(legend_orientation_heatmap == "horizontal") "horizontal" else "vertical",
              title.position = "top",
              title.hjust = 0
              )
            )
      }
      else {
        cli::cli_abort("Unsure how to plot coltype: {coltype}")
      }
      return(gg)
    }
  )
  names(gglist) <- df_col_info[["colnames"]]

  # Remove null columns
  gglist <- gglist[!vapply(gglist, is.null, logical(1))]

  # Get relative heights for plots (make numeric variables taller)
  relheights = ifelse(df_col_info$coltype[df_col_info$plottable] == "numeric", yes = relative_height_numeric, no = 1)

  # Align plots vertically
  if (verbose) cli::cli_alert_info("Stacking plots vertically")
  ggpatch <- patchwork::wrap_plots(gglist, ncol = 1, heights = relheights)


  # Interactivity -----------------------------------------------------------
  if(interactive){
    if(verbose) cli::cli_alert_info("Making plot interactive since `interactive = TRUE`")
    ggpatch <- ggiraph::girafe(
      ggobj = ggpatch,
      options =  list(
        opts_hover = ggiraph::opts_hover(css = "stroke:black;cursor:pointer;r:5px;")
      )
    )}
  else{
    if(verbose) cli::cli_alert_info("Rendering static plot. For interactive version set `interactive = TRUE`")
  }

  # Return -----------------------------------------------------------
  return(ggpatch)
}


theme_categorical <- function(fontsize_y_text = 12, show_legend = TRUE,show_legend_titles = FALSE, legend_position = "right", legend_title_size = NULL, legend_text_size = NULL, legend_key_size = 0.3, vertical_spacing = 0) {
  ggplot2::theme_minimal() %+replace%

    theme(
      panel.grid = element_blank(),
      axis.text.y.left = element_text(size = fontsize_y_text),
      axis.text.y.right = element_text(size = fontsize_y_text),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.key.size = ggplot2::unit(legend_key_size, "line"),
      legend.title = if(show_legend_titles) element_text(size = legend_title_size, face = "bold", hjust = 0) else element_blank(),
      legend.justification = "left",
      legend.text = element_text(size = legend_text_size),
      legend.position = if(show_legend) legend_position else "none",
      strip.placement = "outside",
      plot.margin = ggplot2::margin(t = 0, r = 0, b = vertical_spacing, l = 0, unit = "pt"),
    )
}

theme_numeric_bar <- function(vertical_spacing = 0, fontsize_y_text = 12) {
  ggplot2::theme_minimal() %+replace%

    theme(
      panel.grid = element_blank(),
      axis.title.y = element_blank(),
      axis.title.y.right = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.line.y = element_line(linewidth = 0.3),
      axis.line.x = element_blank(),
      axis.text.y.left = ggtext::element_markdown(size = fontsize_y_text),
      axis.text.y.right = ggtext::element_markdown(size = fontsize_y_text, hjust = 0),
      axis.ticks.y = element_blank(),
      strip.placement = "outside",
      plot.margin = ggplot2::margin(t = 5, r = 0, b = vertical_spacing + 5, l = 0, unit = "pt")
    )
}

theme_numeric_heatmap <- function(fontsize_y_text = 12, show_legend = TRUE, legend_position = "right", show_legend_titles = FALSE, legend_title_size = NULL, legend_text_size = NULL, legend_key_size = 0.3, vertical_spacing = 0) {
  ggplot2::theme_minimal() %+replace%

    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(size = fontsize_y_text),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = if (show_legend_titles) element_text(size = legend_title_size, face = "bold", hjust = 0) else element_blank(),
      legend.justification = "left",
      legend.text = element_text(size = legend_text_size),
      legend.position = if(show_legend) legend_position else "none",
      strip.placement = "outside",
      plot.margin = ggplot2::margin(t = 0, r = 0, b = vertical_spacing, l = 0, unit = "pt")
    )
}


tag_bold <- function(x){
  paste0("<b>", x, "</b>", collapse = "")
}

#' GGplot breaks
#'
#' Find sensible values to add 2 breaks at for a ggplot2 axis
#'
#' @param vector vector fed into ggplot axis you want to define sensible breaks for
#'
#' @return vector of length 2. first element descripts upper break position, lower describes lower break
#'
sensible_2_breaks <- function(vector){
  upper <- max(vector, na.rm = TRUE)
  lower <- min(0, min(vector, na.rm = TRUE), na.rm = TRUE)
  c(upper, lower)
}

sensible_3_breaks <- function(vector){
  upper <- max(vector, na.rm = TRUE)
  lower <- min(0, min(vector, na.rm = TRUE), na.rm = TRUE)
  middle = mean(c(upper, lower))

  breaks <- c(upper, middle, lower)

  if(upper == lower){
    return(lower)
  }

  return(breaks)
}

sensible_3_labels <- function(vector, axis_label, fontsize_numbers = 7){
  upper <- max(vector, na.rm = TRUE)
  lower <- min(0, min(vector, na.rm = TRUE), na.rm = TRUE)
  if (lower == upper)
    return(axis_label)
  upper <- paste0("<span style = 'font-size: ",fontsize_numbers,"pt'>",upper, "</span>")
  lower <- paste0("<span style = 'font-size: ",fontsize_numbers,"pt'>",lower, "</span>")

  #middle = mean(c(upper, lower)
  as.character(c(upper, axis_label, lower))
}


#' Make strings prettier for printing
#'
#' Takes an input string and 'beautify' by converting underscores to spaces and
#'
#' @param string input string
#'
#' @return string
#'
beautify <- function(string){
  # underscores to spaces
  string <- gsub(x=string, pattern = "_", replacement = " ")

  # dots to spaces
  string <- gsub(x=string, pattern = ".", replacement = " ", fixed = TRUE)

  # camelCase to camel Case
  string <- gsub(x=string, pattern = "([a-z])([A-Z])", replacement = "\\1 \\2")

  # Capitalise Each Word
  string <- gsub(x=string, pattern = "^([a-z])",  perl = TRUE, replacement = ("\\U\\1"))
  string <- gsub(x=string, pattern = " ([a-z])",  perl = TRUE, replacement = (" \\U\\1"))
}
