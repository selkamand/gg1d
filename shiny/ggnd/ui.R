library(shiny)
library(bslib)
library(ggplot2)
library(rio)
library(shinycssloaders)
library(htmlwidgets)

# Source Assertions
source('assertions/assert.R')
source('assertions/assert_class.R')
source('assertions/is_comparisons.R')
source('assertions/is_functions.R')
source('assertions/utils.R')
source('assertions/assert_create.R')
source('assertions/assert_type.R')
source('assertions/assert_compare.R')
source('assertions/assert_dataframe.R')
source('assertions/assert_files.R')
source('assertions/assert_functions.R')
source('assertions/set_operations.R')
source('assertions/assert_includes.R')
source('assertions/assert_length.R')
source('assertions/assert_names.R')
source('assertions/assert_null.R')
source('assertions/assert_numerical.R')
source('assertions/assert_set.R')
source('assertions/coverage_testing.R')
source('assertions/export_testing.R')
source('assertions/has.R')
# files_to_source <- list.files(path = "assertions", pattern = "\\.R$", full.names = TRUE)
# sapply(files_to_source, source)
source("utils.R")
source("gg1d/data.R")
source("gg1d/gg1d.R")
source("gg1d/gg1d_options.R")


ui <- page_sidebar(
  title = "ggNd",
  theme = bs_theme(bootswatch = "bootstrap"),
  sidebar = sidebar(width = "300px",
    title = "Visualisation Options",
    ## gg1d Parameters
    # Output("out_sel_id"),
    # Output("out_sel_sort_col"),


    bslib::card(
      card_header("Sorting"),
      selectizeInput("in_sel_sort_col", label = "Sort By", choices = NULL, multiple = TRUE, selected = NULL),
      selectInput("in_sel_sort_type",
                  label = "Sort Type",
                  choices = c("frequency", "alphabetical"), multiple = FALSE, selected = "frequency"
      ),
      checkboxInput("in_desc", label = "Descending", value = TRUE),
      checkboxInput("in_order_matches_sort", "Match Column Order to Sort?", value = TRUE)
    ),

    bslib::card(
      card_header("Size"),
      # Interactivity size
      numericInput("in_svg_width", "Interactive SVG Width", value = 16, min = 1),
      numericInput("in_svg_height", "Interactive SVG Height", value = 5, min = 1)
    ),

    bslib::card(
      card_header("Main Plot"),
      selectInput("in_numeric_plot_type", "Numeric Plot Type",
                  choices = c("bar", "heatmap"), selected = "bar"
      ),
      numericInput("in_relative_height_numeric", "Barplot Height Multiplier", value = 2, min = 1, max = 10),
      selectInput("in_y_axis_position", "Y Axis Position", choices = c("left", "right"), selected = "left"),
      numericInput("in_width", "Width", value = 0.9, min = 0.1, max = 1),
    ),


    bslib::card(
      card_header("Columns to Plot"),
      textInput("in_ignore_regex", "Ignore Column Regex", value = "_ignore$"),
    ),

    bslib::card(
      card_header("Legend"),
      checkboxInput("in_show_legend", "Show Legend", value = TRUE),
      checkboxInput("in_show_legend_titles", "Show Legend Titles", value = FALSE),
      selectInput("in_legend_position", "Legend Position",
                  choices = c("right", "left", "bottom", "top"), selected = "right"
      ),
      selectInput("in_legend_title_position", "Legend Title Position",
                  choices = c("top", "bottom", "left", "right"), selected = "top"
      ),
      numericInput("in_legend_nrow", "Legend Nrow", value = 1, min = 1),
      # numericInput("in_legend_nrow", "Legend Nrow", value = 3, min = 1),
      numericInput("in_legend_key_size", "Legend Key Size", value = 0.3, min = 0.1),
    ),

    bslib::card(
      card_header("Data Conversion"),
      checkboxInput("in_convert_binary_numeric", "Autoconversion", value = TRUE),
      checkboxInput("in_beautify_text", "Beautify Text", value = TRUE),
    ),


    bslib::card(
      card_header("Other"),
      numericInput("in_maxlevels", "Max Levels", value = 6, min = 1),
      numericInput("in_max_plottable_cols", "Max Plottable Columns", value = 15, min = 1),
      selectInput("in_sel_id", label = "Identifier", choices = NULL, multiple = FALSE, selected = ""),
    ),

    bslib::card(
      card_header("Other"),
      numericInput("in_maxlevels", "Max Levels", value = 6, min = 1),
      numericInput("in_max_plottable_cols", "Max Plottable Columns", value = 15, min = 1),
      selectInput("in_sel_id", label = "Identifier", choices = NULL, multiple = FALSE, selected = ""),
    ),

    # Colours (options)
    bslib::card(
      card_header("Default Colours (categorical)"),
      colourpicker::colourInput(inputId = "in_default_colour_1", value = "#66C2A5", label = "Colour 1"),
      colourpicker::colourInput(inputId = "in_default_colour_2", value = "#FC8D62", label = "Colour 2"),
      colourpicker::colourInput(inputId = "in_default_colour_3", value = "#8DA0CB", label = "Colour 3"),
      colourpicker::colourInput(inputId = "in_default_colour_4", value = "#E78AC3", label = "Colour 4"),
      colourpicker::colourInput(inputId = "in_default_colour_5", value = "#A6D854", label = "Colour 5"),
      colourpicker::colourInput(inputId = "in_default_colour_6", value = "#FFD92F", label = "Colour 6"),
      colourpicker::colourInput(inputId = "in_default_colour_7", value = "#E5C494", label = "Colour 7"),
    ),

    bslib::card(
      card_header("Default Colours (logical)"),
      colourpicker::colourInput(inputId = "in_default_logical_colour_true", value = "#648fff", label = "TRUE"),
      colourpicker::colourInput(inputId = "in_default_logical_colour_false", value = "#dc267f", label = "FALSE"),
    ),


  ),

  ## File Import
  card(
    min_height = "120px",
    max_height = "120px",
    card_header("Import your file (TSV, CSV, or XLSX)"),
    card_body(
      fileInput(inputId = "in_file_dataset", label = NULL, multiple = FALSE, width = "100%")
    )
  ),

  ## gg1d plot
  card(
    min_height = "500px",
    max_height = "600px",
    card_header("Visualisation (gg1d)"),
    card_body(
      withSpinner(
        ggiraph::girafeOutput("out_plot_widget", width = "95%")
      )
    )
  ),
#
#   # Colours
#   bslib::card(
#     # max_height =  "300px",
#     card_header("Palettes"),
#     wellPanel("Default palettes can be set using the sidebar panel. Use these settings to colour specific columns independently of the others"),
#     uiOutput("out_ui_palettes",fill =  bslib::as_fill_carrier())
#     # selectInput(inputId = "in_select_cols_for_palettes", label = "Columns requiring custom palette", choices = c(), selected = c(), multiple = TRUE)
#   )

)
