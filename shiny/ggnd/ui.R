library(shiny)
library(bslib)
library(ggplot2)
library(rio)
library(shinycssloaders)
library(htmlwidgets)
library(shinyWidgets)
library(colourpicker)
library(assertions)

source("utils.R")

# Modules
source("modules/mod_colour_picker.R")

ui <- page_sidebar(
  title = "EDA",
  theme = bs_theme(bootswatch = "bootstrap"),


  # SideBar -----------------------------------------------------------------
  sidebar = sidebar(
    width = "900px",
    title = NULL,

    ## File Import
    card(
      min_height = "120px",
      card_header("Import your file (TSV, CSV, or XLSX)"),
      card_body(
        fileInput(inputId = "in_file_dataset", label = NULL, multiple = FALSE, width = "100%")
      ),
      fluidRow(
        div(
          style = "display: flex; align-items: center; gap: 10px;",
          strong("Demo Datasets:"),
          actionLink("in_link_iris", label = "Iris"),
          actionLink("in_link_penguins", label = "Palmer Penguins"),
          actionLink("in_link_minibeans", label = "Minibeans"),
          actionLink("in_link_chickweights", label = "Chicken Weights"),
          actionLink("in_link_lazy_birdwatcher", label = "Lazy Birdwatcher"),
        )
      )
    ),
    hr(),

    ## gg1d Parameters
    card(
      card_header("Choose a graph type", ),
      radioGroupButtons(
        inputId = "in_radio_plot_type",
        label = NULL,
        choices = c("GG1D" = "gg1d", "Parallel Coordinate Plot" = "pcp"),
        justified = TRUE, width = "100%", status = "primary"
      )
    ),
    navset_hidden(
      id = "navset_visoptions",


      ## GG1D --------------------------------------------------------------------
      nav_panel(
        value = "gg1d",
        title = "gg1d",
        layout_column_wrap(
          heights_equal = "row", width = 1 / 3, height = "900px",
          bslib::card(
            card_header("Sorting"),
            selectizeInput("in_sel_sort_col", label = "Sort By", choices = NULL, multiple = TRUE, selected = NULL, options = list(dropdownParent = "body")),
            selectizeInput("in_sel_sort_type",
              label = "Sort Type",
              choices = c("frequency", "alphabetical"), multiple = FALSE, selected = "frequency",
              options = list(dropdownParent = "body")
            ),
            checkboxInput("in_desc", label = "Descending", value = TRUE),
            checkboxInput("in_order_matches_sort", "Match Column Order to Sort?", value = TRUE)
          ),

          # Colours (options)
          bslib::card(
            full_screen = TRUE,
            width = "200px",
            card_header("Default Colours (categorical)"),
            colourpicker::colourInput(inputId = "in_default_colour_1", value = "#66C2A5", label = "Colour 1"),
            colourpicker::colourInput(inputId = "in_default_colour_2", value = "#FC8D62", label = "Colour 2"),
            colourpicker::colourInput(inputId = "in_default_colour_3", value = "#8DA0CB", label = "Colour 3"),
            colourpicker::colourInput(inputId = "in_default_colour_4", value = "#E78AC3", label = "Colour 4"),
            colourpicker::colourInput(inputId = "in_default_colour_5", value = "#A6D854", label = "Colour 5"),
            colourpicker::colourInput(inputId = "in_default_colour_6", value = "#FFD92F", label = "Colour 6"),
            colourpicker::colourInput(inputId = "in_default_colour_7", value = "#E5C494", label = "Colour 7")
          ),
          bslib::card(
            card_header("Default Colours (logical)"),
            colourpicker::colourInput(inputId = "in_default_logical_colour_true", value = "#648fff", label = "TRUE"),
            colourpicker::colourInput(inputId = "in_default_logical_colour_false", value = "#dc267f", label = "FALSE")
          ),
          bslib::card(
            card_header("Size"),
            # Interactivity size
            numericInput("in_svg_width", "Interactive SVG Width", value = 8, min = 1),
            numericInput("in_svg_height", "Interactive SVG Height", value = 6, min = 1)
          ),
          bslib::card(
            card_header("Main Plot"),
            selectizeInput("in_numeric_plot_type", "Numeric Plot Type",
              choices = c("bar", "heatmap"), selected = "bar",
              options = list(dropdownParent = "body")
            ),
            numericInput("in_relative_height_numeric", "Barplot Height Multiplier", value = 2, min = 1, max = 10),
            selectizeInput("in_y_axis_position", "Y Axis Position", choices = c("left", "right"), selected = "left", options = list(dropdownParent = "body")),
            numericInput("in_width", "Width", value = 0.9, min = 0.1, max = 1)
          ),
          bslib::card(
            card_header("Legend"),
            checkboxInput("in_show_legend", "Show Legend", value = TRUE),
            checkboxInput("in_show_legend_titles", "Show Legend Titles", value = FALSE),
            selectizeInput("in_legend_position", "Legend Position",
              choices = c("right", "left", "bottom", "top"), selected = "right",
              options = list(dropdownParent = "body")
            ),
            selectizeInput("in_legend_title_position", "Legend Title Position",
              choices = c("top", "bottom", "left", "right"), selected = "top",
              options = list(dropdownParent = "body")
            ),
            numericInput("in_legend_nrow", "Legend Rows", value = 1, min = 1),
            numericInput("in_legend_key_size", "Legend Key Size", value = 0.3, min = 0.1)
          ),
          bslib::card(
            card_header("Other"),
            numericInput("in_maxlevels", "Max Levels", value = 7, min = 1),
            numericInput("in_max_plottable_cols", "Max Plottable Columns", value = 10, min = 1),
            selectizeInput("in_sel_id", label = "Identifier", choices = NULL, multiple = FALSE, selected = "", options = list(dropdownParent = "body"))
          ),
          bslib::card(
            card_header("Data Conversion"),
            checkboxInput("in_convert_binary_numeric", "Autoconversion", value = TRUE),
            checkboxInput("in_beautify_text", "Beautify Text", value = TRUE)
          ),
          bslib::card(
            card_header("Columns to Plot"),
            selectizeInput("in_sel_plotting_cols", label = "Plot Columns", choices = NULL, multiple = TRUE, selected = NULL, options = list(dropdownParent = "body")),
            checkboxInput("in_limit_plots", label = "Limit Plot Columns", value = TRUE),
            textInput("in_ignore_regex", "Ignore Column Regex", value = "_ignore$")
          )
        )
      ),
      # Options For PCP plots

      ## PCPs --------------------------------------------------------------------
      nav_panel(
        value = "pcp",
        title = "pcp",
        layout_column_wrap(
          heights_equal = "row", width = 1 / 3, height = "1000px",
          bslib::card(
            card_header("Sorting"),
            selectizeInput("in_sel_pcp_order_columns", label = "Order Columns By", choices = c("auto", "appearance", "random"), multiple = FALSE, selected = "auto", options = list(dropdownParent = "body")),
            selectizeInput("in_sel_pcp_order_observations_by", label = "Order observations by", choices = c("frequency", "original"), options = list(dropdownParent = "body")),
            selectizeInput("in_sel_pcp_scaling", label = "Scaling", choices = c("uniminmax", "none"), multiple = FALSE, selected = "uniminmax", options = list(dropdownParent = "body"))
          ),
          bslib::card(
            card_header("Column to Colour"),
            selectizeInput("in_sel_pcp_col_colour", label = "Colour by", choices = "", options = list(dropdownParent = "body")),
            selectizeInput("in_sel_pcp_highlight", label = "Highlight", choices = "", options = list(dropdownParent = "body")),
            colourpicker::colourInput(inputId = "in_colour_pcp_highlight", value = "red", label = "Highlighted Colour"),
            colourpicker::colourInput(inputId = "in_colour_pcp_highlight_other", value = "grey90", label = "Non-Highlighted Colour")
            # selectInput("in_sel_pcp_col_colour", label = "Colour by", choices = NULL),
          ),
          bslib::card(
            card_header("Palette"),
            colourPickerModuleUI("mod_pcp_pick_colours"),
          ),
          bslib::card(
            card_header("Size"),
            # Interactivity size
            numericInput("in_pcp_svg_width", "Interactive SVG Width", value = 8, min = 1),
            numericInput("in_pcp_svg_height", "Interactive SVG Height", value = 6, min = 1)
          ),
          bslib::card(
            card_header("Legend"),
            checkboxInput("in_check_pcp_show_legend", label = "Show Legend", value = TRUE),
            checkboxInput("in_check_pcp_show_legend_titles", label = "Show Legend Title", value = FALSE),
            selectizeInput("in_sel_pcp_legend_position", label = "Legend Position", choices = c("right", "left", "bottom", "top"), selected = "bottom", options = list(dropdownParent = "body")),
            selectizeInput("in_sel_pcp_legend_title_position", label = "Legend Title Position", choices = c("top", "bottom", "right", "left"), selected = "right", options = list(dropdownParent = "body")),
            numericInput("in_num_pcp_legend_nrow", "Rows", value = 4, min = 1, step = 1),
            numericInput("in_num_pcp_legend_key_size", "Key Size", value = 1, min = 0.1)
          ),
          bslib::card(
            card_header("Column Names"),
            checkboxInput("in_check_pcp_show_column_names", label = "Show Column Names", value = TRUE),
            checkboxInput("in_pcp_beautify_text", "Beautify Text", value = TRUE),
            numericInput("in_num_pcp_x_axis_text_angle", "Angle", value = 90, step = 10),
            numericInput("in_num_pcp_x_axis_text_hjust", "Horizontal Justification", value = 0, step = 0.5, min = 0, max = 1),
            numericInput("in_num_pcp_fontsize_x_axis_text", "Fontsize", value = 12, step = 2, min = 1, max = 100)
          ),
          bslib::card(
            card_header("Bounds"),
            checkboxInput("in_pcp_show_bounds_labels", "Show Bounds Labels", value = FALSE),
            checkboxInput("in_pcp_show_bounds_rect", "Show Bounds Rectangle", value = FALSE),
            numericInput("in_pcp_max_digits_bounds", label = "Max Digits", value = 1, min = 0, max = 5)
          ),
          bslib::card(
            card_header("Line Settings"),
            numericInput("in_pcp_line_alpha", label = "Line Opacity", value = 0.5, min = 0, max = 1, step = 0.2),
            numericInput("in_pcp_line_width", label = "Line Width", value = 0.5, min = 0.01, max = 4, step = 0.25),
            selectizeInput("in_pcp_line_type", label = "Line Type",
                           choices = c(
                             "solid" = "1",
                             "dashed" = "2",
                             "dotted" = "3",
                             "dotdash" = "4",
                             "longdash" = "5",
                             "twodash" = "6"
                           ),
                           selected = "1",
                           options = list(dropdownParent = "body")
            ),
          )
        )
      )
    )
  ),


  # Main UI ------------------------------------------------------
  ## gg1d plot
  card(
    min_height = "500px",
    card_header("Visualisation (gg1d)"),
    card_body(
      withSpinner(
        ggiraph::girafeOutput("out_plot_widget", width = "95%")
      )

      # wellPanel(verbatimTextOutput("out_message_log"))
    )
  ),

  # card(
  #   card_header(title = ""),
  #   min_height = "500px",
  #   card_header("Visualisation (gg1d)"),
  #   card_body(
  #     withSpinner(
  #       ggiraph::girafeOutput("out_plot_widget", width = "95%")
  #     )
  #   )
  # ),

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
