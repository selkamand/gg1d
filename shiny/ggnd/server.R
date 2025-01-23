library(shiny)
library(ggiraph)
library(colourpicker)

# Modules
source("modules/mod_colour_picker.R")

server <- function(input, output, session) {

  # By default we start with IRIS dataset
  dataset <- reactiveVal(value = iris)

  ## Set Selector Choices based on column names
  observeEvent(dataset(), priority = 10, {
    updateSelectInput(
      session = session,
      inputId = "in_sel_id",
      choices = c("", colnames(dataset())),
      selected = ""
    )

    updateSelectInput(
      session = session,
      inputId = "in_sel_plotting_cols",
      choices = c("", colnames(dataset())),
      selected = "",
    )

    updateSelectInput(
      session = session,
      inputId = "in_sel_sort_col",
      choices = c("Column to Sort By" = "", colnames(dataset())),
      selected = "",
    )

    updateSelectInput(
      session = session,
      inputId = "in_sel_pcp_col_colour",
      choices = c("", colnames(dataset())),
      selected = "",
    )

  })

  # Demo Dataset Selection
  observeEvent(input$in_link_iris, dataset(iris))
  observeEvent(input$in_link_penguins, dataset(palmerpenguins::penguins[1:6]))
  observeEvent(input$in_link_minibeans, dataset(minibeans))
  observeEvent(input$in_link_chickweights, dataset(chickwts))
  observeEvent(input$in_link_lazy_birdwatcher, dataset(lazy_birdwatcher))

  observeEvent(input$in_sel_pcp_col_colour, {
    validate(need(!is.null(input$in_sel_pcp_col_colour) && nchar(input$in_sel_pcp_col_colour) > 0 , message = "Requires selection of Column to Colour by"))

    updateSelectInput(
      session = session,
      inputId = "in_sel_pcp_highlight",
      choices = c("Value to highlight"= "", unique(dataset()[input$in_sel_pcp_col_colour])),
      selected = "",
    )
  })

  pcp_col_to_colour_by <- reactive({ input$in_sel_pcp_col_colour })

  colour_palette_pcp <- colourPickerModuleServer(
    id        = "mod_pcp_pick_colours",
    df        = dataset,
    colname   = pcp_col_to_colour_by,
    palette   = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728",
                  "#9467BD", "#8C564B", "#E377C2")
  )

  # Update Nav-Bar Page (showing options) based on Selected Plot Type
  observeEvent( input$in_radio_plot_type, {
    nav_select(id = "navset_visoptions", selected = input$in_radio_plot_type,session = session)
  }, priority = 1)



  # Reactive expression to load data if user provides file. Defaults to iris if none provided.
  observeEvent(input$in_file_dataset, {

    tryCatch(
      expr = {
        data = rio::import(input$in_file_dataset$datapath)
      },
      error = function(err){
        shinyWidgets::sendSweetAlert(session = session, title = "Failed to import file", text =  err2html(err))
        validate("Failed to import file")
      },
      warning = function(warn){
        shinyWidgets::sendSweetAlert(session = session, title = "Failed to Generate GG1D Plot", text =  err2html(warn))
        validate("Failed to import file")
      })

      dataset(data)
    })

  output$out_plot_widget <- ggiraph::renderGirafe({
    data <- req(dataset())

    # Now call gg1d with user-specified parameters
     tryCatch(
      expr = {
        interactive_visualisation <- if(input$in_radio_plot_type == "gg1d"){
          gg1d(
            data = data,
            col_id = validate_column(input$in_sel_id, dataset()),
            col_sort = validate_column(input$in_sel_sort_col, dataset()),
            order_matches_sort = input$in_order_matches_sort,
            maxlevels = input$in_maxlevels %|?|% 7,
            verbose = 1,
            drop_unused_id_levels = FALSE,
            palettes = NULL,
            sort_type = input$in_sel_sort_type,
            desc = input$in_desc,
            limit_plots = input$in_limit_plots,
            max_plottable_cols = input$in_max_plottable_cols %|?|% 15,
            cols_to_plot = validate_column(input$in_sel_plotting_cols, dataset()),
            tooltip_column_suffix = "_tooltip",
            ignore_column_regex = if (nchar(input$in_ignore_regex) > 0) input$in_ignore_regex else NULL,
            convert_binary_numeric_to_factor = input$in_convert_binary_numeric,
            options = gg1d_options(
              show_legend = input$in_show_legend,
              show_legend_titles = input$in_show_legend_titles,
              legend_position = input$in_legend_position,
              legend_title_position = input$in_legend_title_position,
              legend_nrow = input$in_legend_nrow %|?|% 1,
              legend_key_size = input$in_legend_key_size %|?|% 0.3,
              beautify_text = input$in_beautify_text,
              numeric_plot_type = input$in_numeric_plot_type,
              y_axis_position = input$in_y_axis_position,
              width = input$in_width %|?|% 0.9,
              interactive_svg_width = input$in_svg_width %|?|% 8,
              interactive_svg_height = input$in_svg_height %|?|% 6,
              relative_height_numeric = input$in_relative_height_numeric %|?|% 2,
              colours_default = c(
                input$in_default_colour_1,
                input$in_default_colour_2,
                input$in_default_colour_3,
                input$in_default_colour_4,
                input$in_default_colour_5,
                input$in_default_colour_6,
                input$in_default_colour_7
              ),
              colours_default_logical = c(
                "TRUE" = input$in_default_logical_colour_true,
                "FALSE" = input$in_default_logical_colour_false
              )
            )
          )
        }
        else {

          ggparallel(
            data = data,
            order_columns_by = input$in_sel_pcp_order_columns,
            order_observations_by = input$in_sel_pcp_order_observations_by,
            col_colour = is_valid_column(input$in_sel_pcp_col_colour, dataset()),
            highlight = is_valid_column(input$in_sel_pcp_highlight, dataset()),
            scaling = input$in_sel_pcp_scaling,
            convert_binary_numeric_to_factor = TRUE,
            palette_colour = colour_palette_pcp(),
            palette_highlight = c(
              input$in_colour_pcp_highlight,
              input$in_colour_pcp_highlight_other
              ),
            options = ggparallel_options(
              interactive_svg_width = input$in_pcp_svg_width %|?|% 8,
              interactive_svg_height =  input$in_pcp_svg_height %|?|% 6,
              show_legend = input$in_check_pcp_show_legend,
              show_legend_titles = input$in_check_pcp_show_legend_titles,
              legend_position = input$in_sel_pcp_legend_position,
              legend_title_position = input$in_sel_pcp_legend_title_position,
              legend_nrow = input$in_num_pcp_legend_nrow %|?|% NULL,
              legend_key_size = input$in_num_pcp_legend_key_size %|?|% 1,
              show_column_names = input$in_check_pcp_show_column_names,
              x_axis_text_angle = input$in_num_pcp_x_axis_text_angle %|?|% 90,
              x_axis_text_hjust = input$in_num_pcp_x_axis_text_hjust %|?|% 0,
              fontsize_x_axis_text = input$in_num_pcp_fontsize_x_axis_text %|?|% 12,
              beautify_text = input$in_pcp_beautify_text,
              show_bounds_labels = input$in_pcp_show_bounds_labels,
              show_bounds_rect = input$in_pcp_show_bounds_rect,
              max_digits_bounds = input$in_pcp_max_digits_bounds %|?|% 1,
              line_alpha = input$in_pcp_line_alpha %|?|% 0.5,
              line_width = input$in_pcp_line_width %|?|% NULL,
              line_type = as.numeric(input$in_pcp_line_type)
              )
          )
        }

        return(interactive_visualisation)
      },
      error = function(err){
        shinyWidgets::sendSweetAlert(session = session, title = "Failed to Generate Plot", text =  err2html(err))
        validate("Failed to Generate Plot")
      }
      # warning = function(warn){
      #   shinyWidgets::sendSweetAlert(session = session, title = "Warning when Generating Plot", text =  err2html(warn))
      #   validate("Failed to Generate GG1D plot")
      # }
    )
  })

  # Render UI palettes
  # output$out_ui_palettes <- renderUI({
  #   data <- req(dataset())
  #   col_is_categorical <- vapply(data, function(x) { is.character(x) || is.factor(x) }, logical(1))
  #   categorical_col_names <- colnames(data)[col_is_categorical]
  #
  #   pickers_list <- lapply(categorical_col_names, function(colname) {
  #     categorical_col_values <- unique(data[[colname]])
  #     card(min_height = "300px",
  #       card_header(colname),
  #       div(
  #         style = "display:flex; flex-wrap: wrap; gap: 10px;",
  #         lapply(categorical_col_values, function(level) {
  #           colourpicker::colourInput(
  #             inputId = paste0("palette_", colname, "_", level),
  #             label = level,
  #             value = "#000000"
  #           )
  #         })
  #       )
  #     )
  #   })
  #
  #   do.call(tagList, pickers_list)
  # })


}
