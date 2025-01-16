library(shiny)
library(ggiraph)

server <- function(input, output, session) {

  ## Set Selector Choices based on column names
  dataset <- reactiveVal(value = iris)

  observeEvent(dataset() , {
    updateSelectInput(
      session = session,
      inputId = "in_sel_id",
      choices = c("", colnames(dataset())),
      selected = ""
    )

    updateSelectInput(
      session = session,
      inputId = "in_sel_sort_col",
      choices = c("", colnames(dataset())),
      selected = "",
    )

    updateSelectInput(
      session = session,
      inputId = "in_sel_pcp_col_colour",
      choices = c("", colnames(dataset())),
      selected = "",
    )
  })

  observeEvent(input$in_sel_pcp_col_colour, {
    validate(need(!is.null(input$in_sel_pcp_col_colour) && nchar(input$in_sel_pcp_col_colour) > 0 , message = "Requires selection of Column to Colour by"))

    updateSelectInput(
      session = session,
      inputId = "in_sel_pcp_highlight",
      choices = c("Column to Sort By"= "", unique(dataset()[input$in_sel_pcp_col_colour])),
      selected = "",
    )
  })

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



    # Build gg1d_options from user input
    opts <- gg1d_options(
      show_legend = input$in_show_legend,
      show_legend_titles = input$in_show_legend_titles,
      legend_position = input$in_legend_position,
      legend_title_position = input$in_legend_title_position,
      legend_nrow = input$in_legend_nrow,
      # We leave legend_ncol as NULL if not specified.
      # user can add another input if needed.
      legend_key_size = input$in_legend_key_size,
      beautify_text = input$in_beautify_text,
      numeric_plot_type = input$in_numeric_plot_type,
      y_axis_position = input$in_y_axis_position,
      width = input$in_width,
      interactive_svg_width = input$in_svg_width,
      interactive_svg_height = input$in_svg_height,
      relative_height_numeric = input$in_relative_height_numeric,
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
      # Add other gg1d_options parameters as needed
    )

    # Now call gg1d with user-specified parameters
    tryCatch(
      expr = {
        if(input$in_radio_plot_type == "gg1d"){
          gg1d(
            data = data,
            col_id = if(nchar(input$in_sel_id) == 0) NULL else input$in_sel_id,
            col_sort = input$in_sel_sort_col,
            order_matches_sort = input$in_order_matches_sort,
            maxlevels = input$in_maxlevels,
            verbose = 1,
            drop_unused_id_levels = FALSE,
            palettes = NULL,
            sort_type = input$in_sel_sort_type,
            desc = input$in_desc,
            limit_plots = TRUE,
            max_plottable_cols = input$in_max_plottable_cols,
            cols_to_plot = NULL,
            tooltip_column_suffix = "_tooltip",
            ignore_column_regex = if(nchar(input$in_ignore_regex) > 0) input$in_ignore_regex else NULL,
            convert_binary_numeric_to_factor = input$in_convert_binary_numeric,
            options = opts
          )
        }
        else
          ggparallel(
            data = data,
            order_columns_by = input$in_sel_pcp_order_columns,
            order_observations_by = input$in_sel_pcp_order_observations_by,
            col_colour = if(nchar(input$in_sel_pcp_col_colour) == 0) NULL else input$in_sel_pcp_col_colour,
            highlight = if(nchar(input$in_sel_pcp_highlight) == 0) NULL else input$in_sel_pcp_highlight,
            scaling = input$in_sel_pcp_scaling,
            options = ggparallel_options(
              interactive_svg_width = input$in_pcp_svg_width,
              interactive_svg_height =  input$in_pcp_svg_height
              )
          )
      },
      error = function(err){
        shinyWidgets::sendSweetAlert(session = session, title = "Failed to Generate GG1D Plot", text =  err2html(err))
        validate("Failed to Generate GG1D plot")
      },
      warning = function(warn){
        shinyWidgets::sendSweetAlert(session = session, title = "Failed to Generate GG1D Plot", text =  err2html(warn))
        validate("Failed to Generate GG1D plot")
      }
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
