# =========== MODULE UI ===========
colourPickerModuleUI <- function(id) {
  ns <- NS(id)

  # We use uiOutput so that we can dynamically create the inputs
  # on the server side
  tagList(
    uiOutput(ns("picker_ui"))
  )
}

# =========== MODULE SERVER ===========
#' Colour Picker Based On Column
#'
#' Generate a reactive set of colourPicker UI elements (1 per unique value in \code{df[colname]}) and
#' return a palette vector with colours for each unique value.
#'
#' @param id module ID
#' @param df dataset (data.frame; reactive)
#' @param colname column in \code{df} to extract unique levels from (string; reactive)
#' @param palette default colours to use. If length is less than the size of the    (character)
#'
#' @returns a named vector where names are all unique values of column \code{colname} in dataset \code{df} and values are colours based on pickerInput selections.
#' @export
#'
colourPickerModuleServer <- function(id, df, colname, palette) {
  assertions::assert_reactive(colname)
  assertions::assert_reactive(df)
  moduleServer(id, function(input, output, session) {
    # Create a reactive expression for the unique values in the specified column
    unique_vals <- reactive({
      # convert the factor to character before returning
      unique_vals_raw <- df()[[colname()]]
      as.character(unique(unique_vals_raw))
    })


    # Render UI: color pickers for each unique value
    output$picker_ui <- renderUI({
      ns <- session$ns

      # Subset the palette to the number of unique values (recycle if needed)
      # or handle short palettes in some robust way
      n_unique <- length(unique_vals())
      if (length(palette) < n_unique) {
        # e.g. recycle palette if it is too short
        palette_use <- rep_len(palette, n_unique)
      } else {
        palette_use <- palette[seq_len(n_unique)]
      }

      # Create a colourInput for each unique value
      pickers <- mapply(
        FUN = function(value, default_color) {
          colourpicker::colourInput(
            inputId = ns(value),
            label = value,
            value = default_color,
            showColour = "both"
          )
        },
        value = unique_vals(),
        default_color = palette_use,
        SIMPLIFY = FALSE
      )

      # Wrap all pickers in a tagList
      tagList(pickers)
    })

    # Return a reactive list of selected colors, keyed by the unique value
    #    e.g. list("Species1" = "#FF0000", "Species2" = "#00FF00", ...)
    selected_colors <- reactive({
      vals <- unique_vals()

      colours <- vapply(vals, function(v){input[[v]] %|?|% "black"}, FUN.VALUE = character(1))
      names(colours) <- vals
      return(colours)
    })

    return(selected_colors)
  })
}

