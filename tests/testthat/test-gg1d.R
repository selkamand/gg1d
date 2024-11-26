test_that("gg1d works as expected", {
  expect_error(suppressMessages(gg1d(mtcars)), NA)
})



test_that("gg1d works even if col only has 1 valid numeric column", {
  df <- data.frame(
    col1 = c(0, 0, NA, NA, NA, NA, NA, NA, 0, NA),
    col2 = c(0, 0, 0, 0, 0)
  )
  expect_error(suppressWarnings(suppressMessages(gg1d(df, cols_to_plot = "col1"))), NA)
  expect_error(suppressMessages(gg1d(df, cols_to_plot = "col2")), NA)
})


test_that("gg1d throws error if no plottable columns", {
  expect_error(gg1d(data.frame(), verbose = 0), "No plottable columns found")
})


cli::test_that_cli("gg1d doesn't warn about columns the user isn't interested in", configs = "plain", {
  # We first define a data-frame which includes a 'Letters' column that has way too many
  # levels for gg1d to plot. If we try it will warn that it drops
  df <- data.frame(
    ID = 1:19,
    Glasses = c(
      TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE,
      FALSE, FALSE, TRUE, TRUE, FALSE, NA, NA, FALSE
    ),
    Letters = c(
      "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
      "M", "N", "O", "P", "Q", "R", "S"
    )
  )

  # Check the appropriate warning is thrown
  suppressMessages(expect_message(gg1d(df, verbose = 2), "Columns with too many unique values: Letters"))

  # If user only wants to plot glasses, there's no reason to warn about Letters
  suppressMessages(expect_no_message(gg1d(df, cols_to_plot = c("Glasses"), verbose = 2), message = "Columns with too many unique values:"))
})



# Newtests ----------------------------------------------------------------

# Mock Data
mock_data <- data.frame(
  ID = 1:10,
  Category = rep(c("A", "B", "C", "D", "E"), 2),
  Numeric = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  Logical = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
  Tooltip = letters[1:10],
  stringsAsFactors = FALSE
)

# Core Tests
test_that("gg1d returns a plot object", {
  result <- expect_no_error(gg1d(data = mock_data, col_id = "ID", verbose = FALSE))
  expect_s3_class(result, "girafe") # Assuming the output is interactive by default
})

test_that("gg1d handles missing col_id gracefully", {
  result <- expect_no_error(gg1d(data = mock_data, verbose = FALSE))
  expect_s3_class(result, "girafe")
})

test_that("gg1d filters columns based on maxlevels", {
  result <- gg1d(data = mock_data, maxlevels = 4, debug_return_col_info = TRUE, verbose = FALSE)
  expect_false("Category" %in% result$colnames[result$plottable])
})

test_that("gg1d applies ignore_column_regex", {
  data <- mock_data
  colnames(data)[2] <- "IgnoreMe_ignore"
  result <- gg1d(data = data, debug_return_col_info = TRUE, verbose = FALSE)
  expect_false("IgnoreMe_ignore" %in% result$colnames[result$plottable])
})

test_that("gg1d limits the number of plottable columns", {
  data <- mock_data
  for (i in 1:20) {
    data[[paste0("Col", i)]] <- rnorm(10)
  }
  expect_error(
    gg1d(data = data, max_plottable_cols = 10, verbose = FALSE),
    "Autoplotting > 10 fields by `gg1d` is not recommended"
  )
})

test_that("gg1d validates palettes input", {
  palettes <- list(
    Category = c(A = "red", B = "green", C = "blue", D = "black", E = "purple")
  )
  result <- gg1d(data = mock_data, palettes = palettes, debug_return_col_info = TRUE, verbose = FALSE)
  expect_equal(result$palette[[which(result$colnames == "Category")]], palettes$Category)

  # Throws error if missing colours for any values
  palettes_incomplete <- list(
    Category = c(A = "red", B = "green", C = "blue", D = "black", "purple")
  )
  expect_error(gg1d(data = mock_data, palettes = palettes_incomplete, verbose = FALSE), "missing 1 required name: `E`")
})


test_that("gg1d raises error for invalid column inputs", {
  expect_error(gg1d(data = mock_data, col_sort = "NonExistentCol", verbose = FALSE), "Column `NonExistentCol` does not exist in your dataset. Please set the `col_sort` argument to a valid column name.")
  expect_error(gg1d(data = mock_data, col_id = "NonExistentCol", verbose = FALSE), "Column `NonExistentCol` does not exist in your dataset. Please set the `col_id` argument to a valid column name.")
})


test_that("gg1d returns column information when debug_return_col_info is TRUE", {
  result <- gg1d(data = mock_data, debug_return_col_info = TRUE, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("colnames", "coltype", "plottable", "palette") %in% colnames(result)))
})

test_that("gg1d handles logical columns with default logical colors", {
  result <- gg1d(data = mock_data, debug_return_col_info = TRUE, verbose = FALSE)
  expect_equal(
    result$palette[[which(result$colnames == "Logical")]],
    c("TRUE" = "#648fff", "FALSE" = "#dc267f")
  )
})


test_that("gg1d gracefully handles non-plottable datasets", {
  data <- data.frame(category = LETTERS)
  expect_error(gg1d(data = data, verbose = FALSE), "No plottable columns found")
})


# Edge Cases
test_that("gg1d warns about too many unique levels in categorical data", {
  data <- data.frame(ID = 1:10, TooManyLevels = as.factor(1:10))
  suppressMessages(expect_message(
    gg1d(data = data, maxlevels = 5, debug_return_col_info = TRUE, verbose = TRUE),
    "must have <= 5 unique values"
  ))
})


test_that("gg1d can handle interactive and static plot settings", {
  interactive_plot <- gg1d(data = mock_data, interactive = TRUE, verbose = FALSE)
  expect_s3_class(interactive_plot, "girafe")

  static_plot <- gg1d(data = mock_data, interactive = FALSE, verbose = FALSE)
  expect_s3_class(static_plot, "ggplot")
})

#
test_that("gg1d correctly applies column tooltips", {
  data <- mock_data
  colnames(data)[5] <- "Category_tooltip"
  result <- gg1d(data = data, debug_return_col_info = TRUE, verbose = FALSE)
  expect_equal(result$coltooltip[[which(result$colnames == "Category")]], "Category_tooltip")
})
