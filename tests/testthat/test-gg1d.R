test_that("gg1d works as expected", {
  expect_error(suppressMessages(gg1d(mtcars)), NA)
})



test_that("gg1d works even if col only has 1 valid numeric column", {
  df <- data.frame(
    col1 = c(0, 0, NA, NA, NA, NA, NA, NA, 0, NA),
    col2 = c(0, 0, 0, 0, 0)
    )
  expect_error(suppressWarnings(suppressMessages(gg1d(df, cols_to_plot="col1"))), NA)
  expect_error(suppressMessages(gg1d(df, cols_to_plot="col2")), NA)
})


test_that("gg1d throws error if no plottable columns", {
  expect_error(gg1d(data.frame(), verbose = FALSE), "No plottable columns found")
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
    Letters = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
                "M", "N", "O", "P", "Q", "R", "S")
  )

  # Check the appropriate warning is thrown
  suppressMessages(expect_message(gg1d(df, verbose = TRUE), "Columns with too many unique values: Letters"))

  # If user only wants to plot glasses, there's no reason to warn about Letters
  suppressMessages(expect_no_message(gg1d(df, cols_to_plot = c("Glasses"), verbose = TRUE), message = "Columns with too many unique values: Letters"))
})


