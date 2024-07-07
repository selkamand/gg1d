test_that("gg1d works as expected", {
  expect_error(suppressMessages(gg1d_plot(mtcars)), NA)
})



test_that("gg1d works even if col only has 1 valid numeric column", {
  df <- data.frame(
    col1 = c(0, 0, NA, NA, NA, NA, NA, NA, 0, NA),
    col2 = c(0, 0, 0, 0, 0)
    )
  expect_error(suppressWarnings(suppressMessages(gg1d_plot(df, cols_to_plot="col1"))), NA)
  expect_error(suppressMessages(gg1d_plot(df, cols_to_plot="col2")), NA)
})


test_that("gg1d throws error if no plottable columns", {
  expect_error(gg1d_plot(data.frame(), verbose = FALSE), "No plottable columns found")
})




