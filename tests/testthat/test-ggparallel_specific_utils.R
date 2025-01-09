# example_data <- data.frame(
#   var1 = c(50, 30, 42, 46),
#   var2 = c(100, 115, 108, 108)
# )
#
# example_data <- data.frame(
#   var1 = c(42, 46),
#   var2 = c(108, 108)
# )

test_that("count_edge_crossings works", {
  expect_equal(count_edge_crossings(l = c(50, 30), r = c(100, 115)), 1)
  expect_equal(count_edge_crossings(l = c(50, 30, 40), r = c(100, 115, 40)), 2)
  expect_equal(count_edge_crossings(l = c(50, 30, 40), r = c(100, 115, 108)), 3)
})



test_that("no warnings when lines go to the same point (caused by rescaling issues)", {

  example_data <- data.frame(
    var1 = c(42, 46),
    var2 = c(108, 108)
  )

  expect_no_warning(ggparallel(example_data, verbose = FALSE))
})
