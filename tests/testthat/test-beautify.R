test_that("beautify works as expected", {
  # Names are original values, values, are what beautify should them into
  examples <- c(
    "species" = "Species",
    "island" = "Island",
    "bill_length_mm" = "Bill Length (mm)",
    "bill_depth_mm" = "Bill Depth (mm)",
    "flipper_length_mm" = "Flipper Length (mm)",
    "body_mass_g" = "Body Mass (g)",
    "kgtest_kg" = "Kgtest (kg)",
    "sex" = "Sex",
    "Length (mm)" = "Length (mm)"
  )

  expect_equal(examples, beautify(names(examples)), ignore_attr = TRUE)
})
