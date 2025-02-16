library(raster)

test_that("calculate_ridgelines returns expected data structure", {
  # Calculate ridgelines with default parameters
  result <- calculate_ridgelines(ele_wilder_kaiser)

  # Test structure
  expect_snapshot({
    str(head(result))
    range(result$x)
    range(result$y)
    range(result$elevation)
    range(result$group)
    nrow(result)
    length(unique(result$group))
  })

  # Test with different number of lines
  result_15_lines <- calculate_ridgelines(ele_wilder_kaiser, n_lines = 15)
  expect_snapshot({
    length(unique(result_15_lines$group))
    nrow(result_15_lines)
  })
})

test_that("calculate_ridgelines handles edge cases", {
  # Test error when elevation is NULL and no last elevation
  expect_error(
    calculate_ridgelines(elevation = NULL),
    "No elevation data available."
  )
})
