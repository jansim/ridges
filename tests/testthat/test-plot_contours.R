library(raster)

test_that("plot_contours creates expected visualizations", {
  # Test default contour plot
  vdiffr::expect_doppelganger(
    "default contour plot",
    plot_contours(ele_wilder_kaiser)
  )

  # Test custom interval and line appearance
  vdiffr::expect_doppelganger(
    "custom contour plot",
    plot_contours(
      ele_wilder_kaiser,
      interval = 50,
      line_color = "darkred",
      linewidth = 0.5
    )
  )

  # Test colored contour lines
  vdiffr::expect_doppelganger(
    "colored contour lines",
    plot_contours(
      ele_wilder_kaiser,
      color_by_elevation = "lines",
      low_color = "darkgreen",
      high_color = "yellow"
    )
  )

  # Test filled contours
  vdiffr::expect_doppelganger(
    "filled contours",
    plot_contours(
      ele_wilder_kaiser,
      color_by_elevation = "fill",
      low_color = "darkblue",
      high_color = "darkred",
      fill_alpha = 0.7
    )
  )
})

test_that("plot_contours handles invalid inputs appropriately", {
  expect_error(
    plot_contours(color_by_elevation = "invalid"),
    'color_by_elevation must be one of: "none", "lines", "fill", "both"'
  )

  expect_error(
    plot_contours(NULL),
    "No elevation data available. Please run get_elevation\\(\\) first."
  )
})
