library(raster)

test_that("plot_elevation creates expected visualization", {
  vdiffr::expect_doppelganger(
    "default elevation plot",
    plot_elevation(ele_wilder_kaiser)
  )

  vdiffr::expect_doppelganger(
    "custom colored elevation plot",
    plot_elevation(
      ele_wilder_kaiser,
      low_color = "darkgreen",
      high_color = "yellow"
    )
  )
})