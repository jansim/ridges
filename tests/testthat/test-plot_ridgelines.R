library(raster)

test_that("plot_ridgelines creates expected visualization", {
  vdiffr::expect_doppelganger(
    "default ridgeline plot",
    plot_ridgelines(ele_wilder_kaiser)
  )

  vdiffr::expect_doppelganger(
    "ridgeline plot without fill",
    plot_ridgelines(
      ele_wilder_kaiser,
      fill_color = NA,
      scale_factor = 12
    )
  )

  vdiffr::expect_doppelganger(
    "black on white ridgeline plot",
    plot_ridgelines(
      ele_wilder_kaiser,
      line_color = "#000000",
      fill_color = "white",
      background_color = "white",
      scale_factor = 8
    )
  )

  vdiffr::expect_doppelganger(
    "custom colored ridgeline plot",
    plot_ridgelines(
      ele_wilder_kaiser,
      n_lines = 35,
      line_color = "#FF4081",
      fill_color = "#FF408133",
      background_color = "#1A237E"
    )
  )
})
