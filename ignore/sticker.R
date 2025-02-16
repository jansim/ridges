library(hexSticker)
library(terra)
library(sf)
library(ridges)

# Get elevation data
elevation <- get_elevation(bb_wilder_kaiser)

# Create the ridgeline plot with specific styling for the sticker
p <- plot_ridgelines(
  elevation = elevation,
  scale_factor = 13,
  line_color = "#FFFFFF",
  fill_color = "#FFFFFF00",
  linewidth = .25
) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
    panel.background = ggplot2::element_rect(fill = "transparent", color = NA)
  )

# Create the hex sticker
sticker(
  subplot = p,
  package = "ridges",
  p_y = 1.5,
  p_size = 17,
  p_color = "white",
  p_family = "mono",
  s_x = 1,
  s_y = 0.65,
  s_width = 2.5,
  s_height = 1.6,
  h_fill = "#1a1a1a",
  h_color = "white",
  filename = "inst/figures/logo.png",
  white_around_sticker = TRUE,
  dpi = 300
)
