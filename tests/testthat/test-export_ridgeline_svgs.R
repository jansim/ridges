test_that("export_ridgeline_svgs creates expected SVG files", {
  # Create temporary directory that gets cleaned up automatically
  temp_dir <- withr::local_tempdir("ridgelines_test")

  # Export 3 SVGs
  files <- export_ridgeline_svgs(
    elevation = ele_wilder_kaiser,
    n_lines = 3,
    output_dir = temp_dir,
    width = 400,
    height = 100
  )

  # Test number of files created
  expect_length(files, 3)
  expect_true(all(file.exists(files)))

  # Test file naming pattern
  expect_match(basename(files), "^ridgeline_\\d{3}\\.svg$")
  expect_equal(
    basename(files),
    c("ridgeline_001.svg", "ridgeline_002.svg", "ridgeline_003.svg")
  )

  # Test file contents (basic check that files are valid SVGs)
  first_file <- readLines(files[1])
  expect_true(any(grepl("<svg", first_file)))
  expect_true(any(grepl("</svg>", first_file)))

  # Snapshot test of the first SVG
  expect_snapshot_file(files[1], "ridgeline_001.svg")
})
