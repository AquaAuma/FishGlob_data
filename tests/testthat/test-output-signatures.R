source(here::here("verification", "output_signatures_utils.R"))

testthat::test_that("cached output signatures match the committed baseline", {
  baseline_path <- here::here("tests", "baselines", "output_signatures.csv")

  testthat::expect_true(
    file.exists(baseline_path),
    info = "Run verification/make_output_signatures.R to create the baseline file."
  )

  expected <- read_signatures(baseline_path)
  actual <- compute_output_signatures()
  issues <- compare_signatures(expected, actual)

  issue_summary <- if (nrow(issues) == 0L) {
    NULL
  } else {
    paste(capture.output(print(head(issues, 20L))), collapse = "\n")
  }

  testthat::expect_equal(
    nrow(issues),
    0L,
    info = issue_summary
  )
})
