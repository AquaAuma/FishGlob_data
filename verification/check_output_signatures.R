source(here::here("verification", "output_signatures_utils.R"))

baseline_path <- here::here("tests", "baselines", "output_signatures.csv")

if (!file.exists(baseline_path)) {
  cli::cli_abort("Baseline file tests/baselines/output_signatures.csv does not exist.")
}

expected <- read_signatures(baseline_path)
actual <- compute_output_signatures(progress = TRUE)
issues <- compare_signatures(expected, actual)

if (nrow(issues) == 0L) {
  cli::cli_alert_success("Output signatures match baseline across {nrow(actual)} files.")
} else {
  cli::cli_alert_danger(glue::glue("Found {nrow(issues)} signature mismatch(es)."))
  print(issues)
}

