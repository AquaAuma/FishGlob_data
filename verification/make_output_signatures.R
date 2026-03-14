source(here::here("verification", "output_signatures_utils.R"))

baseline_path <- here::here("tests", "baselines", "output_signatures.csv")
signatures <- compute_output_signatures(progress = TRUE)
write_signatures(signatures, baseline_path)

cli::cli_alert_success("Wrote {nrow(signatures)} output signatures to tests/baselines/output_signatures.csv")
