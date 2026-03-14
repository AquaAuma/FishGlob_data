output_files <- function() {
  sort(c(
    list.files(here::here("outputs", "Cleaned_data"), pattern = "[.]RData$", full.names = TRUE),
    list.files(here::here("outputs", "Compiled_data"), pattern = "[.]RData$", full.names = TRUE)
  ))
}

load_output_data <- function(path) {
  e <- new.env(parent = emptyenv())
  load(path, envir = e)

  if (!exists("data", envir = e, inherits = FALSE) || !is.data.frame(e$data)) {
    cli::cli_abort(glue::glue("Expected a data frame called `data` in {path}"))
  }

  e$data
}

short_output_path <- function(path) {
  sub(
    paste0("^", paste0(normalizePath(here::here(), winslash = "/", mustWork = TRUE), "/")),
    "",
    normalizePath(path, winslash = "/", mustWork = TRUE)
  )
}

count_distinct <- function(data, column) {
  if (is.null(column) || !column %in% names(data)) {
    return("")
  }

  as.character(length(unique(data[[column]][!is.na(data[[column]])])))
}

count_na <- function(data, column) {
  if (!column %in% names(data)) {
    return("")
  }

  as.character(sum(is.na(data[[column]])))
}

year_stat <- function(data, stat) {
  if (!"year" %in% names(data)) {
    return("")
  }

  years <- suppressWarnings(as.numeric(data$year))
  years <- years[!is.na(years)]

  if (length(years) == 0L) {
    return("")
  }

  as.character(stat(years))
}

mean_numeric <- function(data, column) {
  if (!column %in% names(data)) {
    return("")
  }

  values <- suppressWarnings(as.numeric(data[[column]]))
  values <- values[!is.na(values)]

  if (length(values) == 0L) {
    return("")
  }

  format(mean(values), digits = 4L, trim = TRUE, scientific = FALSE, na.rm = TRUE)
}

signature_for_file <- function(path) {
  data <- load_output_data(path)
  classes <- vapply(data, function(x) paste(class(x), collapse = "/"), character(1L))
  root <- paste0(normalizePath(here::here(), winslash = "/", mustWork = TRUE), "/")
  file <- sub(paste0("^", root), "", normalizePath(path, winslash = "/", mustWork = TRUE))
  aphia_col <- intersect(c("AphiaID", "aphia_id"), names(data))[1]

  data.frame(
    file = file,
    rows = as.character(nrow(data)),
    cols = as.character(ncol(data)),
    column_names = paste(names(data), collapse = "|"),
    column_classes = paste(classes, collapse = "|"),
    distinct_survey = count_distinct(data, "survey"),
    distinct_survey_unit = count_distinct(data, "survey_unit"),
    distinct_haul_id = count_distinct(data, "haul_id"),
    distinct_taxon = count_distinct(data, "taxon"),
    distinct_accepted_name = count_distinct(data, "accepted_name"),
    distinct_aphia = count_distinct(data, aphia_col),
    year_min = year_stat(data, min),
    year_max = year_stat(data, max),
    na_latitude = count_na(data, "latitude"),
    na_longitude = count_na(data, "longitude"),
    na_haul_id = count_na(data, "haul_id"),
    na_taxon = count_na(data, "taxon"),
    mean_num_cpue = mean_numeric(data, "num_cpue"),
    mean_num_cpua = mean_numeric(data, "num_cpua"),
    mean_wgt = mean_numeric(data, "wgt"),
    mean_wgt_cpue = mean_numeric(data, "wgt_cpue"),
    mean_wgt_cpua = mean_numeric(data, "wgt_cpua"),
    mean_area_swept = mean_numeric(data, "area_swept"),
    mean_haul_dur = mean_numeric(data, "haul_dur"),
    stringsAsFactors = FALSE
  )
}

compute_output_signatures <- function(progress = FALSE) {
  files <- output_files()

  if (length(files) == 0L) {
    cli::cli_abort("No output .RData files found.")
  }

  signatures <- vector("list", length(files))
  progress_bar <- NULL

  if (progress) {
    progress_bar <- cli::cli_progress_bar(
      name = "",
      total = length(files),
      clear = FALSE
    )
  }

  for (i in seq_along(files)) {
    file <- files[[i]]

    if (progress) {
      short_file <- short_output_path(file)
      cli::cli_progress_update(id = progress_bar, set = i - 1L, status = short_file)
    }

    signatures[[i]] <- signature_for_file(file)

    if (progress) {
      cli::cli_progress_update(id = progress_bar, set = i, status = short_file)
    }
  }

  if (progress) {
    cli::cli_progress_done(id = progress_bar)
  }

  signatures <- do.call(rbind, signatures)
  signatures[order(signatures$file), , drop = FALSE]
}

read_signatures <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = "")
}

write_signatures <- function(signatures, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(signatures, path, row.names = FALSE, na = "")
}

compare_signatures <- function(expected, actual) {
  if (!identical(names(expected), names(actual))) {
    cli::cli_abort("Baseline and current signatures have different columns.")
  }

  expected <- expected[order(expected$file), , drop = FALSE]
  actual <- actual[order(actual$file), , drop = FALSE]

  if (!identical(expected$file, actual$file)) {
    expected_only <- setdiff(expected$file, actual$file)
    actual_only <- setdiff(actual$file, expected$file)

    return(
      data.frame(
        file = c(expected_only, actual_only),
        field = "file",
        expected = c(rep("present", length(expected_only)), rep("absent", length(actual_only))),
        actual = c(rep("missing", length(expected_only)), rep("present", length(actual_only))),
        stringsAsFactors = FALSE
      )
    )
  }

  expected_mat <- as.matrix(data.frame(lapply(expected, as.character), stringsAsFactors = FALSE, check.names = FALSE))
  actual_mat <- as.matrix(data.frame(lapply(actual, as.character), stringsAsFactors = FALSE, check.names = FALSE))
  same <- (expected_mat == actual_mat) | (is.na(expected_mat) & is.na(actual_mat))
  mismatches <- which(!same, arr.ind = TRUE)

  if (nrow(mismatches) == 0L) {
    return(data.frame(file = character(), field = character(), expected = character(), actual = character(), stringsAsFactors = FALSE))
  }

  data.frame(
    file = actual_mat[mismatches[, 1L], "file"],
    field = colnames(actual_mat)[mismatches[, 2L]],
    expected = expected_mat[mismatches],
    actual = actual_mat[mismatches],
    stringsAsFactors = FALSE
  )
}
