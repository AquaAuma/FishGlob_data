setwd(here::here())

testthat::test_dir(here::here("tests", "testthat"), reporter = "summary")
