#' Check and Load Multiple R Packages
#'
#'Author: Juliano Palacios Abrantes
#'Date: August, 2025
#' This function loads the packages listed in `pkg_list` into the current R session.
#' If any package cannot be loaded, it stops with an instruction to restore the
#' project environment with `renv`.
#' 
#'@param pkg_list A character vector of package names to be checked and loaded.
#' @details
#' The function:
#' \enumerate{
#'   \item Attempts to load all packages in \code{pkg_list} using \code{require()}.
#'   \item Stops with a clear error if any package cannot be loaded.
#' }
#'
#' @return 
#' A logical vector (invisibly) indicating the success of loading each package, as returned by \code{sapply()} over \code{require()}.
#'
#' @examples
#' \dontrun{
#' check_pkg(c("dplyr", "ggplot2", "sf"))
#' }
#'
#' @seealso \code{\link[base]{require}}
#'
#' @export

check_pkg <- function(pkg_list){
  success <- suppressMessages(
    sapply(pkg_list, require, character.only = TRUE)
  )

  if (any(!success)) {
    stop(
      paste0(
        "Could not load required package(s): ",
        paste(pkg_list[!success], collapse = ", "),
        ". Restore the project environment with renv::restore() and rerun."
      ),
      call. = FALSE
    )
  }

  invisible(success)
}
