#' Internal: load required packages (used for interactive sourcing too)
#' @keywords internal
.wj_load <- function(
  pkgs = c("data.table"),
  attach = FALSE,
  quiet = TRUE
) {
  optional <- c("fst", "digest", "nasapower", "anytime")
  needed <- unique(c(pkgs, optional))
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  missing_required <- setdiff(missing, optional)
  if (length(missing_required)) {
    stop("Missing required packages: ", paste(missing_required, collapse = ", "),
         "\nInstall them with install.packages().")
  }
  if (attach) {
    for (p in setdiff(needed, missing)) {
      suppressPackageStartupMessages(library(p, character.only = TRUE))
    }
  }
  if (!quiet) {
    opt_miss <- intersect(optional, missing)
    if (length(opt_miss)) message("Optional packages not installed (OK): ", paste(opt_miss, collapse = ", "))
  }
  invisible(TRUE)
}

#' @keywords internal
.canon_params <- function(params) {
  p <- sort(unique(toupper(trimws(params))))
  p <- p[nzchar(p)]
  paste(p, collapse = "|")
}

#' @keywords internal
.round_coord <- function(x, digits = 5) round(as.numeric(x), digits)
