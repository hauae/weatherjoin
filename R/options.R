#' Get weatherjoin option with default
#' @keywords internal
.wj_opt <- function(name, default) {
  getOption(paste0("weatherjoin.", name), default)
}
