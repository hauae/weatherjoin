#' Placeholder elevation lookup
#' @keywords internal
.elev_lookup <- function(lon, lat, method = c("constant"), constant = 100, ...) {
  method <- match.arg(method)
  if (method == "constant") return(rep(as.numeric(constant), length(lon)))
  rep(as.numeric(constant), length(lon))
}
