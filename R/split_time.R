#' Split sparse time points into segments using a gap penalty (hours)
#' @keywords internal
.split_time_ranges <- function(times_utc, split_penalty_hours = 72, pad_hours = 0, max_parts = 50L) {
  if (!inherits(times_utc, "POSIXct")) stop("times_utc must be POSIXct.")
  t <- sort(unique(times_utc[!is.na(times_utc)]))
  if (length(t) == 0) {
    return(data.table::data.table(start_utc = as.POSIXct(NA, tz="UTC"),
                                  end_utc = as.POSIXct(NA, tz="UTC"),
                                  n_points = 0L))
  }
  if (length(t) == 1) {
    s <- t[1]; e <- t[1]
    if (pad_hours > 0) { s <- s - pad_hours*3600; e <- e + pad_hours*3600 }
    return(data.table::data.table(start_utc=s, end_utc=e, n_points=1L))
  }
  gaps_h <- as.numeric(diff(t), units="hours")
  cut_idx <- which(gaps_h > split_penalty_hours)
  starts <- c(1L, cut_idx + 1L)
  ends   <- c(cut_idx, length(t))
  if (length(starts) > max_parts) {
    starts <- 1L; ends <- length(t)
  }
  seg <- data.table::data.table(
    start_utc = t[starts],
    end_utc   = t[ends],
    n_points  = ends - starts + 1L
  )
  if (pad_hours > 0) {
    seg[, `:=`(start_utc = start_utc - pad_hours*3600,
               end_utc   = end_utc   + pad_hours*3600)]
  }
  seg[]
}
