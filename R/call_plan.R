#' Plan provider calls: for each loc_id, split by time sparsity
#' @keywords internal
.call_plan <- function(
  x,
  time_col = "timestamp_utc",
  loc_id_col = "loc_id",
  rep_lat_col = "rep_lat",
  rep_lon_col = "rep_lon",
  split_penalty_hours = 72,
  pad_hours = 0,
  max_parts = 50L,
  tz = "UTC"
) {
  DT <- data.table::as.data.table(x)
  if (!all(c(time_col, loc_id_col, rep_lat_col, rep_lon_col) %in% names(DT))) {
    stop("x must contain: ", paste(c(time_col, loc_id_col, rep_lat_col, rep_lon_col), collapse=", "))
  }
  DT[, t := get(time_col)]
  DT <- DT[!is.na(t)]
  DT[, t := as.POSIXct(t, tz = tz)]
  attr(DT$t, "tzone") <- tz

  out <- DT[, {
    seg <- .split_time_ranges(t, split_penalty_hours = split_penalty_hours, pad_hours = pad_hours, max_parts = max_parts)
    seg[, `:=`(
      loc_id = get(loc_id_col)[1],
      rep_lat = get(rep_lat_col)[1],
      rep_lon = get(rep_lon_col)[1]
    )]
    seg
  }, by = loc_id_col]

  data.table::setcolorder(out, c("loc_id","rep_lat","rep_lon","start_utc","end_utc","n_points"))
  out[]
}
