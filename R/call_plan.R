#' Plan provider calls: for each loc_id, split by time sparsity
#' @keywords internal
.call_plan <- function(
    x,
    time_col = "timestamp_utc",
    loc_id_col = "loc_id",
    rep_lat_col = "rep_lat",
    rep_lon_col = "rep_lon",
    tz = "UTC"
) {
  DT <- data.table::as.data.table(x)
  need <- c(time_col, loc_id_col, rep_lat_col, rep_lon_col)
  if (!all(need %in% names(DT))) stop("x must contain: ", paste(need, collapse = ", "))
  
  DT[, t := get(time_col)]
  DT <- DT[!is.na(t)]
  DT[, t := as.POSIXct(t, tz = tz)]
  attr(DT$t, "tzone") <- tz
  
  out <- DT[, {
    .split_time_ranges(times_utc = t)  # no args; reads options internally
  }, by = .(
    loc_id = get(loc_id_col),
    rep_lat = get(rep_lat_col),
    rep_lon = get(rep_lon_col)
  )]
  
  data.table::setcolorder(out, c("loc_id","rep_lat","rep_lon","start_utc","end_utc","n_points"))
  out[]
}
