#' @keywords internal
.utils_global_vars <- function() {
  utils::globalVariables(c(
    # data.table NSE / operators
    ":=", ".",
    # common temporary columns
    ".I", ".SD", ".row_id", "timestamp_raw", "timestamp_utc", "date", "hour",
    # spatial
    ".lat", ".lon", "lat", "lon",
    "rep_lat", "rep_lon", "rep_group", "loc_id", "dist_to_rep_m", "cluster_id",
    "rep_lat_r", "rep_lon_r",
    # cache / index
    "key", "time_api", "params", "site_elevation", "file", "format", "n_rows",
    "cache_path", "fetch_needed", "hit", "stale", "is_stale", "created_utc",
    "start_utc", "end_utc", "span_sec", "stale_rank",
    # POWER time cols
    "YEAR", "MO", "DY", "HR", "MM", "DD", "DOY", "YYYYMMDD",
    # join diagnostics
    "time_diff_hours", "i.timestamp_utc",
    # internal function sometimes flagged
    ".cache_check"
  ))
  invisible(TRUE)
}
.utils_global_vars()
