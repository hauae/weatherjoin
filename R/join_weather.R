#' Join gridded weather data to an event table
#'
#' Attach gridded weather variables to rows of an event table using spatial planning,
#' optional time-range splitting, local caching, and exact/rolling joins.
#'
#' Currently supports NASA POWER via the \pkg{nasapower} package.
#'
#' @param x A data.frame/data.table with event rows.
#' @param params Character vector of NASA POWER parameter codes (e.g. \code{"T2M"}).
#' @param time A single column name containing time (POSIXct/Date/character/numeric) OR
#'   a character vector of column names used to assemble a timestamp (e.g. \code{c("YEAR","MO","DY","HR")}).
#' @param provider Weather data provider. Currently only \code{"power"} is implemented.
#' @param time_api One of \code{"guess"}, \code{"hourly"}, \code{"daily"}. If \code{"daily"} is chosen
#'   while input is hourly, timestamps are downsampled to dates; if \code{"hourly"} is chosen while input
#'   has no time-of-day, an error is raised.
#' @param lat_col,lon_col Column names for latitude and longitude.
#' @param tz Time zone used to interpret/construct timestamps (default \code{"UTC"}).
#' @param dummy_hour Hour used for daily timestamps (default 12). Only used when \code{time_api="daily"}.
#' @param roll Join behavior when matching hourly timestamps: \code{"nearest"}, \code{"last"}, or \code{"none"} (exact).
#' @param roll_max_hours Optional maximum allowed distance (in hours) for a rolling match; farther matches become NA.
#' @param spatial_mode How to reduce many points to representative locations before calling the provider:
#'   \code{"exact"}, \code{"by_group"}, or \code{"cluster"}.
#' @param group_col Grouping column used when \code{spatial_mode="by_group"}.
#' @param rep_method Representative location method: \code{"median"} or \code{"centroid"}.
#' @param cluster_radius_m Clustering radius in meters when \code{spatial_mode="cluster"}.
#' @param keep_rep_cols If TRUE, keep representative-location diagnostics (rep_lon/rep_lat, distance, elevation).
#' @param split_penalty_hours Gap threshold (hours) to split sparse time series into separate API calls.
#' @param pad_hours Extra hours padded on both sides of each planned call window.
#' @param max_parts Maximum number of time-window parts per location (safety cap).
#' @param cache_dir Optional explicit cache directory. If NULL, determined by \code{cache_scope}.
#' @param cache_scope Where to store cache by default: \code{"user"} or \code{"project"}.
#' @param pkg Package name used when \code{cache_scope="user"}.
#' @param cache_max_age_days Cache entries older than this (days) are considered stale.
#' @param refresh When to refetch: \code{"if_missing"}, \code{"if_stale"}, or \code{"always"}.
#' @param match_mode Cache matching mode: \code{"cover"} (cached window covers requested) or \code{"exact"}.
#' @param param_match Parameter matching for cache reuse: \code{"superset"} (default) allows cached files with extra params.
#' @param community Passed to \code{nasapower::get_power()}.
#' @param time_standard Passed to \code{nasapower::get_power()} (default \code{"UTC"}).
#' @param site_elevation Elevation strategy: \code{"auto"} or \code{"constant"} (or a numeric scalar/vector).
#' @param elev_constant Constant elevation (meters) when \code{site_elevation="constant"} or default for \code{"auto"}.
#' @param elev_fun Optional function \code{function(lon, lat, ...)} returning elevation (meters) for representative points.
#' @param verbose If TRUE, print progress messages.
#' @param ... Passed through to \code{nasapower::get_power()}.
#' @return A data.table with weather columns appended.
#' @export
join_weather <- function(
  x,
  params,
  time,
  provider = c("power"),
  time_api = c("guess", "hourly", "daily"),
  lat_col = "lat",
  lon_col = "lon",
  tz = "UTC",
  dummy_hour = 12L,
  roll = c("nearest", "last", "none"),
  roll_max_hours = NULL,
  spatial_mode = c("exact", "by_group", "cluster"),
  group_col = NULL,
  rep_method = c("median", "centroid"),
  cluster_radius_m = NULL,
  keep_rep_cols = FALSE,
  split_penalty_hours = 72,
  pad_hours = 0,
  max_parts = 50L,
  cache_dir = NULL,
  cache_scope = c("user", "project"),
  pkg = "weatherjoin",
  cache_max_age_days = 30,
  refresh = c("if_missing", "if_stale", "always"),
  match_mode = c("cover", "exact"),
  param_match = c("superset", "exact"),
  community = "ag",
  time_standard = "UTC",
  site_elevation = c("auto", "constant"),
  elev_constant = 100,
  elev_fun = NULL,
  verbose = FALSE,
  ...
) {
  .wj_load(attach = FALSE)
  provider <- match.arg(provider)
  if (provider != "power") stop("Only provider='power' is currently supported.")

  DT <- data.table::as.data.table(x)

  roll <- match.arg(roll)
  time_api <- match.arg(time_api)
  spatial_mode <- match.arg(spatial_mode)
  rep_method <- match.arg(rep_method)
  cache_scope <- match.arg(cache_scope)
  refresh <- match.arg(refresh)
  match_mode <- match.arg(match_mode)
  param_match <- match.arg(param_match)

  if (!all(c(lat_col, lon_col) %in% names(DT))) stop("x must contain columns: ", lat_col, ", ", lon_col)
  if (is.null(time) || length(time) < 1) stop("Argument 'time' must be provided (column name or vector of column names).")

  if (!is.null(group_col) && spatial_mode == "exact") {
    if (verbose) message("group_col provided: switching spatial_mode from 'exact' to 'by_group'.")
    spatial_mode <- "by_group"
  }

  DT[, .row_id := .I]

  key_na <- is.na(DT[[lat_col]]) | is.na(DT[[lon_col]])
  if (length(time) == 1L) {
    if (!(time %in% names(DT))) stop("time column not found in x: ", time)
    key_na <- key_na | is.na(DT[[time]])
  } else {
    if (!all(time %in% names(DT))) stop("Missing time columns: ", paste(setdiff(time, names(DT)), collapse=", "))
    key_na <- key_na | Reduce(`|`, lapply(time, function(nm) is.na(DT[[nm]])))
  }

  DT_ok  <- DT[!key_na]
  DT_bad <- DT[key_na]

  if (verbose && nrow(DT_bad) > 0) message("Keeping ", nrow(DT_bad), " rows with missing lon/lat/time inputs; weather will be NA.")

  req_params <- unique(toupper(trimws(params)))
  req_params <- req_params[nzchar(req_params)]

  if (nrow(DT_ok) == 0L) {
    out_bad <- data.table::copy(DT_bad)
    out_bad[, `:=`(timestamp_utc = as.POSIXct(NA, tz = tz), loc_id = NA_integer_)]
    for (p in req_params) out_bad[, (p) := NA_real_]
    data.table::setorder(out_bad, .row_id)
    out_bad[, .row_id := NULL]
    return(out_bad[])
  }

  bt <- .build_time(DT_ok, time = time, tz = tz)
  DT_ok <- bt$dt

  bad_ts <- is.na(DT_ok$timestamp_raw)
  if (any(bad_ts)) {
    DT_bad <- data.table::rbindlist(list(DT_bad, DT_ok[bad_ts]), use.names=TRUE, fill=TRUE)
    DT_ok  <- DT_ok[!bad_ts]
    if (verbose) message("Keeping ", sum(bad_ts), " rows with bad timestamps; weather will be NA.")
  }
  if (nrow(DT_ok) == 0L) {
    out_bad <- data.table::copy(DT_bad)
    out_bad[, `:=`(timestamp_utc = as.POSIXct(NA, tz = tz), loc_id = NA_integer_)]
    for (p in req_params) out_bad[, (p) := NA_real_]
    data.table::setorder(out_bad, .row_id)
    out_bad[, .row_id := NULL]
    return(out_bad[])
  }

  rt <- .resolve_time_api(DT_ok, time_api = time_api, input_res = bt$input_res, tz = tz, dummy_hour = dummy_hour)
  DT_ok <- rt$dt
  time_api_resolved <- rt$time_api_resolved

  sp <- .spatial_plan(
    DT_ok,
    spatial_mode = spatial_mode,
    lat_col = lat_col,
    lon_col = lon_col,
    group_col = group_col,
    rep_method = rep_method,
    cluster_radius_m = cluster_radius_m,
    keep_diag = TRUE
  )
  mapped <- data.table::as.data.table(sp$mapped)

  reps <- unique(mapped[, .(loc_id, rep_lon, rep_lat)])
  elev_mode <- if (is.numeric(site_elevation)) "numeric" else as.character(site_elevation[1])

  if (elev_mode == "numeric") {
    if (length(site_elevation) == 1L) reps[, site_elevation := as.numeric(site_elevation)]
    else if (length(site_elevation) == nrow(reps)) reps[, site_elevation := as.numeric(site_elevation)]
    else stop("If site_elevation is numeric, it must be length 1 or length equal to number of representative locations.")
  } else if (elev_mode == "constant") {
    reps[, site_elevation := as.numeric(elev_constant)]
  } else if (elev_mode == "auto") {
    ef <- elev_fun
    if (is.null(ef)) ef <- function(lon, lat, ...) .elev_lookup(lon, lat, method="constant", constant=elev_constant, ...)
    reps[, site_elevation := as.numeric(ef(rep_lon, rep_lat, ...))]
  } else stop("site_elevation must be numeric, or one of: 'auto', 'constant'.")

  mapped <- reps[mapped, on="loc_id"]

  plan <- .call_plan(
    x = mapped,
    time_col = "timestamp_utc",
    loc_id_col = "loc_id",
    rep_lat_col = "rep_lat",
    rep_lon_col = "rep_lon",
    split_penalty_hours = split_penalty_hours,
    pad_hours = pad_hours,
    max_parts = max_parts,
    tz = tz
  )
  plan <- reps[plan, on="loc_id"]
  plan <- unique(plan, by = c("rep_lat","rep_lon","site_elevation","start_utc","end_utc"))

  chk <- .cache_check(
    calls = plan,
    time_api = time_api_resolved,
    params = params,
    settings = list(provider = provider, community = community, time_standard = time_standard),
    cache_dir = cache_dir,
    cache_scope = cache_scope,
    pkg = pkg,
    cache_max_age_days = cache_max_age_days,
    refresh = refresh,
    match_mode = match_mode,
    param_match = param_match
  )

  fetched <- NULL
  if (nrow(chk$to_fetch) > 0) {
    to_fetch <- data.table::as.data.table(chk$to_fetch)
    f <- .fetch_power(
      calls_to_fetch = to_fetch,
      time_api = time_api_resolved,
      params = params,
      community = community,
      time_standard = time_standard,
      settings = list(provider = provider, community = community, time_standard = time_standard),
      cache_dir = cache_dir,
      cache_scope = cache_scope,
      pkg = pkg,
      dummy_hour = dummy_hour,
      verbose = verbose,
      ...
    )
    fetched <- f$fetched
  } else if (verbose) {
    message("All requested segments satisfied by cache.")
  }

  out_ok <- .materialize_and_attach(
    x_mapped = mapped,
    chk = chk,
    fetched = fetched,
    params = params,
    roll = roll,
    roll_max_hours = roll_max_hours
  )

  if (!keep_rep_cols) {
    drop <- intersect(c("rep_lat","rep_lon","dist_to_rep_m","site_elevation"), names(out_ok))
    if (length(drop)) out_ok[, (drop) := NULL]
  }

  for (p in req_params) if (!(p %in% names(out_ok))) out_ok[, (p) := NA_real_]
  out_bad <- data.table::copy(DT_bad)
  out_bad[, `:=`(timestamp_utc = as.POSIXct(NA, tz = tz), loc_id = NA_integer_)]
  for (p in req_params) out_bad[, (p) := NA_real_]

  miss_bad <- setdiff(names(out_ok), names(out_bad))
  for (nm in miss_bad) out_bad[, (nm) := NA]
  miss_ok <- setdiff(names(out_bad), names(out_ok))
  for (nm in miss_ok) out_ok[, (nm) := NA]
  out_bad <- out_bad[, names(out_ok), with=FALSE]

  out_all <- data.table::rbindlist(list(out_ok, out_bad), use.names=TRUE, fill=TRUE)
  data.table::setorder(out_all, .row_id)
  out_all[, .row_id := NULL]
  out_all[]
}
