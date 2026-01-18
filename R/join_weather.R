#' Join gridded weather data to an event table
#'
#' Attach gridded weather variables to rows of an event table using spatial planning,
#' local caching, and exact/rolling joins.
#'
#' The current provider is NASA POWER, accessed via the \pkg{nasapower} package.
#' The package requests POWER data in UTC and performs all internal planning and matching in UTC.
#'
#' @param x A data.frame/data.table with event rows.
#' @param params Character vector of NASA POWER parameter codes (e.g. \code{"T2M"}).
#' @param time A single column name containing time (POSIXct/Date/character/numeric) OR
#'   a character vector of column names used to assemble a timestamp
#'   (e.g. \code{c("YEAR","MO","DY","HR")}).
#' @param lat_col,lon_col Column names for latitude and longitude.
#' @param time_api One of \code{"guess"}, \code{"hourly"}, \code{"daily"}.
#'   If \code{"daily"} is chosen while input is hourly, timestamps are downsampled to dates.
#'   If \code{"hourly"} is chosen while input has no time-of-day, an error is raised.
#' @param tz Time zone used to interpret/construct input timestamps (default \code{"UTC"}).
#'   Input times are standardised internally to UTC.
#' @param roll Join behavior when matching hourly timestamps: \code{"nearest"} (default),
#'   \code{"last"}, or \code{"none"} (exact).
#' @param roll_max_hours Optional maximum allowed distance (in hours) for a rolling match;
#'   farther matches become NA.
#' @param spatial_mode How to reduce many points to representative locations before calling the provider:
#'   \code{"exact"}, \code{"by_group"}, or \code{"cluster"}.
#' @param group_col Grouping column used when \code{spatial_mode="by_group"}.
#' @param cluster_radius_m Clustering radius in meters when \code{spatial_mode="cluster"}.
#' @param cache_scope Where to store cache by default: \code{"user"} or \code{"project"}.
#' @param cache_dir Optional explicit cache directory. If NULL, determined by \code{cache_scope}.
#' @param community Passed to \code{nasapower::get_power()} (e.g. \code{"ag"}).
#' @param site_elevation Elevation strategy: \code{"auto"} or \code{"constant"} (or a numeric scalar/vector).
#' @param elev_constant Constant elevation (meters) when \code{site_elevation="constant"} or default for \code{"auto"}.
#' @param elev_fun Optional function \code{function(lon, lat, ...)} returning elevation (meters)
#'   for representative points.
#' @param verbose If TRUE, print progress messages.
#' @param ... Passed through to \code{nasapower::get_power()}.
#'
#' @details
#' Advanced behaviour is controlled via package options; see \code{\link{weatherjoin_options}}.
#' In particular:
#' \itemize{
#' \item Cache policy (staleness and reuse rules).
#' \item Time-window planning and splitting for sparse time series.
#' \item Daily \dQuote{dummy hour} used when constructing timestamps for daily data.
#' \item Optional diagnostic columns in outputs.
#' }
#'
#' @return A data.table with requested weather columns appended.
#' @export
join_weather <- function(
    x,
    params,
    time,
    
    # Input mapping
    lat_col = "lat",
    lon_col = "lon",
    
    # Time handling
    time_api = c("guess", "hourly", "daily"),
    tz = "UTC",
    
    # Hourly join behaviour
    roll = c("nearest", "last", "none"),
    roll_max_hours = NULL,
    
    # Spatial planning
    spatial_mode = c("exact", "by_group", "cluster"),
    group_col = NULL,
    cluster_radius_m = NULL,
    
    # Cache location (policy is via options)
    cache_scope = c("user", "project"),
    cache_dir = NULL,
    
    # POWER-specific
    community = "ag",
    
    # Elevation (kept exposed)
    site_elevation = c("auto", "constant"),
    elev_constant = 100,
    elev_fun = NULL,
    
    verbose = FALSE,
    ...
) {
  .wj_load(attach = FALSE)
  
  # --- match.arg() for user-facing enums ---
  time_api     <- match.arg(time_api)
  roll         <- match.arg(roll)
  spatial_mode <- match.arg(spatial_mode)
  cache_scope  <- match.arg(cache_scope)
  site_elevation <- match.arg(site_elevation)
  
  # --- hardcoded provider decisions ---
  provider <- "power"
  time_standard <- "UTC"
  rep_method <- "centroid"
  
  # --- pull internal knobs from options ---
  dummy_hour   <- as.integer(.wj_opt("dummy_hour", 12))
  keep_rep_cols <- isTRUE(.wj_opt("keep_rep_cols", FALSE))
  
  # cache policy knobs (as in your options_doc)
  pkg <- .wj_opt("cache_pkg", "weatherjoin")
  cache_max_age_days <- .wj_opt("cache_max_age_days", 30)
  
  refresh <- match.arg(.wj_opt("cache_refresh", "if_missing"),
                       c("if_missing", "if_stale", "always"))
  match_mode <- match.arg(.wj_opt("cache_match_mode", "cover"),
                          c("cover", "exact"))
  param_match <- match.arg(.wj_opt("cache_param_match", "superset"),
                           c("superset", "exact"))
  
  DT <- data.table::as.data.table(x)
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

  # ---- Infer input time resolution capability (schema/type-based, not content-based) ----
  # input_res means: does the input contain time-of-day info?
  if (length(time) == 1L) {
    raw <- DT_ok[[time[[1L]]]]
    if (inherits(raw, "Date")) {
      input_res <- "daily"
    } else if (is.numeric(raw)) {
      # numeric is treated as YYYYMMDD in this package (epoch numeric is rejected)
      input_res <- "daily"
    } else if (inherits(raw, "POSIXt")) {
      input_res <- "hourly"
    } else {
      # character/factor: may contain time-of-day, so we treat as hour-capable
      input_res <- "hourly"
    }
  } else {
    spec <- .map_time_columns(time_cols = time, names_x = names(DT_ok))
    input_res <- if (!is.na(spec$hour)) "hourly" else "daily"
  }
  
  # ---- Resolve/enforce requested API ----
  # Rules (as agreed):
  # - guess -> input_res
  # - user can force daily even if hourly-capable (OK)
  # - user cannot force hourly if no hour info (error)
  if (time_api == "guess") {
    time_api_resolved <- input_res
  } else if (time_api == "daily") {
    time_api_resolved <- "daily"
  } else { # time_api == "hourly"
    if (input_res != "hourly") {
      stop(
        "time_api='hourly' was requested, but the input time has no hour information.\n",
        "Provide a POSIXct timestamp column, a datetime string column, or multi-column time including HR."
      )
    }
    time_api_resolved <- "hourly"
  }
  
  # ---- Build validated timestamp_utc and numeric join key t_utc ----
  # This enforces time_api_resolved (e.g., Date + hourly => clear error)
  DT_ok <- .build_time(
    DT = DT_ok,
    time = time,
    tz = tz,
    time_api_resolved = time_api_resolved
  )
  
  # If user forced daily, downsample timestamps to date + dummy_hour (safe & explicit)
  if (time_api_resolved == "daily") {
    DT_ok[, timestamp_utc := as.POSIXct(as.Date(timestamp_utc, tz = "UTC"), tz = "UTC") +
            as.integer(dummy_hour) * 3600]
    attr(DT_ok$timestamp_utc, "tzone") <- "UTC"
    DT_ok[, t_utc := as.numeric(timestamp_utc)]
  }

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
