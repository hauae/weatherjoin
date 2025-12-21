#' Fetch NASA POWER for planned calls
#' @keywords internal
.fetch_power <- function(
  calls_to_fetch,
  time_api,
  params,
  community = "ag",
  time_standard = "UTC",
  settings = list(),
  cache_dir = NULL,
  cache_scope = c("user","project"),
  pkg = "weatherjoin",
  dummy_hour = 12L,
  verbose = FALSE,
  ...
) {
  if (!requireNamespace("nasapower", quietly = TRUE)) stop("Package 'nasapower' must be installed to fetch data.")
  cache_scope <- match.arg(cache_scope)
  cache_dir <- .cache_dir(cache_dir, cache_scope, pkg)
  .cache_init(cache_dir, cache_scope, pkg)
  idx <- .cache_read_index(cache_dir)

  calls <- data.table::as.data.table(calls_to_fetch)
  if (!all(c("rep_lat","rep_lon","start_utc","end_utc","site_elevation") %in% names(calls))) {
    stop("calls_to_fetch must include rep_lat, rep_lon, start_utc, end_utc, site_elevation")
  }

  fetched_list <- list()
  for (i in seq_len(nrow(calls))) {
    lat <- calls$rep_lat[i]
    lon <- calls$rep_lon[i]
    elev <- calls$site_elevation[i]
    s <- as.POSIXct(calls$start_utc[i], tz="UTC")
    e <- as.POSIXct(calls$end_utc[i], tz="UTC")

    date_start <- format(as.Date(s, tz="UTC"), "%Y-%m-%d")
    date_end   <- format(as.Date(e, tz="UTC"), "%Y-%m-%d")
    dates <- c(date_start, date_end)

    key <- .cache_key(
      time_api = time_api,
      params = params,
      rep_lat = lat,
      rep_lon = lon,
      start_utc = s,
      end_utc = e,
      settings = utils::modifyList(settings, list(site_elevation = elev))
    )

    if (verbose) message("POWER fetch [", i, "/", nrow(calls), "] key=", substr(key,1,8),
                         " lat=", lat, " lon=", lon, " elev=", elev,
                         " ", date_start, " .. ", date_end)

    wth <- nasapower::get_power(
      community = community,
      lonlat = c(lon, lat),
      site_elevation = elev,
      pars = params,
      dates = dates,
      temporal_api = time_api,
      time_standard = time_standard,
      ...
    )
    w_dt <- data.table::as.data.table(wth)
    w_norm <- .normalize_power_time(w_dt, time_api = time_api, tz = "UTC", dummy_hour = dummy_hour)
    w_norm[, `:=`(rep_lat = lat, rep_lon = lon, site_elevation = elev)]

    idx <- .cache_register(
      index = idx,
      cache_dir = cache_dir,
      key = key,
      time_api = time_api,
      params = params,
      rep_lat = lat,
      rep_lon = lon,
      site_elevation = elev,
      start_utc = min(w_norm$timestamp_utc, na.rm=TRUE),
      end_utc = max(w_norm$timestamp_utc, na.rm=TRUE),
      data = w_norm
    )

    fetched_list[[length(fetched_list)+1]] <- w_norm
  }

  list(
    fetched = if (length(fetched_list)) data.table::rbindlist(fetched_list, use.names=TRUE, fill=TRUE) else NULL,
    index = idx
  )
}
