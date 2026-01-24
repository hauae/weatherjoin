#' Spatial planning: map points to representative locations
#' @keywords internal
.spatial_plan <- function(
    x,
    spatial_mode = c("cluster", "exact", "by_group"),
    lat_col = "lat",
    lon_col = "lon",
    group_col = NULL,
    rep_method = c("median", "centroid"),
    cluster_radius_m = 250,
    keep_diag = TRUE,
    check_range = TRUE,
    coord_digits = 5L
) {
  DT <- data.table::copy(data.table::as.data.table(x))
  spatial_mode <- match.arg(spatial_mode)
  rep_method <- match.arg(rep_method)
  
  if (!all(c(lat_col, lon_col) %in% names(DT))) stop("x must contain lat_col and lon_col.")
  
  # --- Coerce to numeric working columns ---
  lat_raw <- DT[[lat_col]]
  lon_raw <- DT[[lon_col]]
  
  DT[, `:=`(
    .lat = suppressWarnings(as.numeric(get(lat_col))),
    .lon = suppressWarnings(as.numeric(get(lon_col)))
  )]
  
  # --- Type sanity: did coercion create new NAs from non-missing input? ---
  bad_lat <- !is.na(lat_raw) & is.na(DT$.lat)
  bad_lon <- !is.na(lon_raw) & is.na(DT$.lon)
  
  if (any(bad_lat) || any(bad_lon)) {
    ex_lat <- unique(lat_raw[bad_lat])
    ex_lon <- unique(lon_raw[bad_lon])
    ex_lat <- ex_lat[seq_len(min(length(ex_lat), 3L))]
    ex_lon <- ex_lon[seq_len(min(length(ex_lon), 3L))]
    
    msg <- c(
      "Latitude/longitude columns must be numeric (or numeric-like).",
      sprintf("Failed to convert some values to numeric in lat_col='%s' and/or lon_col='%s'.", lat_col, lon_col)
    )
    if (any(bad_lat)) msg <- c(msg, paste0("Examples in ", lat_col, ": ", paste(ex_lat, collapse = ", ")))
    if (any(bad_lon)) msg <- c(msg, paste0("Examples in ", lon_col, ": ", paste(ex_lon, collapse = ", ")))
    msg <- c(msg, "Tip: ensure coordinates are decimal degrees, not text labels, and that commas/degree symbols are removed.")
    stop(paste(msg, collapse = "\n"))
  }
  
  # --- Range sentry ---
  if (isTRUE(check_range)) {
    ok <- !(is.na(DT$.lat) | is.na(DT$.lon))
    bad_lat_rng <- ok & (DT$.lat < -90 | DT$.lat > 90)
    bad_lon_rng <- ok & (DT$.lon < -180 | DT$.lon > 180)
    
    if (any(bad_lat_rng) || any(bad_lon_rng)) {
      ex_lat <- DT$.lat[which(bad_lat_rng)[1]]
      ex_lon <- DT$.lon[which(bad_lon_rng)[1]]
      
      msg <- c(
        "Invalid latitude/longitude ranges detected.",
        "Expected: lat in [-90, 90], lon in [-180, 180].",
        if (any(bad_lat_rng)) paste0("Example invalid lat: ", format(ex_lat, digits = 16)) else NULL,
        if (any(bad_lon_rng)) paste0("Example invalid lon: ", format(ex_lon, digits = 16)) else NULL,
        "Tip: check that lat/lon were not swapped and are in decimal degrees."
      )
      stop(paste(msg[!vapply(msg, is.null, logical(1))], collapse = "\n"))
    }
  }
  
  # --- Optional precision control (default 5 dp) ---
  if (!is.null(coord_digits)) {
    coord_digits <- as.integer(coord_digits)
    if (is.na(coord_digits) || coord_digits < 0L) stop("coord_digits must be a non-negative integer or NULL.")
    DT[, `:=`(.lat = round(.lat, coord_digits),
              .lon = round(.lon, coord_digits))]
  }
  
  # helper: round representative coordinates consistently (cache identity)
  .round_reps <- function(reps_dt) {
    if (!is.null(coord_digits)) {
      reps_dt[, `:=`(
        rep_lat = round(as.numeric(rep_lat), coord_digits),
        rep_lon = round(as.numeric(rep_lon), coord_digits)
      )]
    }
    reps_dt
  }
  
  hav_m <- function(lat1, lon1, lat2, lon2) {
    r <- 6371000
    to_rad <- pi/180
    lat1 <- lat1 * to_rad; lon1 <- lon1 * to_rad
    lat2 <- lat2 * to_rad; lon2 <- lon2 * to_rad
    dlat <- lat2 - lat1; dlon <- lon2 - lon1
    a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
    2 * r * asin(pmin(1, sqrt(a)))
  }
  
  if (spatial_mode == "exact") {
    reps <- unique(DT[, .(rep_lat = .lat, rep_lon = .lon)])
    reps <- .round_reps(reps)
    reps[, loc_id := .I]
    
    # Update-join: keep DT, add loc_id/rep_lat/rep_lon
    DT[reps, `:=`(
      loc_id = i.loc_id,
      rep_lat = i.rep_lat,
      rep_lon = i.rep_lon
    ), on = .(.lat = rep_lat, .lon = rep_lon)]
  }
  
  if (spatial_mode == "by_group") {
    if (is.null(group_col) || !(group_col %in% names(DT))) {
      stop("group_col must be provided for spatial_mode='by_group'.")
    }
    
    if (rep_method == "centroid") {
      reps <- DT[, .(rep_lat = mean(.lat, na.rm = TRUE),
                     rep_lon = mean(.lon, na.rm = TRUE)),
                 by = group_col]
    } else {
      reps <- DT[, .(rep_lat = stats::median(.lat, na.rm = TRUE),
                     rep_lon = stats::median(.lon, na.rm = TRUE)),
                 by = group_col]
    }
    
    data.table::setnames(reps, group_col, "rep_group")
    reps <- .round_reps(reps)
    reps[, loc_id := .I]
    
    DT[, rep_group := get(group_col)]
    
    DT[reps, `:=`(
      loc_id = i.loc_id,
      rep_lat = i.rep_lat,
      rep_lon = i.rep_lon
    ), on = "rep_group"]
  }
  
  if (spatial_mode == "cluster") {
    locs <- unique(DT[, .(lat = .lat, lon = .lon)])
    n <- nrow(locs)
    
    if (n == 1) {
      locs[, cluster_id := 1L]
    } else {
      D <- matrix(0, n, n)
      for (i in 1:(n - 1)) {
        d <- hav_m(locs$lat[i], locs$lon[i], locs$lat[(i + 1):n], locs$lon[(i + 1):n])
        D[i, (i + 1):n] <- d
        D[(i + 1):n, i] <- d
      }
      hc <- stats::hclust(stats::as.dist(D), method = "single")
      locs[, cluster_id := stats::cutree(hc, h = cluster_radius_m)]
    }
    
    if (rep_method == "centroid") {
      reps <- locs[, .(rep_lat = mean(lat), rep_lon = mean(lon)), by = cluster_id]
    } else {
      reps <- locs[, .(rep_lat = stats::median(lat), rep_lon = stats::median(lon)), by = cluster_id]
    }
    reps <- .round_reps(reps)
    reps[, loc_id := .I]
    
    # Update-join 1: add cluster_id to DT (keep .lat/.lon intact)
    DT[locs, cluster_id := i.cluster_id, on = .(.lat = lat, .lon = lon)]
    
    # Update-join 2: add loc_id/rep_lat/rep_lon from cluster reps
    DT[reps, `:=`(
      loc_id = i.loc_id,
      rep_lat = i.rep_lat,
      rep_lon = i.rep_lon
    ), on = "cluster_id"]
  }
  
  if (keep_diag) {
    DT[, dist_to_rep_m := hav_m(.lat, .lon, rep_lat, rep_lon)]
  }
  
  DT[, c(".lat", ".lon") := NULL]
  
  # Prefer returning the reps table we actually constructed (source of truth)
  if (spatial_mode == "by_group") {
    reps_out <- unique(reps[, .(loc_id, rep_group, rep_lat, rep_lon)])
  } else {
    reps_out <- unique(reps[, .(loc_id, rep_lat, rep_lon)])
  }
  
  list(reps = reps_out, mapped = DT)
}
