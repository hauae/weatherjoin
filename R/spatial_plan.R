#' Spatial planning: map points to representative locations
#' @keywords internal
.spatial_plan <- function(
  x,
  spatial_mode = c("exact", "by_group", "cluster"),
  lat_col = "lat",
  lon_col = "lon",
  group_col = NULL,
  rep_method = c("median", "centroid"),
  cluster_radius_m = NULL,
  keep_diag = TRUE
) {
  DT <- data.table::as.data.table(x)
  spatial_mode <- match.arg(spatial_mode)
  rep_method <- match.arg(rep_method)

  if (!all(c(lat_col, lon_col) %in% names(DT))) stop("x must contain lat_col and lon_col.")
  DT[, `:=`(.lat = as.numeric(get(lat_col)), .lon = as.numeric(get(lon_col)))]

  hav_m <- function(lat1, lon1, lat2, lon2) {
    r <- 6371000
    to_rad <- pi/180
    lat1 <- lat1*to_rad; lon1 <- lon1*to_rad
    lat2 <- lat2*to_rad; lon2 <- lon2*to_rad
    dlat <- lat2-lat1; dlon <- lon2-lon1
    a <- sin(dlat/2)^2 + cos(lat1)*cos(lat2)*sin(dlon/2)^2
    2*r*asin(pmin(1, sqrt(a)))
  }

  if (spatial_mode == "exact") {
    DT[, `:=`(rep_lat = .lat, rep_lon = .lon)]
    reps <- unique(DT[, .(rep_lat, rep_lon)])
    reps[, loc_id := .I]
    DT <- reps[DT, on=.(rep_lat, rep_lon)]
  }

  if (spatial_mode == "by_group") {
    if (is.null(group_col) || !(group_col %in% names(DT))) stop("group_col must be provided for spatial_mode='by_group'.")
    if (rep_method == "centroid") {
      reps <- DT[, .(rep_lat = mean(.lat, na.rm=TRUE), rep_lon = mean(.lon, na.rm=TRUE)), by = group_col]
    } else {
      reps <- DT[, .(rep_lat = stats::median(.lat, na.rm=TRUE), rep_lon = stats::median(.lon, na.rm=TRUE)), by = group_col]
    }
    data.table::setnames(reps, group_col, "rep_group")
    reps[, loc_id := .I]
    DT[, rep_group := get(group_col)]
    DT <- reps[DT, on="rep_group"]
  }

  if (spatial_mode == "cluster") {
    if (is.null(cluster_radius_m) || !is.numeric(cluster_radius_m) || cluster_radius_m <= 0) {
      stop("cluster_radius_m must be positive for spatial_mode='cluster'.")
    }
    locs <- unique(DT[, .(lat=.lat, lon=.lon)])
    n <- nrow(locs)
    if (n == 1) {
      locs[, cluster_id := 1L]
    } else {
      D <- matrix(0, n, n)
      for (i in 1:(n-1)) {
        d <- hav_m(locs$lat[i], locs$lon[i], locs$lat[(i+1):n], locs$lon[(i+1):n])
        D[i,(i+1):n] <- d
        D[(i+1):n,i] <- d
      }
      hc <- stats::hclust(stats::as.dist(D), method="single")
      locs[, cluster_id := stats::cutree(hc, h = cluster_radius_m)]
    }
    if (rep_method == "centroid") {
      reps <- locs[, .(rep_lat = mean(lat), rep_lon = mean(lon)), by=cluster_id]
    } else {
      reps <- locs[, .(rep_lat = stats::median(lat), rep_lon = stats::median(lon)), by=cluster_id]
    }
    reps[, loc_id := .I]
    DT <- locs[DT, on=.(lat=.lat, lon=.lon)]
    DT <- reps[DT, on="cluster_id"]
  }

  if (keep_diag) DT[, dist_to_rep_m := hav_m(.lat, .lon, rep_lat, rep_lon)]
  DT[, c(".lat",".lon") := NULL]

  reps_out <- unique(DT[, .(loc_id, rep_lat, rep_lon)])
  if ("rep_group" %in% names(DT)) reps_out <- unique(DT[, .(loc_id, rep_group, rep_lat, rep_lon)])

  list(reps = reps_out, mapped = DT)
}
