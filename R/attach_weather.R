#' Join weather back to events (supports rolling join for hourly)
#' @keywords internal
.attach_weather <- function(
  x,
  weather,
  params,
  tz = "UTC",
  roll = c("nearest","last","none"),
  roll_max_hours = NULL,
  coord_digits = 5
) {
  roll <- match.arg(roll)
  X <- data.table::as.data.table(x)
  W <- data.table::as.data.table(weather)

  req <- unique(toupper(trimws(params)))
  req <- req[nzchar(req)]
  keep_weather_cols <- intersect(req, names(W))
  if (!length(keep_weather_cols)) stop("None of requested params found in weather table.")

  X[, `:=`(
    rep_lat_r = round(as.numeric(rep_lat), coord_digits),
    rep_lon_r = round(as.numeric(rep_lon), coord_digits)
  )]
  W[, `:=`(
    rep_lat_r = round(as.numeric(rep_lat), coord_digits),
    rep_lon_r = round(as.numeric(rep_lon), coord_digits)
  )]
  attr(X$timestamp_utc, "tzone") <- tz
  attr(W$timestamp_utc, "tzone") <- tz

  Wj <- W[, c("rep_lat_r","rep_lon_r","timestamp_utc", keep_weather_cols), with=FALSE]
  data.table::setkey(Wj, rep_lat_r, rep_lon_r, timestamp_utc)
  Wj <- unique(Wj)

  data.table::setkey(X, rep_lat_r, rep_lon_r, timestamp_utc)

  if (roll == "none") {
    out <- Wj[X, on=.(rep_lat_r, rep_lon_r, timestamp_utc)]
  } else {
    roll_arg <- if (roll == "nearest") "nearest" else TRUE
    out <- Wj[X, on=.(rep_lat_r, rep_lon_r, timestamp_utc), roll = roll_arg]
  }

  if (!is.null(roll_max_hours) && roll != "none") {
    x_time <- out[["i.timestamp_utc"]]
    w_time <- out[["timestamp_utc"]]
    diff_hours <- abs(as.numeric(difftime(w_time, x_time, units="hours")))
    too_far <- diff_hours > roll_max_hours
    if (any(too_far, na.rm=TRUE)) out[too_far, (keep_weather_cols) := NA]
    out[, time_diff_hours := diff_hours]
  }

  drop_exact <- intersect(c("rep_lat_r","rep_lon_r"), names(out))
  if (length(drop_exact)) out[, (drop_exact) := NULL]
  i_cols <- grep("^i\\.", names(out), value=TRUE)
  i_cols <- setdiff(i_cols, "i.timestamp_utc")
  if (length(i_cols)) out[, (i_cols) := NULL]
  if ("i.timestamp_utc" %in% names(out) && !is.null(roll_max_hours) && roll != "none") out[, i.timestamp_utc := NULL]

  base_cols <- setdiff(names(out), keep_weather_cols)
  data.table::setcolorder(out, c(base_cols, keep_weather_cols))
  out[]
}

#' @keywords internal
.load_cached_segments <- function(chk_to_load) {
  if (nrow(chk_to_load) == 0L) return(NULL)
  paths <- chk_to_load$cache_path
  out <- list()
  for (p in paths) {
    if (is.na(p) || !file.exists(p)) next
    if (grepl("\\.fst$", p) && requireNamespace("fst", quietly=TRUE)) {
      out[[length(out)+1]] <- data.table::as.data.table(fst::read_fst(p))
    } else {
      out[[length(out)+1]] <- data.table::as.data.table(readRDS(p))
    }
  }
  if (!length(out)) return(NULL)
  data.table::rbindlist(out, use.names=TRUE, fill=TRUE)
}

#' @keywords internal
.materialize_and_attach <- function(
  x_mapped,
  chk,
  fetched,
  params,
  roll = c("nearest","last","none"),
  roll_max_hours = NULL
) {
  roll <- match.arg(roll)
  Wc <- .load_cached_segments(chk$to_load)
  Wf <- fetched
  W <- NULL
  if (!is.null(Wc) && !is.null(Wf)) W <- data.table::rbindlist(list(Wc, Wf), use.names=TRUE, fill=TRUE)
  if (!is.null(Wc) && is.null(Wf)) W <- Wc
  if (is.null(Wc) && !is.null(Wf)) W <- Wf
  if (is.null(W)) stop("No weather data available to attach (cache empty and nothing fetched).")

  need <- c("timestamp_utc","rep_lat","rep_lon")
  if (!all(need %in% names(x_mapped))) stop("x_mapped must contain: ", paste(need, collapse=", "))
  if (!all(need %in% names(W))) stop("weather must contain: ", paste(need, collapse=", "))

  .attach_weather(x = x_mapped, weather = W, params = params, roll = roll, roll_max_hours = roll_max_hours)
}
