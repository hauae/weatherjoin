#' Normalize POWER output time columns to timestamp_utc (UTC)
#' @keywords internal
.normalize_power_time <- function(w, time_api = c("hourly","daily"), tz = "UTC", dummy_hour = 12L) {
  time_api <- match.arg(time_api)
  DT <- data.table::as.data.table(w)

  if (time_api == "hourly") {
    need <- c("YEAR","MO","DY","HR")
    if (!all(need %in% names(DT))) stop("Hourly POWER data must contain: ", paste(need, collapse=", "))
    iso <- sprintf("%04d-%02d-%02d %02d:00:00", as.integer(DT$YEAR), as.integer(DT$MO), as.integer(DT$DY), as.integer(DT$HR))
    DT[, `:=`(
      date = as.Date(sprintf("%04d-%02d-%02d", YEAR, MO, DY)),
      hour = as.integer(HR),
      timestamp_utc = as.POSIXct(iso, tz = tz)
    )]
  } else {
    if ("YYYYMMDD" %in% names(DT)) {
      d <- as.Date(as.character(DT$YYYYMMDD), format = "%Y%m%d")
    } else if (all(c("YEAR","MM","DD") %in% names(DT))) {
      d <- as.Date(sprintf("%04d-%02d-%02d", as.integer(DT$YEAR), as.integer(DT$MM), as.integer(DT$DD)))
    } else {
      stop("Daily POWER data must contain YYYYMMDD or YEAR/MM/DD.")
    }
    DT[, date := d]
    DT[, hour := as.integer(dummy_hour)]
    DT[, timestamp_utc := as.POSIXct(date, tz = tz) + hour*3600]
  }

  drop <- intersect(c("YEAR","MO","DY","HR","MM","DD","DOY","YYYYMMDD"), names(DT))
  if (length(drop)) DT[, (drop) := NULL]
  DT[]
}
