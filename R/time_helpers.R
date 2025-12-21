#' Build timestamp_raw from a single time column or multiple time-part columns
#' @keywords internal
.build_time <- function(x, time, tz = "UTC") {
  DT <- data.table::as.data.table(x)
  if (is.null(time) || length(time) < 1) stop("Argument 'time' must be provided.")
  if (!all(time %in% names(DT))) stop("Missing time column(s): ", paste(setdiff(time, names(DT)), collapse = ", "))

  parse_any <- function(v) {
    if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) {
      out <- as.POSIXct(v, tz = tz); attr(out, "tzone") <- tz; return(out)
    }
    if (inherits(v, "Date")) {
      out <- as.POSIXct(v, tz = tz); attr(out, "tzone") <- tz; return(out)
    }
    if (is.numeric(v)) {
      vv <- as.character(v)
      out <- as.POSIXct(vv, format = "%Y%m%d", tz = tz)
      if (all(is.na(out)) && any(!is.na(v)) && stats::median(v, na.rm = TRUE) > 1e9) {
        out <- as.POSIXct(v, origin = "1970-01-01", tz = tz)
      }
      attr(out, "tzone") <- tz; return(out)
    }
    vch <- as.character(v)
    if (requireNamespace("anytime", quietly = TRUE)) {
      out <- anytime::anytime(vch, tz = tz); attr(out, "tzone") <- tz; return(out)
    }
    out <- suppressWarnings(as.POSIXct(vch, tz = tz)); attr(out, "tzone") <- tz; out
  }

  if (length(time) == 1L) {
    DT[, timestamp_raw := parse_any(DT[[time]])]
  } else {
    nms <- toupper(time)
    get_ci <- function(name) {
      i <- match(name, nms)
      if (is.na(i)) return(NULL)
      DT[[time[i]]]
    }
    YEAR <- get_ci("YEAR"); 
    if (is.null(YEAR)) YEAR <- get_ci("YYYY"); 
    if (is.null(YEAR)) YEAR <- get_ci("Y")
    MO <- get_ci("MO"); 
    if (is.null(MO)) MO <- get_ci("MM"); 
    if (is.null(MO)) MO <- get_ci("MONTH")
    DY <- get_ci("DY"); 
    if (is.null(DY)) DY <- get_ci("DD"); 
    if (is.null(DY)) DY <- get_ci("DAY")
    HR <- get_ci("HR"); 
    if (is.null(HR)) HR <- get_ci("HOUR")
    MIN <- get_ci("MIN"); 
    if (is.null(MIN)) MIN <- get_ci("MN"); 
    if (is.null(MIN)) MIN <- get_ci("MINUTE")
    SEC <- get_ci("SEC"); 
    if (is.null(SEC)) SEC <- get_ci("SECOND")
    DOY <- get_ci("DOY")
    YYYYMMDD <- get_ci("YYYYMMDD")

    if (!is.null(YYYYMMDD)) {
      DT[, timestamp_raw := as.POSIXct(as.character(YYYYMMDD), format = "%Y%m%d", tz = tz)]
    } else if (!is.null(YEAR) && !is.null(MO) && !is.null(DY)) {
      y <- as.integer(YEAR); m <- as.integer(MO); d <- as.integer(DY)
      hh <- if (!is.null(HR)) as.integer(HR) else 0L
      mi <- if (!is.null(MIN)) as.integer(MIN) else 0L
      ss <- if (!is.null(SEC)) as.integer(SEC) else 0L
      iso <- sprintf("%04d-%02d-%02d %02d:%02d:%02d", y, m, d, hh, mi, ss)
      DT[, timestamp_raw := as.POSIXct(iso, tz = tz)]
    } else if (!is.null(YEAR) && !is.null(DOY)) {
      y <- as.integer(YEAR); j <- as.integer(DOY)
      base_date <- as.Date(sprintf("%04d-01-01", y)) + (j - 1L)
      hh <- if (!is.null(HR)) as.integer(HR) else 0L
      mi <- if (!is.null(MIN)) as.integer(MIN) else 0L
      ss <- if (!is.null(SEC)) as.integer(SEC) else 0L
      DT[, timestamp_raw := as.POSIXct(base_date, tz = tz) + hh*3600 + mi*60 + ss]
    } else {
      stop("Don't know how to build time from columns: ", paste(time, collapse=", "))
    }
  }

  ts <- DT[["timestamp_raw"]]
  ts_ok <- ts[!is.na(ts)]
  if (length(ts_ok) == 0L) {
    input_res <- "daily"
  } else {
    lt <- as.POSIXlt(ts_ok, tz = tz)
    has_tod <- any(lt$hour != 0L | lt$min != 0L | lt$sec != 0L)
    input_res <- if (has_tod) "hourly" else "daily"
  }
  list(dt = DT, input_res = input_res)
}

#' Resolve time_api based on user choice and input resolution
#' @keywords internal
.resolve_time_api <- function(dt, time_api = c("guess","hourly","daily"), input_res = c("hourly","daily"),
                              tz = "UTC", dummy_hour = 12L) {
  DT <- data.table::as.data.table(dt)
  time_api <- match.arg(time_api)
  input_res <- match.arg(input_res)

  ts_raw <- DT[["timestamp_raw"]]
  if (!inherits(ts_raw, "POSIXct")) ts_raw <- as.POSIXct(ts_raw, tz = tz)
  attr(ts_raw, "tzone") <- tz
  DT[, timestamp_raw := ts_raw]

  time_api_resolved <- if (time_api == "guess") input_res else time_api

  if (time_api_resolved == "hourly" && input_res == "daily") {
    stop("time_api='hourly' requested, but input time has no time-of-day information. Provide datetime/HR or set time_api='daily'.")
  }

  if (time_api_resolved == "daily") {
    d <- as.Date(DT$timestamp_raw, tz = tz)
    DT[, date := d]
    DT[, hour := as.integer(dummy_hour)]
    DT[, timestamp_utc := as.POSIXct(date, tz = tz) + hour * 3600]
  } else {
    DT[, timestamp_utc := timestamp_raw]
    DT[, date := as.Date(timestamp_utc, tz = tz)]
    DT[, hour := as.integer(format(timestamp_utc, "%H", tz = tz))]
  }

  list(dt = DT, time_api_resolved = time_api_resolved, input_res = input_res)
}
