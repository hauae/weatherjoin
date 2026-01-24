#' Single-column time input path
#' Validate and normalize a time column
#' @keywords internal
.validate_single_time <- function(raw, tz = "UTC", dummy_hour = 12L,
                                  time_api_resolved = c("daily","hourly"),
                                  time_col = "<time>", max_examples = 5L) {
  time_api_resolved <- match.arg(time_api_resolved)
  
  # Keep NA rows; only validate non-NA
  idx_non_na <- which(!is.na(raw))
  
  # Parse to POSIXct
  ts <- NULL
  
  if (inherits(raw, "POSIXt")) {
    ts <- as.POSIXct(raw, tz = tz)
    attr(ts, "tzone") <- tz
  } else if (inherits(raw, "Date")) {
    if (time_api_resolved == "hourly") {
      stop(
        "time_api='hourly' requires a timestamp with hour information, but `", time_col, "` is Date.\n",
        "Provide a POSIXct column or multi-column time including HR."
      )
    }
    ts <- as.POSIXct(raw, tz = tz) + as.integer(dummy_hour) * 3600
    attr(ts, "tzone") <- tz
  } else if (is.numeric(raw)) {
    # Heuristic: treat as YYYYMMDD if it looks like 8 digits in plausible date range
    # (Avoid interpreting epoch seconds accidentally.)
    x <- raw
    looks_yyyymmdd <- !is.na(x) & x >= 18000101 & x <= 22001231 & abs(x - round(x)) < 1e-6
    if (any(looks_yyyymmdd)) {
      d <- as.Date(as.character(x), format = "%Y%m%d")
      if (any(is.na(d) & looks_yyyymmdd)) {
        bad <- utils::head(which(is.na(d) & looks_yyyymmdd), max_examples)
        stop(
          "Could not parse some numeric YYYYMMDD values in `", time_col, "`. Examples:\n",
          paste0("  row ", bad, ": ", x[bad], collapse = "\n")
        )
      }
      if (time_api_resolved == "hourly") {
        stop(
          "time_api='hourly' requires hour information, but `", time_col, "` looks like YYYYMMDD.\n",
          "Provide a POSIXct column or multi-column time including HR."
        )
      }
      ts <- as.POSIXct(d, tz = tz) + as.integer(dummy_hour) * 3600
      attr(ts, "tzone") <- tz
    } else {
      stop(
        "Numeric `", time_col, "` is ambiguous (not recognized as YYYYMMDD).\n",
        "If this is an epoch timestamp, convert it to POSIXct before calling join_weather()."
      )
    }
  } else {
    # character/factor/etc.
    x <- raw
    if (is.factor(x)) x <- as.character(x)
    
    if (requireNamespace("anytime", quietly = TRUE)) {
      ts <- anytime::anytime(x, tz = tz)
      attr(ts, "tzone") <- tz
    } else {
      ts <- as.POSIXct(x, tz = tz)
    }
  }
  
  # Validate parsing success for non-NA inputs
  if (length(idx_non_na) > 0L) {
    bad <- idx_non_na[is.na(ts[idx_non_na])]
    if (length(bad)) {
      bad <- utils::head(bad, max_examples)
      stop(
        "Could not parse some values in `", time_col, "`. Examples:\n",
        paste0("  row ", bad, ": ", raw[bad], collapse = "\n"),
        "\nTip: install 'anytime' for more flexible parsing or pre-convert to POSIXct."
      )
    }
  }
  
  # Range sanity check (years)
  yr <- as.integer(format(ts, "%Y", tz = tz))
  bad_y <- which(!is.na(yr) & (yr < 1800 | yr > 2200))
  if (length(bad_y)) {
    bad_y <- utils::head(bad_y, max_examples)
    stop(
      "Some timestamps in `", time_col, "` are out of the supported range (~1800-2200). Examples:\n",
      paste0("  row ", bad_y, ": ", format(ts[bad_y], "%Y-%m-%d %H:%M:%S", tz = tz), collapse = "\n")
    )
  }
  
  ts
}

#' Multi-column time input path
#' Map time columns to roles
#' @param time_cols Character vector of column names supplied by the user via `time=`.
#' @param names_x Names of the input table.
#' @return A list with `mode` ("ymd" or "ydoy") and role names: year, month, day, hour (optional), doy (optional).
#' @keywords internal
.map_time_columns <- function(time_cols, names_x) {
  if (length(time_cols) < 2L) {
    stop("Internal error: .map_time_columns() expects multiple time columns.")
  }
  
  # Work only with what user passed
  if (!all(time_cols %in% names_x)) {
    miss <- setdiff(time_cols, names_x)
    stop("Some `time=` columns are missing from `x`: ", paste(miss, collapse = ", "))
  }
  
  # name aliases (case-insensitive)
  lc <- tolower(time_cols)
  pick1 <- function(candidates) {
    idx <- which(lc %in% candidates)
    if (length(idx) == 0L) return(NA_character_)
    if (length(idx) > 1L) {
      stop(
        "Ambiguous `time=` specification: multiple candidates for the same role: ",
        paste(time_cols[idx], collapse = ", "),
        ". Please pass only one column per role."
      )
    }
    time_cols[idx]
  }
  
  year  <- pick1(c("year","yyyy","yr","y"))
  month <- pick1(c("mo","mm","month","mon"))
  day   <- pick1(c("dy","dd","day"))
  hour  <- pick1(c("hr","hour"))
  doy   <- pick1(c("doy"))
  
  if (is.na(year)) {
    stop(
      "`time=` uses multiple columns but no year column was detected.\n",
      "Expected one of: YEAR, YYYY.\n",
      "You provided: ", paste(time_cols, collapse = ", ")
    )
  }
  
  # Two supported schemas:
  # 1) Y/M/D (+ optional HR)
  # 2) YEAR + DOY (+ optional HR)
  if (!is.na(doy)) {
    if (!is.na(month) || !is.na(day)) {
      stop(
        "`time=` mixes DOY with month/day columns.\n",
        "Use either {YEAR, DOY, (optional HR)} OR {YEAR, MO/MM, DY/DD, (optional HR)}.\n",
        "You provided: ", paste(time_cols, collapse = ", ")
      )
    }
    return(list(mode = "ydoy", year = year, doy = doy, hour = hour))
  }
  
  if (is.na(month) || is.na(day)) {
    stop(
      "`time=` uses multiple columns but month/day could not be detected.\n",
      "Expected month column: MO or MM; and day column: DY or DD.\n",
      "You provided: ", paste(time_cols, collapse = ", ")
    )
  }
  
  list(mode = "ymd", year = year, month = month, day = day, hour = hour)
}

#' Multi-column time input path
#' Validate time components and build Date safely
#' @param y,m,d Integer-ish vectors (for mode="ymd").
#' @param doy Integer-ish vector (for mode="ydoy").
#' @param h Optional integer-ish vector.
#' @param mode "ymd" or "ydoy"
#' @param time_api_resolved "hourly" or "daily" (for hourly requirement checks)
#' @param time_cols Character vector of user-specified columns for error context.
#' @param max_examples How many bad examples to show in error messages.
#' @return A list with `date` (Date) and `hour` (integer, possibly NA if missing and not allowed).
#' @keywords internal
.validate_time_components <- function(
    y,
    m = NULL,
    d = NULL,
    doy = NULL,
    h = NULL,
    mode = c("ymd","ydoy"),
    time_api_resolved = c("daily","hourly"),
    time_cols = character(),
    max_examples = 5L
) {
  mode <- match.arg(mode)
  time_api_resolved <- match.arg(time_api_resolved)
  
  # helper: integer-ish check
  intish <- function(x) is.numeric(x) && all(is.na(x) | abs(x - round(x)) < 1e-6)
  
  # Coerce numeric-like factors/characters to numeric where possible
  to_num <- function(x) {
    if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.numeric(x))
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) suppressWarnings(as.numeric(x)) else x
  }
  
  y <- to_num(y); m <- to_num(m); d <- to_num(d); doy <- to_num(doy); h <- to_num(h)
  
  # Basic type checks
  if (!intish(y)) {
    stop("Year column contains non-numeric or non-integer values. ",
         "Columns provided via `time=`: ", paste(time_cols, collapse = ", "))
  }
  
  # Range checks
  bad_year <- which(!is.na(y) & (y < 1800 | y > 2200))
  if (length(bad_year)) {
    ex <- utils::head(bad_year, max_examples)
    stop(
      "Year values out of range (expected ~1800-2200). Examples:\n",
      paste0("  row ", ex, ": ", y[ex], collapse = "\n")
    )
  }
  
  if (!is.null(h)) {
    if (!intish(h)) {
      stop("Hour column contains non-numeric or non-integer values. ",
           "Columns provided via `time=`: ", paste(time_cols, collapse = ", "))
    }
    bad_h <- which(!is.na(h) & (h < 0 | h > 23))
    if (length(bad_h)) {
      ex <- utils::head(bad_h, max_examples)
      stop(
        "Hour values out of range (expected 0-23). Examples:\n",
        paste0("  row ", ex, ": ", h[ex], collapse = "\n")
      )
    }
  }
  
  if (mode == "ymd") {
    if (!intish(m) || !intish(d)) {
      stop(
        "Month/day columns contain non-numeric or non-integer values.\n",
        "Columns provided via `time=`: ", paste(time_cols, collapse = ", ")
      )
    }
    bad_m <- which(!is.na(m) & (m < 1 | m > 12))
    if (length(bad_m)) {
      ex <- utils::head(bad_m, max_examples)
      stop(
        "Month values out of range (expected 1-12). Examples:\n",
        paste0("  row ", ex, ": ", m[ex], collapse = "\n")
      )
    }
    bad_d <- which(!is.na(d) & (d < 1 | d > 31))
    if (length(bad_d)) {
      ex <- utils::head(bad_d, max_examples)
      stop(
        "Day values out of range (expected 1-31). Examples:\n",
        paste0("  row ", ex, ": ", d[ex], collapse = "\n")
      )
    }
    
    # Build date; will be NA for invalid combinations (e.g., 2024-02-31)
    date <- as.Date(
      sprintf("%04d-%02d-%02d", round(y), round(m), round(d)),
      format = "%Y-%m-%d"
    )
    bad_date <- which(!is.na(y) & !is.na(m) & !is.na(d) & is.na(date))
    if (length(bad_date)) {
      ex <- utils::head(bad_date, max_examples)
      stop(
        "Invalid calendar dates detected when combining YEAR/MONTH/DAY. Examples:\n",
        paste0("  row ", ex, ": ", round(y[ex]), "-", round(m[ex]), "-", round(d[ex]), collapse = "\n"),
        "\nTip: check whether month/day columns are swapped or contain invalid values."
      )
    }
  } else {
    # ydoy mode
    if (!intish(doy)) {
      stop("DOY column contains non-numeric or non-integer values. ",
           "Columns provided via `time=`: ", paste(time_cols, collapse = ", "))
    }
    bad_doy <- which(!is.na(doy) & (doy < 1 | doy > 366))
    if (length(bad_doy)) {
      ex <- utils::head(bad_doy, max_examples)
      stop(
        "DOY values out of range (expected 1-366). Examples:\n",
        paste0("  row ", ex, ": ", doy[ex], collapse = "\n")
      )
    }
    
    # Build date from year + doy
    date0 <- as.Date(sprintf("%04d-01-01", round(y)))
    date <- date0 + (round(doy) - 1L)
    
    # Validate: year must match (catches DOY=366 on non-leap years producing next year)
    bad_year_roll <- which(!is.na(y) & !is.na(doy) & as.integer(format(date, "%Y")) != round(y))
    if (length(bad_year_roll)) {
      ex <- utils::head(bad_year_roll, max_examples)
      stop(
        "DOY/year combination rolls into a different year (likely DOY=366 for a non-leap year). Examples:\n",
        paste0("  row ", ex, ": YEAR=", round(y[ex]), ", DOY=", round(doy[ex]), collapse = "\n")
      )
    }
  }
  
  # Hour requirement logic
  if (time_api_resolved == "hourly" && is.null(h)) {
    stop(
      "time_api='hourly' requires an hour column (e.g. HR) or a full timestamp column.\n",
      "You provided time columns: ", paste(time_cols, collapse = ", ")
    )
  }
  
  list(
    date = date,
    hour = if (is.null(h)) NA_integer_ else as.integer(round(h))
  )
}

#' Build standard time keys used by weatherjoin
#'
#' @param DT data.table with input data.
#' @param time User `time=` specification (single column or multiple columns).
#' @param tz Timezone used for parsing/constructing timestamps (default UTC).
#' @param time_api_resolved "daily" or "hourly" (already resolved from user setting/guess).
#' @return DT with `timestamp_utc` (POSIXct) and `t_utc` (numeric seconds) columns added.
#' @keywords internal
.build_time <- function(DT, time, tz = "UTC", time_api_resolved = c("daily","hourly")) {
  time_api_resolved <- match.arg(time_api_resolved)
  dummy_hour <- .wj_opt("dummy_hour", 12)
  data.table::setDT(DT)
  
  if (length(time) < 1L) stop("`time` must be a column name or a vector of column names.")
  # Always keep NA rows; they will receive NA weather later
  if (length(time) == 1L) {
    time_col <- time[[1L]]
    if (!(time_col %in% names(DT))) stop("`time` column not found in `x`: ", time_col)
    
    DT[, timestamp_utc := .validate_single_time(
      raw = DT[[time_col]],
      tz = tz,
      dummy_hour = dummy_hour,
      time_api_resolved = time_api_resolved,
      time_col = time_col
    )]
  } else {
    # Multi-column time
    spec <- .map_time_columns(time_cols = time, names_x = names(DT))
    
    if (spec$mode == "ymd") {
      
      v <- .validate_time_components(
        y = DT[[spec$year]],
        m = DT[[spec$month]],
        d = DT[[spec$day]],
        h = if (!is.na(spec$hour)) DT[[spec$hour]] else NULL,
        mode = "ymd",
        time_api_resolved = time_api_resolved,
        time_cols = time
      )
      hour <- v$hour
      hour[is.na(hour)] <- as.integer(dummy_hour)
      
      DT[, timestamp_utc := as.POSIXct(v$date, tz = tz) + as.integer(hour) * 3600]

    } else {
      
      v <- .validate_time_components(
        y = DT[[spec$year]],
        doy = DT[[spec$doy]],
        h = if (!is.na(spec$hour)) DT[[spec$hour]] else NULL,
        mode = "ydoy",
        time_api_resolved = time_api_resolved,
        time_cols = time
      )
      hour <- v$hour
      if (all(is.na(hour))) hour <- as.integer(dummy_hour)
      
      DT[, timestamp_utc := as.POSIXct(v$date, tz = tz) + as.integer(hour) * 3600]
    }
  }
  
  # Standardise internal time to UTC representation
  DT[, timestamp_utc := as.POSIXct(format(timestamp_utc, tz = "UTC", usetz = TRUE), tz = "UTC")]
  attr(DT$timestamp_utc, "tzone") <- "UTC"
  
  # Always provide numeric join key
  DT[, t_utc := as.numeric(timestamp_utc)]
  DT[]
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
