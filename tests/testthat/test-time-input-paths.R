testthat::test_that(".map_time_columns maps expected roles", {
  DT <- make_time_fixture()
  
  spec1 <- weatherjoin:::.map_time_columns(c("YEAR","MO","DY","HR"), names(DT))
  testthat::expect_equal(spec1$mode, "ymd")
  testthat::expect_equal(spec1$year, "YEAR")
  testthat::expect_equal(spec1$month, "MO")
  testthat::expect_equal(spec1$day, "DY")
  testthat::expect_equal(spec1$hour, "HR")
  
  spec2 <- weatherjoin:::.map_time_columns(c("YYYY","MM","DD"), names(DT))
  testthat::expect_equal(spec2$mode, "ymd")
  testthat::expect_true(is.na(spec2$hour))
  
  spec3 <- weatherjoin:::.map_time_columns(c("YEAR","DOY"), names(DT))
  testthat::expect_equal(spec3$mode, "ydoy")
  testthat::expect_equal(spec3$year, "YEAR")
  testthat::expect_equal(spec3$doy, "DOY")
  
  testthat::expect_error(
    weatherjoin:::.map_time_columns(c("YEAR","DOY","MO"), names(DT)),
    "mixes DOY"
  )
})

testthat::test_that(".validate_time_components catches invalid ranges", {
  DT <- make_time_fixture()
  
  # Invalid hour
  DT2 <- data.table::copy(DT)
  DT2[1, HR := 24]
  testthat::expect_error(
    weatherjoin:::.validate_time_components(
      y = DT2$YEAR, m = DT2$MO, d = DT2$DY, h = DT2$HR,
      mode = "ymd", time_api_resolved = "hourly",
      time_cols = c("YEAR","MO","DY","HR")
    ),
    "Hour values out of range"
  )
  
  # Invalid month
  DT3 <- data.table::copy(DT)
  DT3[1, MO := 13]
  testthat::expect_error(
    weatherjoin:::.validate_time_components(
      y = DT3$YEAR, m = DT3$MO, d = DT3$DY, h = DT3$HR,
      mode = "ymd", time_api_resolved = "hourly",
      time_cols = c("YEAR","MO","DY","HR")
    ),
    "Month values out of range"
  )
  
  # Invalid calendar date (e.g., Feb 31) should raise our domain error (not charToDate)
  testthat::expect_error(
    weatherjoin:::.validate_time_components(
      y = c(2024, 2024),
      m = c(2, 2),
      d = c(31, 1),
      h = c(12, 12),
      mode = "ymd",
      time_api_resolved = "hourly",
      time_cols = c("YEAR","MO","DY","HR")
    ),
    "Invalid calendar dates"
  )
})

testthat::test_that(".validate_time_components never leaks charToDate errors", {
  # Regression guard: Feb 31 must never trigger base::charToDate() errors
  testthat::expect_error(
    weatherjoin:::.validate_time_components(
      y = c(2024),
      m = c(2),
      d = c(31),
      h = c(12),
      mode = "ymd",
      time_api_resolved = "hourly",
      time_cols = c("YEAR","MO","DY","HR")
    ),
    "Invalid calendar dates"
  )
})

testthat::test_that(".validate_single_time enforces hourly requirements", {
  DT <- make_time_fixture()
  
  # Date + hourly should error
  testthat::expect_error(
    weatherjoin:::.validate_single_time(
      raw = DT$event_date,
      tz = "UTC",
      time_api_resolved = "hourly",
      time_col = "event_date"
    ),
    "requires.*hour"
  )
  
  # Numeric YYYYMMDD + hourly should error
  testthat::expect_error(
    weatherjoin:::.validate_single_time(
      raw = DT$event_yyyymmdd,
      tz = "UTC",
      time_api_resolved = "hourly",
      time_col = "event_yyyymmdd"
    ),
    "requires.*hour"
  )
  
  # POSIXct + hourly should work
  ts <- weatherjoin:::.validate_single_time(
    raw = DT$event_time_posix,
    tz = "UTC",
    time_api_resolved = "hourly",
    time_col = "event_time_posix"
  )
  testthat::expect_s3_class(ts, "POSIXct")
})

testthat::test_that(".build_time single-column works and produces timestamp_utc + t_utc", {
  DT <- make_time_fixture()
  
  out <- weatherjoin:::.build_time(
    DT = data.table::copy(DT),
    time = "event_time_posix",
    tz = "UTC",
    time_api_resolved = "hourly"
  )
  
  testthat::expect_true(all(c("timestamp_utc","t_utc") %in% names(out)))
  testthat::expect_s3_class(out$timestamp_utc, "POSIXct")
  testthat::expect_true(is.numeric(out$t_utc))
  
  # Date + daily should work and set dummy hour from option
  withr::local_options(list(weatherjoin.dummy_hour = 12L))
  
  out2 <- weatherjoin:::.build_time(
    DT = data.table::copy(DT),
    time = "event_date",
    tz = "UTC",
    time_api_resolved = "daily"
  )
  hh <- as.integer(format(out2$timestamp_utc, "%H"))
  testthat::expect_true(all(hh == 12L | is.na(hh)))
})

testthat::test_that(".build_time multi-column YEAR/MO/DY/HR works; missing HR + hourly errors", {
  DT <- make_time_fixture()
  
  out <- weatherjoin:::.build_time(
    DT = data.table::copy(DT),
    time = c("YEAR","MO","DY","HR"),
    tz = "UTC",
    time_api_resolved = "hourly"
  )
  
  testthat::expect_s3_class(out$timestamp_utc, "POSIXct")
  testthat::expect_true(any(!is.na(out$timestamp_utc)))
  
  # Missing hour but time_api_resolved=hourly should error
  testthat::expect_error(
    weatherjoin:::.build_time(
      DT = data.table::copy(DT),
      time = c("YYYY","MM","DD"),
      tz = "UTC",
      time_api_resolved = "hourly"
    ),
    "requires.*hour"
  )
  
  # Same columns with daily should work; dummy hour from option
  withr::local_options(list(weatherjoin.dummy_hour = 12L))
  
  out2 <- weatherjoin:::.build_time(
    DT = data.table::copy(DT),
    time = c("YYYY","MM","DD"),
    tz = "UTC",
    time_api_resolved = "daily"
  )
  hh2 <- as.integer(format(out2$timestamp_utc, "%H"))
  testthat::expect_true(all(hh2 == 12L | is.na(hh2)))
})

testthat::test_that(".build_time supports YEAR+DOY schema (daily)", {
  DT <- make_time_fixture()
  
  withr::local_options(list(weatherjoin.dummy_hour = 12L))
  
  out <- weatherjoin:::.build_time(
    DT = data.table::copy(DT),
    time = c("YEAR","DOY"),
    tz = "UTC",
    time_api_resolved = "daily"
  )
  hh <- as.integer(format(out$timestamp_utc, "%H"))
  testthat::expect_true(all(hh == 12L | is.na(hh)))
  
  # DOY on non-leap year should error if 366
  testthat::expect_error(
    weatherjoin:::.validate_time_components(
      y = c(2023, 2023),
      doy = c(366, 365),
      h = c(12, 12),
      mode = "ydoy",
      time_api_resolved = "daily",
      time_cols = c("YEAR","DOY")
    ),
    "non-leap"
  )
})

testthat::test_that("weatherjoin.dummy_hour option controls daily timestamp hour", {
  DT <- make_time_fixture()
  
  # Set a non-default dummy hour
  withr::local_options(list(weatherjoin.dummy_hour = 7L))
  
  out <- weatherjoin:::.build_time(
    DT = data.table::copy(DT),
    time = "event_date",
    tz = "UTC",
    time_api_resolved = "daily"
  )
  
  hh <- as.integer(format(out$timestamp_utc, "%H"))
  
  # All non-NA timestamps should reflect the option value
  testthat::expect_true(all(hh == 7L | is.na(hh)))
})
