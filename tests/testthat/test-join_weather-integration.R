testthat::test_that("join_weather integrates: plans, fetches (mocked), caches, joins", {
  testthat::skip_if_not_installed("nasapower")
  
  # Use a temp cache dir so tests don't touch user/project cache
  tmp <- withr::local_tempdir()
  withr::local_options(list(
    # Make behaviour deterministic and avoid huge splitting
    weatherjoin.split_penalty_hours = 72,
    weatherjoin.pad_hours = 0,
    weatherjoin.max_parts = 50,
    weatherjoin.dummy_hour = 12,
    # Force fetch path (even if cache exists in tmp dir)
    weatherjoin.cache_refresh = "always",
    weatherjoin.cache_match_mode = "cover",
    weatherjoin.cache_param_match = "superset",
    weatherjoin.cache_max_age_days = 30
  ))
  
  # Minimal event table (2 times around the same hour)
  x <- data.frame(
    lat = 51.5,
    lon = -0.1,
    event_time = as.POSIXct(c("2024-06-01 12:10:00", "2024-06-01 12:50:00"), tz = "UTC")
  )
  
  # --- Mock nasapower::get_power() ---
  # We return a small hourly grid covering the request window with deterministic values.
  mock_get_power <- function(community, lonlat, site_elevation, pars, dates, temporal_api, time_standard, ...) {
    # 'dates' in your code is typically c(start_date, end_date) as character "YYYY-MM-DD"
    # We'll build hourly data for the whole date span.
    start <- as.POSIXct(paste0(dates[1], " 00:00:00"), tz = "UTC")
    end   <- as.POSIXct(paste0(dates[2], " 23:00:00"), tz = "UTC")
    tt <- seq(start, end, by = "hour")
    
    out <- data.frame(
      YEAR = as.integer(format(tt, "%Y")),
      MO   = as.integer(format(tt, "%m")),
      DY   = as.integer(format(tt, "%d")),
      HR   = as.integer(format(tt, "%H"))
    )
    
    # Provide the requested params as numeric columns
    pars_up <- toupper(pars)
    if ("T2M" %in% pars_up) out$T2M <- rep(15.4, nrow(out))
    if ("PRECTOTCORR" %in% pars_up) out$PRECTOTCORR <- rep(0.2, nrow(out))
    
    # Provide the same attribute shape you parse elsewhere (optional but realistic)
    attr(out, "POWER.Parameters") <- paste(
      sprintf("%-15s mock parameter", pars_up),
      collapse = " ;\n "
    )
    
    out
  }
  
  # Mock within the nasapower namespace for the duration of the test
  testthat::local_mocked_bindings(
    get_power = mock_get_power,
    .package = "nasapower"
  )
  
  # Run the real join_weather() (it will call mocked nasapower::get_power())
  out <- weatherjoin::join_weather(
    x = x,
    params = c("T2M", "PRECTOTCORR"),
    time = "event_time",
    time_api = "hourly",
    lat_col = "lat",
    lon_col = "lon",
    tz = "UTC",
    roll = "nearest",
    roll_max_hours = 1,
    cache_scope = "project",
    cache_dir = tmp,
    community = "ag"
  )
  
  testthat::expect_true(all(c("T2M", "PRECTOTCORR") %in% names(out)))
  testthat::expect_true(all(!is.na(out$T2M)))
  testthat::expect_true(all(!is.na(out$PRECTOTCORR)))
  
  # Rolling join should still match (values are constant anyway)
  testthat::expect_equal(out$T2M, c(15.4, 15.4))
  testthat::expect_equal(out$PRECTOTCORR, c(0.2, 0.2))
})
