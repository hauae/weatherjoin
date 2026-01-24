testthat::test_that("join_weather cluster_radius_m sanity checks", {
  x <- data.frame(
    lat = 51.5,
    lon = -0.1,
    event_time = as.POSIXct("2024-06-01 12:00:00", tz = "UTC"),
    site_elevation = "constant",
    elev_constant = 100
  )
  
  # minimal valid call (should not error, but we don't want real POWER calls)
  # so we mock provider/caching layer if your integration test already does that.
  # Here we only test argument validation, so we expect failure before any fetching.
  
  testthat::expect_error(
    weatherjoin::join_weather(
      x = x, params = "T2M", time = "event_time",
      spatial_mode = "cluster",
      cluster_radius_m = NULL,
      site_elevation = "constant",
      elev_constant = 100
    ),
    "cluster_radius_m"
  )
  
  testthat::expect_error(
    weatherjoin::join_weather(
      x = x, params = "T2M", time = "event_time",
      spatial_mode = "cluster",
      cluster_radius_m = -1,
      site_elevation = "constant",
      elev_constant = 100
    ),
    "positive"
  )
  
  testthat::expect_error(
    weatherjoin::join_weather(
      x = x, params = "T2M", time = "event_time",
      spatial_mode = "cluster",
      cluster_radius_m = c(10, 20),
      site_elevation = "constant",
      elev_constant = 100
    ),
    "single numeric"
  )
  
  # If spatial_mode is not cluster, cluster_radius_m should be ignored with a warning
  testthat::expect_warning(
    weatherjoin::join_weather(
      x = x, params = "T2M", time = "event_time",
      spatial_mode = "exact",
      cluster_radius_m = 123,
      site_elevation = "constant",
      elev_constant = 100
    ),
    "ignored"
  )
})
