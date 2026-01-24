testthat::test_that("site_elevation auto without elev_fun warns", {
  x <- data.frame(
    lat = 51.5,
    lon = -0.1,
    event_time = as.POSIXct("2024-06-01 12:00:00", tz = "UTC")
  )
  
  testthat::expect_warning(
    weatherjoin::join_weather(
      x = x,
      params = "T2M",
      time = "event_time",
      site_elevation = "auto",
      elev_fun = NULL,
      # avoid extra warnings:
      spatial_mode = "cluster",
      cluster_radius_m = 250,
      # make sure we don't fetch (force early failure)
      time_api = "hourly",
      # (optional) keep cache quiet in tests
      cache_scope = "project",
      cache_dir = tempfile("wj_cache_")
    ),
    "site_elevation='auto' selected",
    fixed = TRUE
  )
})
