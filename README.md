# weatherjoin

Attach NASA POWER weather variables to an event table (e.g., field observations) with:
- Efficient spatio-temporal call planning
- Local caching (re-uses downloads)
- Exact or rolling joins for hourly data

## Install (development)

```r
# install.packages("remotes")
remotes::install_local("path/to/weatherjoin")
```

## Quick start

```r
library(data.table)
library(weatherjoin)

X <- data.table(
  event_time = as.POSIXct(c("2024-04-07 12:30:00","2024-04-07 13:30:00"), tz="UTC"),
  LAT = c(51.5, 51.5),
  LON = c(-0.1, -0.1),
  site = "A"
)

out <- join_weather(
  X,
  params = c("T2M","PRECTOTCORR"),
  time = "event_time",
  lat_col = "LAT",
  lon_col = "LON",
  spatial_mode = "by_group",
  group_col = "site",
  time_api = "hourly",
  roll = "nearest",
  roll_max_hours = 1,
  cache_scope = "project"
)
```

## Cache helpers

```r
wj_cache_list(cache_scope="project")
wj_cache_clear(cache_scope="project", dry_run=TRUE)
```

## Notes

This package is not affiliated with or endorsed by NASA.
NASA POWER data are accessed via the `nasapower` R package.
