# weatherjoin

**weatherjoin** is an R package for attaching gridded weather data to event-based
datasets using efficient spatio-temporal planning and local caching.

The package is designed for workflows where weather variables need to be matched
to many observations across space and time, while minimising repeated data
downloads.

Currently, weather data are retrieved from the
[NASA POWER](https://power.larc.nasa.gov/) project via the
[`nasapower`](https://CRAN.R-project.org/package=nasapower) R package.

This package is not affiliated with or endorsed by NASA.

## Features

- Attach daily or hourly gridded weather data to event tables
- Automatic planning of efficient spatio-temporal API calls
- Local caching of downloaded weather segments for reuse
- Exact or rolling joins for hourly data
- Flexible handling of multiple locations (exact points, grouped sites, or spatial clustering)

## Installation

The package can be installed from GitHub:

```r
remotes::install_github("hauae/weatherjoin")
```

## Example

```r
library(weatherjoin)

out <- join_weather(
  x = events,
  params = c("T2M", "PRECTOTCORR"),
  time = "event_time",
  lat_col = "lat",
  lon_col = "lon"
)
```

## Caching

Downloaded weather data are cached locally and reused across calls.
By default, the cache is stored in a user-specific directory obtained via
tools::R_user_dir(). The cache can be inspected and managed using:

```r
wj_cache_list()
wj_cache_clear()

```

## Author and affiliation

Developed by Przemek Dolowy, Harper Adams University.

## License

MIT License.
