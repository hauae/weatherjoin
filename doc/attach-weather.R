## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----eval=FALSE---------------------------------------------------------------
# library(data.table)
# library(weatherjoin)
# 
# X <- data.table(
#   event_time = as.POSIXct(c("2024-04-07 12:30:00","2024-04-07 13:30:00"), tz="UTC"),
#   LAT = c(51.5, 51.5),
#   LON = c(-0.1, -0.1)
# )
# 
# out <- join_weather(
#   X,
#   params = c("T2M"),
#   time = "event_time",
#   lat_col = "LAT",
#   lon_col = "LON",
#   time_api = "hourly",
#   roll = "nearest",
#   roll_max_hours = 1
# )

