make_time_fixture <- function() {
  data.table::data.table(
    lat = 51.5,
    lon = -0.1,
    
    # Single-column POSIXct (hourly-capable)
    event_time_posix = as.POSIXct(
      c("2024-06-01 12:00:00", "2024-06-01 13:00:00", "2024-06-02 12:00:00"),
      tz = "UTC"
    ),
    
    # Single-column Date (daily)
    event_date = as.Date(c("2024-06-01", "2024-06-01", "2024-06-02")),
    
    # Character datetime strings (hourly-capable)
    event_time_chr = c("2024-06-01 12:00:00", "2024-06-01 13:00:00", "2024-06-02 12:00:00"),
    
    # Character date strings (daily-only by convention; parsing yields midnight)
    event_date_chr = c("2024-06-01", "2024-06-01", "2024-06-02"),
    
    # Numeric YYYYMMDD (daily)
    event_yyyymmdd = c(20240601, 20240601, 20240602),
    
    # Split columns (hourly)
    YEAR = c(2024, 2024, 2024),
    MO   = c(6,    6,    6),
    DY   = c(1,    1,    2),
    HR   = c(12,   13,   12),
    
    # Split columns (daily)
    YYYY = c(2024, 2024, 2024),
    MM   = c(6,    6,    6),
    DD   = c(1,    1,    2),
    
    # YEAR + DOY (daily)
    DOY  = c(153, 153, 154) # 2024-06-01 is DOY 153 (leap year)
  )
}
