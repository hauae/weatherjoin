# Cache helpers ---------------------------------------------------------------

#' @keywords internal
.cache_dir <- function(cache_dir = NULL, cache_scope = c("user","project"), pkg = "weatherjoin") {
  cache_scope <- match.arg(cache_scope)
  if (!is.null(cache_dir)) return(normalizePath(cache_dir, winslash = "/", mustWork = FALSE))
  if (cache_scope == "project") {
    return(normalizePath(file.path(getwd(), ".weatherjoin_cache"), winslash = "/", mustWork = FALSE))
  }
  if (exists("R_user_dir", where = asNamespace("tools"), inherits = FALSE)) {
    return(normalizePath(tools::R_user_dir(pkg, which = "cache"), winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(path.expand("~"), ".cache", pkg), winslash="/", mustWork = FALSE)
}

#' @keywords internal
.cache_init <- function(cache_dir = NULL, cache_scope = c("user","project"), pkg = "weatherjoin") {
  cache_scope <- match.arg(cache_scope)
  cache_dir <- .cache_dir(cache_dir, cache_scope, pkg)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  idx_path <- file.path(cache_dir, "index.rds")
  if (!file.exists(idx_path)) {
    idx <- data.table::data.table(
      key = character(),
      time_api = character(),
      params = character(),
      rep_lat = numeric(),
      rep_lon = numeric(),
      site_elevation = numeric(),
      start_utc = as.POSIXct(character(), tz="UTC"),
      end_utc = as.POSIXct(character(), tz="UTC"),
      created_utc = as.POSIXct(character(), tz="UTC"),
      file = character(),
      format = character(),
      n_rows = integer()
    )
    saveRDS(idx, idx_path)
  }
  invisible(cache_dir)
}

#' @keywords internal
.cache_read_index <- function(cache_dir) {
  idx_path <- file.path(cache_dir, "index.rds")
  if (!file.exists(idx_path)) return(data.table::data.table())
  data.table::as.data.table(readRDS(idx_path))
}

#' @keywords internal
.cache_write_index <- function(index, cache_dir) {
  saveRDS(data.table::as.data.table(index), file.path(cache_dir, "index.rds"))
  invisible(TRUE)
}

#' @keywords internal
.cache_data_path <- function(cache_dir, key) {
  fmt <- if (requireNamespace("fst", quietly = TRUE)) "fst" else "rds"
  list(format = fmt, path = file.path(cache_dir, paste0(key, ".", fmt)))
}

#' @keywords internal
.cache_key <- function(time_api, params, rep_lat, rep_lon, start_utc, end_utc, settings = list(),
                       coord_digits = 5) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    raw <- paste(time_api, .canon_params(params),
                 round(rep_lat, coord_digits), round(rep_lon, coord_digits),
                 format(as.POSIXct(start_utc, tz="UTC"), "%Y-%m-%d %H:%M:%S", tz="UTC"),
                 format(as.POSIXct(end_utc, tz="UTC"), "%Y-%m-%d %H:%M:%S", tz="UTC"),
                 jsonlite::toJSON(settings, auto_unbox = TRUE), sep="|")
    return(substr(raw, 1, 16))
  }
  s <- list(
    time_api = time_api,
    params = sort(unique(toupper(params))),
    rep_lat = round(as.numeric(rep_lat), coord_digits),
    rep_lon = round(as.numeric(rep_lon), coord_digits),
    start_utc = format(as.POSIXct(start_utc, tz="UTC"), "%Y-%m-%d %H:%M:%S", tz="UTC"),
    end_utc = format(as.POSIXct(end_utc, tz="UTC"), "%Y-%m-%d %H:%M:%S", tz="UTC"),
    settings = settings
  )
  digest::digest(s, algo = "xxhash64")
}

#' @keywords internal
.cache_register <- function(index, cache_dir, key, time_api, params, rep_lat, rep_lon, site_elevation, start_utc, end_utc, data) {
  .cache_init(cache_dir)
  info <- .cache_data_path(cache_dir, key)
  if (info$format == "fst") {
    fst::write_fst(data.table::as.data.table(data), info$path)
  } else {
    saveRDS(data.table::as.data.table(data), info$path)
  }

  now <- as.POSIXct(Sys.time(), tz="UTC")
  row <- data.table::data.table(
    .key = as.character(key),
    time_api = as.character(time_api),
    params = .canon_params(params),
    rep_lat = as.numeric(rep_lat),
    rep_lon = as.numeric(rep_lon),
    site_elevation = as.numeric(site_elevation),
    start_utc = as.POSIXct(start_utc, tz="UTC"),
    end_utc = as.POSIXct(end_utc, tz="UTC"),
    created_utc = now,
    file = basename(info$path),
    format = info$format,
    n_rows = nrow(data)
  )
  data.table::setnames(row, ".key", "key")

  index <- data.table::as.data.table(index)
  index <- index[!key %in% row$key]
  index <- data.table::rbindlist(list(index, row), use.names = TRUE, fill = TRUE)
  .cache_write_index(index, cache_dir)
  index
}

#' List cached weather segments
#'
#' Returns the cache index (one row per cached segment).
#'
#' @param cache_dir Optional explicit cache directory.
#' @param cache_scope Where to store cache by default: `"user"` or `"project"`.
#' @param pkg Package name used for `"user"` cache scope.
#' @return A data.table index of cached segments.
#' @export
wj_cache_list <- function(cache_dir = NULL, cache_scope = c("user","project"), pkg = "weatherjoin") {
  cache_scope <- match.arg(cache_scope)
  cache_dir <- .cache_dir(cache_dir, cache_scope, pkg)
  if (!file.exists(file.path(cache_dir, "index.rds"))) return(data.table::data.table())
  idx <- .cache_read_index(cache_dir)
  data.table::setorder(idx, time_api, params, rep_lat, rep_lon, site_elevation, start_utc)
  idx[]
}

#' Clear cached weather data
#'
#' Deletes cached files and (optionally) removes rows from the cache index.
#'
#' @param cache_dir Optional explicit cache directory.
#' @param cache_scope Where to store cache by default: `"user"` or `"project"`.
#' @param pkg Package name used for `"user"` cache scope.
#' @param filter Optional expression evaluated within the cache index to select entries to remove.
#' @param keep_index If `TRUE`, leaves index rows (useful for debugging); default `FALSE`.
#' @param dry_run If `TRUE`, prints what would be deleted but does not delete.
#' @param verbose If `TRUE`, prints progress.
#' @return Invisibly returns the rows selected for deletion.
#' @export
wj_cache_clear <- function(cache_dir = NULL, cache_scope = c("user","project"), pkg = "weatherjoin",
                          filter = NULL, keep_index = FALSE, dry_run = FALSE, verbose = TRUE) {
  cache_scope <- match.arg(cache_scope)
  cache_dir <- .cache_dir(cache_dir, cache_scope, pkg)
  idx_path <- file.path(cache_dir, "index.rds")
  if (!file.exists(idx_path)) return(invisible(NULL))
  idx <- .cache_read_index(cache_dir)

  if (!is.null(filter)) {
    to_drop <- idx[eval(filter)]
  } else {
    to_drop <- idx
  }
  if (nrow(to_drop) == 0L) {
    if (verbose) message("No cache entries matched the filter.")
    return(invisible(NULL))
  }
  files <- unique(file.path(cache_dir, to_drop$file))
  files <- files[file.exists(files)]
  if (verbose) {
    message("Entries selected: ", nrow(to_drop), "; files: ", length(files), if (dry_run) " (dry_run)" else "")
  }
  if (!dry_run) {
    if (length(files)) unlink(files, force = TRUE)
    if (!keep_index) {
      idx <- idx[!key %in% to_drop$key]
      .cache_write_index(idx, cache_dir)
    }
  }
  invisible(to_drop)
}

#' Upgrade cache index schema
#'
#' Ensures the cache index contains required columns and correct types.
#'
#' @param cache_dir Optional explicit cache directory.
#' @param cache_scope Where to store cache by default: `"user"` or `"project"`.
#' @param pkg Package name used for `"user"` cache scope.
#' @param verbose If `TRUE`, prints progress.
#' @return The upgraded cache index.
#' @export
wj_cache_upgrade_index <- function(cache_dir = NULL, cache_scope = c("user","project"), pkg = "weatherjoin",
                                  verbose = TRUE) {
  cache_scope <- match.arg(cache_scope)
  cache_dir <- .cache_dir(cache_dir, cache_scope, pkg)
  .cache_init(cache_dir, cache_scope, pkg)
  idx <- .cache_read_index(cache_dir)
  data.table::setDT(idx)

  required <- c("key","time_api","params","rep_lat","rep_lon","site_elevation","start_utc","end_utc","created_utc","file","format","n_rows")
  for (nm in required) if (!(nm %in% names(idx))) idx[, (nm) := NA]

  idx[, `:=`(
    rep_lat = as.numeric(rep_lat),
    rep_lon = as.numeric(rep_lon),
    site_elevation = suppressWarnings(as.numeric(site_elevation)),
    start_utc = as.POSIXct(start_utc, tz="UTC"),
    end_utc = as.POSIXct(end_utc, tz="UTC"),
    created_utc = as.POSIXct(created_utc, tz="UTC"),
    n_rows = as.integer(n_rows)
  )]

  .cache_write_index(idx, cache_dir)
  if (verbose) message("Index upgraded.")
  idx[]
}
