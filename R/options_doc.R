#' weatherjoin options
#'
#' Most users will not need to change package options. Advanced configuration can be
#' controlled via \code{options()}.
#'
#' ## Cache policy
#' \itemize{
#' \item \code{weatherjoin.cache_max_age_days} Cache entries older than this (days) are considered stale (default \code{30}).
#' \item \code{weatherjoin.cache_refresh} When to refetch: one of \code{"if_missing"}, \code{"if_stale"}, \code{"always"} (default \code{"if_missing"}).
#' \item \code{weatherjoin.cache_match_mode} Cache matching mode: \code{"cover"} (cached window covers requested) or \code{"exact"} (default \code{"cover"}).
#' \item \code{weatherjoin.cache_param_match} Parameter matching for cache reuse: \code{"superset"} or \code{"exact"} (default \code{"superset"}).
#' \item \code{weatherjoin.cache_pkg} Internal namespace used when \code{cache_scope="user"} (default \code{"weatherjoin"}).
#' }
#'
#' ## Time splitting and call planning
#' These options control how sparse time series are split into separate provider calls.
#' They are primarily performance controls; incorrect values will not change the meaning
#' of returned weather values, only how much data is downloaded and cached.
#'
#' \itemize{
#' \item \code{weatherjoin.split_penalty_hours} Gap threshold (hours). Larger values yield fewer, wider time windows (default \code{72}).
#' \item \code{weatherjoin.pad_hours} Padding (hours) added to both ends of each planned time window (default \code{0}).
#' \item \code{weatherjoin.max_parts} Maximum number of planned time windows per representative location (default \code{50}).
#' }
#'
#' ## Time construction
#' \itemize{
#' \item \code{weatherjoin.dummy_hour} Hour used when constructing daily timestamps (default \code{12}).
#' }
#'
#' ## Diagnostics
#' \itemize{
#' \item \code{weatherjoin.keep_rep_cols} If \code{TRUE}, keep representative-location diagnostics (rep_lon/rep_lat, distance, elevation) in outputs (default \code{FALSE}).
#' }
#'
#' Use \pkg{withr} for temporary changes:
#' \preformatted{
#' withr::local_options(list(
#'   weatherjoin.split_penalty_hours = 168,
#'   weatherjoin.max_parts = 25
#' ))
#' }
#'
#' @name weatherjoin_options
NULL
