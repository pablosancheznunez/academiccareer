`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

read_extdata_csv <- function(file) {
  utils::read.csv(system.file("extdata", file, package = "academiccareer"),
                  encoding = "UTF-8", stringsAsFactors = FALSE)
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

as_year <- function(x) {
  y <- suppressWarnings(as.integer(substr(as.character(x %||% NA), 1, 4)))
  if (length(y) != 1L) NA_integer_ else y
}

collapse <- function(x) {
  x <- unique(x[!is.na(x) & nzchar(x)])
  if (!length(x)) NA_character_ else paste(sort(x), collapse = "; ")
}

split_values <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  trimws(strsplit(x, ";", fixed = TRUE)[[1]])
}

pct <- function(num, den) if (den > 0) round(100 * num / den) else NA_real_

#' Dimensions of a career and the coverage of each source
#'
#' The vocabulary of dimensions used by the package, with labels in English
#' and Spanish, the open sources that record each one and how well they
#' cover it. Coverage is `"good"`, `"partial"` or `"low"`. When a dimension
#' has low coverage, finding no evidence says almost nothing: the researcher
#' should declare it with [ac_add_evidence()].
#'
#' @return A data frame with columns `id`, `en`, `es`, `sources`, `coverage`
#'   and `description`.
#' @export
#'
#' @examples
#' ac_dimensions()[, c("id", "sources", "coverage")]
ac_dimensions <- function() read_extdata_csv("dimensions.csv")

dimension_label <- function(ids, lang = "en") {
  d <- ac_dimensions()
  out <- d[[lang]][match(ids, d$id)]
  ifelse(is.na(out), ids, out)
}

output_dimensions <- c("research_outputs", "software", "data")
