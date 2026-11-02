#' Timeline of a career
#'
#' Counts the evidence in each dimension, year by year. Outputs count in the
#' year they appeared; positions, funding and other periods count in every
#' year they were active (an open period runs to the current year). Years
#' inside a declared career break are marked.
#'
#' @param career An `ac_career`.
#' @param from,to Optional first and last year.
#'
#' @return A data frame of class `ac_timeline` with columns `year`,
#'   `dimension`, `n` and `break_year`.
#' @export
#'
#' @examples
#' tl <- ac_timeline(ac_example("software"))
#' head(tl)
#' plot(tl)
ac_timeline <- function(career, from = NULL, to = NULL) {
  stopifnot(inherits(career, "ac_career"))
  ev <- career$events[!is.na(career$events$start), , drop = FALSE]
  now <- as.integer(format(Sys.Date(), "%Y"))
  last <- ifelse(is.na(ev$end), ifelse(ev$dimension %in% c("employment", "funding", "education",
                                                           "service", "teaching", "supervision_mentoring"),
                                       pmin(now, max(c(ev$start, now))), ev$start), ev$end)
  from <- from %||% if (nrow(ev)) min(ev$start) else now
  to <- to %||% if (nrow(ev)) max(c(last, ev$start)) else now
  years <- seq(from, to)
  dims <- ac_dimensions()$id
  present <- dims[dims %in% ev$dimension]
  grid <- expand.grid(year = years, dimension = present, stringsAsFactors = FALSE)
  grid$n <- vapply(seq_len(nrow(grid)), function(i) {
    sum(ev$dimension == grid$dimension[i] & ev$start <= grid$year[i] & last >= grid$year[i])
  }, integer(1))
  br <- career$context$breaks
  grid$break_year <- vapply(grid$year, function(y) any(br$start <= y & br$end >= y), logical(1))
  grid <- grid[order(match(grid$dimension, dims), grid$year), , drop = FALSE]
  rownames(grid) <- NULL
  structure(grid, class = c("ac_timeline", "data.frame"))
}

#' @export
print.ac_timeline <- function(x, ...) {
  if (!all(c("year", "dimension", "n") %in% names(x))) return(NextMethod())
  years <- sort(unique(x$year))
  cat("<ac_timeline: ", min(years), "-", max(years), ">\n", sep = "")
  shades <- c(".", ":", "+", "#")
  for (d in unique(x$dimension)) {
    rows <- x[x$dimension == d, , drop = FALSE]
    cells <- ifelse(rows$n == 0, ifelse(rows$break_year, "~", " "),
                    shades[pmin(4L, as.integer(cut(rows$n, c(0, 1, 3, 6, Inf))))])
    cat(sprintf("  %-26s %s\n", substr(dimension_label(d), 1, 26), paste(cells, collapse = "")))
  }
  cat(sprintf("  %-26s %s%s%s\n", "", min(years), strrep(" ", max(0, length(years) - 8)), max(years)))
  cat("  . 1  : 2-3  + 4-6  # 7+  ~ career break\n")
  invisible(x)
}

#' @export
plot.ac_timeline <- function(x, ...) {
  years <- sort(unique(x$year))
  dims <- unique(x$dimension)
  m <- matrix(0, nrow = length(years), ncol = length(dims))
  for (j in seq_along(dims)) m[, j] <- x$n[x$dimension == dims[j]][order(x$year[x$dimension == dims[j]])]
  pal <- c("#f1f1ee", grDevices::colorRampPalette(c("#cfdcd6", "#3d6b5e", "#173a32"))(6))
  brks <- c(-0.5, 0.5, 1.5, 2.5, 4.5, 6.5, 10.5, max(11, max(m) + 1))
  op <- graphics::par(mar = c(3, 12, 2, 1))
  on.exit(graphics::par(op))
  graphics::image(years, seq_along(dims), m, col = pal, breaks = brks, axes = FALSE,
                  xlab = "", ylab = "", main = "Evidence by year and dimension", ...)
  br <- unique(x$year[x$break_year])
  if (length(br)) {
    graphics::rect(br - 0.5, 0.5, br + 0.5, length(dims) + 0.5, density = 12,
                   col = "#b04a2e", border = NA)
  }
  graphics::axis(1, at = pretty(years), las = 1)
  graphics::axis(2, at = seq_along(dims), labels = dimension_label(dims), las = 1, tick = FALSE)
  invisible(x)
}
