non_academic <- c("company", "government", "nonprofit", "healthcare")

#' Contributions by period
#'
#' Summarises each period of a career: how many outputs of each kind, the
#' share of software and data among them, open access, international and
#' cross-sector collaboration, first and last authorship, and the funding,
#' service, supervision and teaching recorded. Shares are computed only over
#' outputs for which the information is known, and the number of outputs
#' with that information is reported.
#'
#' Author order means different things in different fields; in fields that
#' list authors alphabetically, ignore `first_or_last_pct`.
#'
#' @param career An `ac_career`.
#' @param period Length of each period in years.
#'
#' @return A data frame of class `ac_contributions`, one row per period.
#' @export
#'
#' @examples
#' ac_contributions(ac_example("software"))
ac_contributions <- function(career, period = 5) {
  stopifnot(inherits(career, "ac_career"))
  ev <- career$events[!is.na(career$events$start), , drop = FALSE]
  if (!nrow(ev)) return(structure(data.frame(), class = c("ac_contributions", "data.frame")))
  first <- min(ev$start)
  starts <- seq(first, max(ev$start), by = period)
  rows <- lapply(starts, function(s) {
    e <- ev[ev$start >= s & ev$start < s + period, , drop = FALSE]
    out <- e[e$dimension %in% output_dimensions, , drop = FALSE]
    oa <- out$open_access[!is.na(out$open_access)]
    with_country <- out[!is.na(out$country), , drop = FALSE]
    intl <- vapply(with_country$country, function(c) length(split_values(c)) > 1, logical(1))
    with_sector <- out[!is.na(out$sectors), , drop = FALSE]
    cross <- vapply(with_sector$sectors, function(x) any(split_values(x) %in% non_academic), logical(1))
    pos <- out$author_position[!is.na(out$author_position)]
    count <- function(d) sum(e$dimension == d)
    data.frame(
      period = sprintf("%d-%d", s, s + period - 1),
      outputs = nrow(out),
      research_outputs = count("research_outputs"), software = count("software"), data = count("data"),
      software_data_pct = pct(count("software") + count("data"), nrow(out)),
      open_access_pct = pct(sum(oa), length(oa)), n_open_access_known = length(oa),
      international_pct = pct(sum(intl), nrow(with_country)),
      cross_sector_pct = pct(sum(cross), nrow(with_sector)), n_affiliation_known = nrow(with_country),
      first_or_last_pct = pct(sum(pos %in% c("first", "last")), length(pos)),
      funding = count("funding"), service = count("service"),
      supervision = count("supervision_mentoring"), teaching = count("teaching"),
      transfer = count("knowledge_transfer"), policy = count("science_policy"),
      engagement = count("public_engagement"),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  structure(out, class = c("ac_contributions", "data.frame"))
}

#' @export
print.ac_contributions <- function(x, ...) {
  df <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  cat("<ac_contributions: ", nrow(df), " period(s)>\n", sep = "")
  keep <- c("period", "outputs", "software_data_pct", "open_access_pct", "international_pct",
            "cross_sector_pct", "funding", "service", "supervision", "teaching", "transfer")
  print(df[, intersect(keep, names(df)), drop = FALSE], row.names = FALSE)
  cat("Shares (%) are over outputs with known information; see the n_* columns.\n")
  invisible(x)
}
