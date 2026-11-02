#' Career transitions
#'
#' Identifies changes in a career with transparent rules, not with a
#' statistical model:
#'
#' | Rule | Transition |
#' |---|---|
#' | `new_organisation` | A position starts at an organisation not seen before |
#' | `new_country` | A position starts in a country not seen before |
#' | `output_mix` | The share of software and data among outputs changes by at least `shift` percentage points between consecutive periods, with at least `min_outputs` outputs in each |
#' | `new_dimension` | Evidence appears for the first time in supervision, teaching, transfer, policy or engagement |
#' | `career_break` | A declared career break |
#'
#' @param career An `ac_career`.
#' @param period Length of the periods compared for `output_mix`.
#' @param shift Minimum change in percentage points for `output_mix`.
#' @param min_outputs Minimum outputs per period for `output_mix`.
#'
#' @return A data frame of class `ac_transitions` with columns `year`,
#'   `rule` and `description`.
#' @export
#'
#' @examples
#' ac_transitions(ac_example("software"))
ac_transitions <- function(career, period = 5, shift = 20, min_outputs = 3) {
  stopifnot(inherits(career, "ac_career"))
  ev <- career$events[!is.na(career$events$start), , drop = FALSE]
  out <- list()
  add <- function(year, rule, description) {
    out[[length(out) + 1L]] <<- data.frame(year = as.integer(year), rule = rule,
                                           description = description, stringsAsFactors = FALSE)
  }
  pos <- ev[ev$dimension == "employment", , drop = FALSE]
  pos <- pos[order(pos$start), , drop = FALSE]
  seen_org <- character(0)
  seen_country <- character(0)
  for (i in seq_len(nrow(pos))) {
    org <- pos$organisation[i]
    if (!is.na(org) && !org %in% seen_org) {
      if (length(seen_org)) add(pos$start[i], "new_organisation",
                                paste0(pos$title[i] %||% "Position", " at ", org))
      seen_org <- c(seen_org, org)
    }
    for (cty in split_values(pos$country[i])) {
      if (!cty %in% seen_country) {
        if (length(seen_country)) add(pos$start[i], "new_country", paste0("First position in ", cty))
        seen_country <- c(seen_country, cty)
      }
    }
  }
  contrib <- ac_contributions(career, period)
  if (nrow(contrib) > 1) {
    for (i in 2:nrow(contrib)) {
      a <- contrib[i - 1, ]
      b <- contrib[i, ]
      if (a$outputs >= min_outputs && b$outputs >= min_outputs &&
          abs(b$software_data_pct - a$software_data_pct) >= shift) {
        add(as.integer(substr(b$period, 1, 4)), "output_mix",
            sprintf("Software and data went from %s%% of outputs (%s) to %s%% (%s)",
                    a$software_data_pct, a$period, b$software_data_pct, b$period))
      }
    }
  }
  for (d in c("supervision_mentoring", "teaching", "knowledge_transfer", "science_policy",
              "public_engagement")) {
    e <- ev[ev$dimension == d, , drop = FALSE]
    if (nrow(e)) add(min(e$start), "new_dimension", paste("First evidence of", tolower(dimension_label(d))))
  }
  br <- career$context$breaks
  for (i in seq_len(nrow(br))) {
    add(br$start[i], "career_break", sprintf("Career break %d-%d%s", br$start[i], br$end[i],
                                             if (!is.na(br$reason[i])) paste0(": ", br$reason[i]) else ""))
  }
  res <- if (length(out)) do.call(rbind, out) else
    data.frame(year = integer(0), rule = character(0), description = character(0))
  res <- res[order(res$year), , drop = FALSE]
  rownames(res) <- NULL
  structure(res, class = c("ac_transitions", "data.frame"))
}

#' @export
print.ac_transitions <- function(x, ...) {
  if (!all(c("year", "rule", "description") %in% names(x))) return(NextMethod())
  cat("<ac_transitions: ", nrow(x), ">\n", sep = "")
  for (i in seq_len(nrow(x))) cat(sprintf("  %d  %-17s %s\n", x$year[i], x$rule[i], x$description[i]))
  invisible(x)
}
