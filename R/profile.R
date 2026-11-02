strings <- function(lang) {
  tab <- read_extdata_csv("i18n.csv")
  out <- as.list(tab[[lang]])
  names(out) <- tab$key
  out
}

fill <- function(template, values) {
  for (n in names(values)) template <- gsub(paste0("{", n, "}"), values[[n]], template, fixed = TRUE)
  template
}

#' Descriptive profile of a career
#'
#' Highlights the contributions that stand out in the evidence, with
#' transparent thresholds, so that careers built on software, data,
#' transfer, teaching, service or open science become visible. The profile
#' is descriptive: it never compares the person with others and never uses
#' words such as "better" or "more productive".
#'
#' | Contribution | Highlighted when |
#' |---|---|
#' | `software_data` | Software and data are at least `thresholds$software_data`% of outputs (3 or more) |
#' | `open_science` | At least `thresholds$open_access`% of outputs with known status are open access (5 or more known) |
#' | `transfer` | At least `thresholds$cross_sector`% of outputs with known affiliations involve non-academic partners (5 or more known), or transfer activities are recorded |
#' | `international` | At least `thresholds$international`% of outputs with known affiliations are international (5 or more known) |
#' | `teaching_mentoring` | Any teaching, supervision or mentoring is recorded |
#' | `service` | At least 5 peer review or service activities |
#' | `policy_engagement` | Any science policy or public engagement activity |
#' | `funding` | Any funding |
#'
#' @param career An `ac_career`.
#' @param lang Language of the statements: `"en"` or `"es"`.
#' @param thresholds Named list of percentage thresholds.
#'
#' @return An object of class `ac_profile`: a list with `span`, `counts`
#'   (evidence by dimension and origin), `highlights` (a data frame with
#'   `contribution` and `statement`), `no_evidence` (low-coverage dimensions
#'   with no evidence) and `statements`.
#' @export
#'
#' @examples
#' ac_profile(ac_example("software"))
#' ac_profile(ac_example("teaching"), lang = "es")
ac_profile <- function(career, lang = c("en", "es"),
                       thresholds = list(software_data = 20, open_access = 70,
                                         cross_sector = 25, international = 50)) {
  stopifnot(inherits(career, "ac_career"))
  lang <- match.arg(lang)
  s <- strings(lang)
  ev <- career$events
  out <- ev[ev$dimension %in% output_dimensions, , drop = FALSE]
  n_dim <- function(d) sum(ev$dimension %in% d)
  hl <- list()
  add <- function(id, key, values) {
    hl[[length(hl) + 1L]] <<- data.frame(contribution = id, statement = fill(s[[key]], values),
                                         stringsAsFactors = FALSE)
  }

  sd <- sum(out$dimension %in% c("software", "data"))
  if (sd >= 3 && pct(sd, nrow(out)) >= thresholds$software_data) {
    add("software_data", "software_data", list(pct = pct(sd, nrow(out)), n = sd, total = nrow(out)))
  }
  oa <- out$open_access[!is.na(out$open_access)]
  if (length(oa) >= 5 && pct(sum(oa), length(oa)) >= thresholds$open_access) {
    add("open_science", "open_science", list(pct = pct(sum(oa), length(oa)), n = sum(oa), total = length(oa)))
  }
  sect <- out$sectors[!is.na(out$sectors)]
  cross <- vapply(sect, function(x) any(split_values(x) %in% non_academic), logical(1))
  if (length(sect) >= 5 && pct(sum(cross), length(sect)) >= thresholds$cross_sector) {
    add("transfer", "transfer", list(pct = pct(sum(cross), length(sect)), n = sum(cross), total = length(sect)))
  } else if (n_dim("knowledge_transfer") > 0) {
    add("transfer", "transfer_events", list(n = n_dim("knowledge_transfer")))
  }
  ctry <- out$country[!is.na(out$country)]
  intl <- vapply(ctry, function(x) length(split_values(x)) > 1, logical(1))
  if (length(ctry) >= 5 && pct(sum(intl), length(ctry)) >= thresholds$international) {
    add("international", "international", list(pct = pct(sum(intl), length(ctry)), n = sum(intl), total = length(ctry)))
  }
  tm <- n_dim(c("teaching", "supervision_mentoring"))
  if (tm > 0) add("teaching_mentoring", "teaching_mentoring", list(n = tm))
  if (n_dim("service") >= 5) add("service", "service", list(n = n_dim("service")))
  pe <- n_dim(c("science_policy", "public_engagement"))
  if (pe > 0) add("policy_engagement", "policy_engagement", list(n = pe))
  if (n_dim("funding") > 0) add("funding", "funding", list(n = n_dim("funding")))

  dims <- ac_dimensions()
  low <- dims$id[dims$coverage == "low" & !dims$id %in% ev$dimension]
  years <- c(ev$start, ev$end)
  span <- if (any(!is.na(years))) range(years, na.rm = TRUE) else c(NA, NA)
  counts <- as.data.frame(table(dimension = factor(ev$dimension, levels = dims$id),
                                origin = factor(ev$declared_by, levels = c("source", "self"))),
                          stringsAsFactors = FALSE)
  counts <- stats_reshape(counts)
  highlights <- if (length(hl)) do.call(rbind, hl) else
    data.frame(contribution = character(0), statement = character(0))
  statements <- c(
    if (!is.na(span[1])) fill(s$span, list(from = span[1], to = span[2])),
    if (nrow(highlights)) highlights$statement else s$none_distinctive,
    if (length(low)) fill(s$no_evidence, list(dims = paste(tolower(dimension_label(low, lang)), collapse = ", ")))
  )
  structure(list(name = career$name, orcid = career$orcid, span = span, counts = counts,
                 highlights = highlights, no_evidence = low, statements = statements, lang = lang),
            class = "ac_profile")
}

stats_reshape <- function(counts) {
  dims <- unique(counts$dimension)
  out <- data.frame(dimension = dims,
                    from_sources = counts$Freq[counts$origin == "source"][match(dims, counts$dimension[counts$origin == "source"])],
                    declared = counts$Freq[counts$origin == "self"][match(dims, counts$dimension[counts$origin == "self"])],
                    stringsAsFactors = FALSE)
  out[out$from_sources + out$declared > 0, , drop = FALSE]
}

#' @export
print.ac_profile <- function(x, ...) {
  cat("<ac_profile", if (!is.null(x$name)) paste0(": ", x$name), ">\n", sep = "")
  for (st in x$statements) cat(strwrap(st, width = 76, prefix = "  "), sep = "\n")
  cat("\n")
  print(x$counts, row.names = FALSE)
  invisible(x)
}
