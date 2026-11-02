event_columns <- c("event_id", "dimension", "type", "title", "start", "end", "role",
                   "organisation", "country", "sectors", "open_access",
                   "author_position", "doi", "url", "source", "declared_by")

empty_events <- function() {
  data.frame(event_id = character(0), dimension = character(0), type = character(0),
             title = character(0), start = integer(0), end = integer(0),
             role = character(0), organisation = character(0), country = character(0),
             sectors = character(0), open_access = logical(0),
             author_position = character(0), doi = character(0), url = character(0),
             source = character(0), declared_by = character(0), stringsAsFactors = FALSE)
}

# Fill missing columns and coerce types, so every table of events looks alike.
normalise_events <- function(events) {
  if (is.null(events) || !nrow(events)) return(empty_events())
  for (col in setdiff(event_columns, names(events))) events[[col]] <- NA
  events <- events[, event_columns]
  for (col in c("start", "end")) events[[col]] <- suppressWarnings(as.integer(events[[col]]))
  events$open_access <- as.logical(events$open_access)
  chr <- setdiff(event_columns, c("start", "end", "open_access"))
  for (col in chr) events[[col]] <- as.character(events[[col]])
  bad <- !events$dimension %in% ac_dimensions()$id
  if (any(bad)) {
    stop("Unknown dimension(s): ", paste(unique(events$dimension[bad]), collapse = ", "),
         ". See ac_dimensions().", call. = FALSE)
  }
  missing_id <- is.na(events$event_id) | !nzchar(events$event_id)
  events$event_id[missing_id] <- sprintf("ev%04d", which(missing_id))
  events$declared_by[is.na(events$declared_by)] <- "source"
  events[order(events$start, events$dimension, na.last = TRUE), , drop = FALSE]
}

#' Create a career from a table of events
#'
#' Most careers are built with [ac_collect()] from an ORCID iD. `ac_career()`
#' builds one from a table instead, for researchers without an ORCID record,
#' for teaching, or for examples.
#'
#' @param events A data frame with at least `dimension` (see
#'   [ac_dimensions()]), `title` and `start` (a year). Optional columns:
#'   `end`, `type`, `role`, `organisation`, `country` (ISO codes separated by
#'   `;`), `sectors`, `open_access`, `author_position`, `doi`, `url`,
#'   `source` and `declared_by` (`"source"` or `"self"`).
#' @param name Optional name of the person.
#' @param orcid Optional ORCID iD.
#'
#' @return An object of class `ac_career`.
#' @export
#'
#' @examples
#' ev <- data.frame(
#'   dimension = c("employment", "research_outputs", "software"),
#'   title = c("Postdoctoral researcher", "A study of things", "thingr R package"),
#'   start = c(2018, 2019, 2021), end = c(2021, NA, NA)
#' )
#' ac_career(ev, name = "Researcher A")
ac_career <- function(events, name = NULL, orcid = NULL) {
  if (!is.data.frame(events)) stop("`events` must be a data frame.", call. = FALSE)
  if (!all(c("dimension", "title", "start") %in% names(events))) {
    stop("`events` needs the columns `dimension`, `title` and `start`.", call. = FALSE)
  }
  if (!is.null(orcid)) orcid <- ac_orcid(orcid)
  structure(list(
    name = name, orcid = orcid,
    events = normalise_events(events),
    sources = data.frame(source = character(0), status = character(0), n = integer(0),
                         retrieved_at = character(0), detail = character(0),
                         stringsAsFactors = FALSE),
    context = list(breaks = data.frame(start = integer(0), end = integer(0),
                                       reason = character(0), stringsAsFactors = FALSE),
                   field = NULL, notes = NULL)
  ), class = "ac_career")
}

#' @export
print.ac_career <- function(x, ...) {
  ev <- x$events
  cat("<ac_career", if (!is.null(x$name)) paste0(": ", x$name), ">\n", sep = "")
  if (!is.null(x$orcid)) cat("  ORCID:    https://orcid.org/", x$orcid, "\n", sep = "")
  years <- c(ev$start, ev$end)
  if (any(!is.na(years))) {
    cat("  Span:     ", min(years, na.rm = TRUE), "-", max(years, na.rm = TRUE), "\n", sep = "")
  }
  cat("  Evidence: ", nrow(ev), " items (", sum(ev$declared_by == "self"),
      " declared by the researcher)\n", sep = "")
  if (nrow(ev)) {
    tab <- table(factor(ev$dimension, levels = ac_dimensions()$id))
    tab <- tab[tab > 0]
    for (d in names(tab)) cat(sprintf("    %-26s %d\n", dimension_label(d), tab[[d]]))
  }
  if (nrow(x$sources)) {
    cat("  Sources:  ", paste(sprintf("%s (%s)", x$sources$source, x$sources$status),
                              collapse = ", "), "\n", sep = "")
  }
  if (nrow(x$context$breaks)) cat("  Career breaks declared: ", nrow(x$context$breaks), "\n", sep = "")
  invisible(x)
}

#' Add evidence declared by the researcher
#'
#' Open sources miss much of what a career contains: supervision, teaching,
#' mentoring, policy advice, outreach, transfer contracts. Evidence added
#' here is marked as declared by the researcher, so reports always show
#' which evidence comes from a source and which from the person.
#'
#' @param career An `ac_career`.
#' @param dimension One of [ac_dimensions()]`$id`.
#' @param title Short description, such as `"Supervision of a PhD thesis"`.
#' @param start Year it started or happened.
#' @param end Optional year it ended.
#' @param type,role,organisation,url Optional details.
#' @param country Optional ISO country codes, separated by `;`.
#'
#' @return The career, with the evidence added.
#' @export
#'
#' @examples
#' career <- ac_example("traditional")
#' career <- ac_add_evidence(career, "supervision_mentoring",
#'                           "Supervision of a PhD thesis", start = 2019, end = 2023,
#'                           role = "Co-supervisor")
#' tail(career$events[, c("dimension", "title", "declared_by")], 1)
ac_add_evidence <- function(career, dimension, title, start, end = NA, type = NA,
                            role = NA, organisation = NA, country = NA, url = NA) {
  stopifnot(inherits(career, "ac_career"))
  row <- data.frame(event_id = sprintf("self%03d", sum(career$events$declared_by == "self") + 1L),
                    dimension = dimension, type = type, title = title, start = start,
                    end = end, role = role, organisation = organisation, country = country,
                    url = url, source = "self", declared_by = "self",
                    stringsAsFactors = FALSE)
  career$events <- normalise_events(rbind(career$events, normalise_events(row)))
  career
}

#' Read evidence declared by the researcher from a CSV file
#'
#' @param career An `ac_career`.
#' @param file A CSV file with the columns `dimension`, `title` and `start`,
#'   and optionally any of `end`, `type`, `role`, `organisation`, `country`
#'   and `url`.
#'
#' @return The career, with the evidence added.
#' @export
#'
#' @examples
#' f <- tempfile(fileext = ".csv")
#' write.csv(data.frame(dimension = "teaching", title = "Research methods course",
#'                      start = 2020, end = 2024), f, row.names = FALSE)
#' career <- ac_read_evidence(ac_example("traditional"), f)
ac_read_evidence <- function(career, file) {
  stopifnot(inherits(career, "ac_career"))
  ev <- utils::read.csv(file, encoding = "UTF-8", stringsAsFactors = FALSE)
  if (!all(c("dimension", "title", "start") %in% names(ev))) {
    stop("The file needs the columns `dimension`, `title` and `start`.", call. = FALSE)
  }
  ev$source <- "self"
  ev$declared_by <- "self"
  ev$event_id <- sprintf("self%03d", sum(career$events$declared_by == "self") + seq_len(nrow(ev)))
  career$events <- normalise_events(rbind(career$events, normalise_events(ev)))
  career
}

#' Declare the context of a career
#'
#' Career breaks (parental leave, illness, caring) are shown as breaks in
#' timelines and reports, not as gaps in activity, as narrative CV
#' frameworks and the Coalition for Advancing Research Assessment recommend.
#'
#' @param career An `ac_career`.
#' @param breaks Optional data frame with `start` and `end` (years) and
#'   `reason`.
#' @param field Optional research field, which matters when reading author
#'   order and output types.
#' @param notes Optional free text shown in reports.
#'
#' @return The career, with its context.
#' @export
#'
#' @examples
#' career <- ac_context(ac_example("software"),
#'                      breaks = data.frame(start = 2016, end = 2017, reason = "Parental leave"),
#'                      field = "Computational biology")
#' career$context$breaks
ac_context <- function(career, breaks = NULL, field = NULL, notes = NULL) {
  stopifnot(inherits(career, "ac_career"))
  if (!is.null(breaks)) {
    if (!is.data.frame(breaks) || !all(c("start", "end") %in% names(breaks))) {
      stop("`breaks` must be a data frame with `start` and `end`.", call. = FALSE)
    }
    breaks$reason <- breaks$reason %||% NA_character_
    career$context$breaks <- data.frame(start = as.integer(breaks$start), end = as.integer(breaks$end),
                                        reason = as.character(breaks$reason), stringsAsFactors = FALSE)
  }
  if (!is.null(field)) career$context$field <- field
  if (!is.null(notes)) career$context$notes <- notes
  career
}
