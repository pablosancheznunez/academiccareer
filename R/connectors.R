# Connectors to the public APIs of ORCID, OpenAlex and DataCite. Each
# normaliser turns one API response into a table of events.

ac_sources <- c("orcid", "openalex", "datacite")

#' Validate an ORCID iD
#'
#' Accepts the bare form (`"0000-0002-1825-0097"`) or the URL form, checks
#' the format and the ISO 7064 11,2 check digit, and returns the bare iD.
#'
#' @param id An ORCID iD.
#'
#' @return The bare iD, or an error if it is not valid.
#' @export
#'
#' @examples
#' ac_orcid("https://orcid.org/0000-0002-1825-0097")
#' try(ac_orcid("0000-0002-1825-0098"))
ac_orcid <- function(id) {
  bare <- sub("^https?://(www\\.)?orcid\\.org/", "", trimws(as.character(id)[1]))
  ok <- grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$", bare)
  if (ok) {
    digits <- strsplit(gsub("-", "", bare), "")[[1]]
    total <- 0
    for (d in digits[1:15]) total <- (total + as.integer(d)) * 2
    check <- (12 - (total %% 11)) %% 11
    ok <- digits[16] == if (check == 10) "X" else as.character(check)
  }
  if (!ok) stop("Not a valid ORCID iD: ", id, call. = FALSE)
  bare
}

orcid_work_dimension <- function(type) {
  switch(type %||% "other",
    "software" = "software",
    "data-set" = , "data-management-plan" = "data",
    "patent" = , "license" = , "registered-copyright" = , "trademark" = "knowledge_transfer",
    "supervised-student-publication" = "supervision_mentoring",
    "learning-object" = "teaching",
    "standards-and-policy" = "science_policy",
    "lecture-speech" = , "public-speech" = , "online-resource" = , "website" = ,
    "newspaper-article" = , "magazine-article" = , "media-appearance" = "public_engagement",
    "research_outputs")
}

datacite_dimension <- function(type) {
  switch(type %||% "Other",
    "Software" = , "ComputationalNotebook" = "software",
    "Dataset" = , "DataPaper" = "data",
    "research_outputs")
}

value_of <- function(x) if (is.null(x)) NA_character_ else as.character(x$value %||% NA)

doi_from_ids <- function(ids) {
  for (x in ids[["external-id"]] %||% list()) {
    if (identical(x[["external-id-type"]], "doi")) return(tolower(x[["external-id-value"]]))
  }
  NA_character_
}

clean_doi <- function(x) {
  x <- tolower(sub("^https?://(dx\\.)?doi\\.org/", "", x %||% NA_character_))
  if (length(x) != 1L || is.na(x) || !nzchar(x)) NA_character_ else x
}

ev <- function(...) {
  args <- list(...)
  args <- lapply(args, function(v) if (is.null(v) || length(v) == 0L) NA else v[[1]])
  as.data.frame(args, stringsAsFactors = FALSE)
}

bind <- function(rows) {
  if (!length(rows)) return(empty_events())
  out <- do.call(rbind, lapply(rows, normalise_events))
  out$event_id <- NA
  normalise_events(out)
}

#' @rdname ac_normalise
#' @title Turn API responses into events
#' @description Normalisers used by [ac_collect()]. They are exported so
#'   that responses saved earlier can be read again without the network.
#' @param json A parsed JSON response (a list), as returned by
#'   `jsonlite::fromJSON(..., simplifyVector = FALSE)`.
#' @param orcid For OpenAlex, the ORCID iD of the person, used to find their
#'   position in the author list.
#' @return A data frame of events.
#' @export
#' @examples
#' f <- system.file("extdata", "fixtures", "orcid.json", package = "academiccareer")
#' ev <- ac_normalise_orcid(jsonlite::fromJSON(f, simplifyVector = FALSE))
#' ev[, c("dimension", "title", "start")]
ac_normalise_orcid <- function(json) {
  a <- json[["activities-summary"]] %||% list()
  rows <- list()
  affil <- c(employments = "employment", educations = "education", qualifications = "education",
             `invited-positions` = "employment", distinctions = "distinctions",
             memberships = "service", services = "service")
  for (section in names(affil)) {
    for (g in a[[section]][["affiliation-group"]] %||% list()) {
      for (s in g$summaries %||% list()) {
        x <- s[[1]]
        org <- x$organization %||% list()
        rows[[length(rows) + 1L]] <- ev(
          dimension = affil[[section]], type = sub("s$", "", section),
          title = x[["role-title"]] %||% x[["department-name"]] %||% org$name,
          start = as_year(value_of(x[["start-date"]]$year)),
          end = as_year(value_of(x[["end-date"]]$year)),
          role = x[["role-title"]], organisation = org$name,
          country = org$address$country, url = value_of(x$url),
          source = "orcid", declared_by = "source")
      }
    }
  }
  for (g in a$fundings$group %||% list()) {
    x <- g[["funding-summary"]][[1]]
    rows[[length(rows) + 1L]] <- ev(
      dimension = "funding", type = x$type, title = value_of(x$title$title),
      start = as_year(value_of(x[["start-date"]]$year)), end = as_year(value_of(x[["end-date"]]$year)),
      organisation = x$organization$name, country = x$organization$address$country,
      url = value_of(x$url), source = "orcid", declared_by = "source")
  }
  for (g in a$works$group %||% list()) {
    x <- g[["work-summary"]][[1]]
    doi <- doi_from_ids(x[["external-ids"]])
    rows[[length(rows) + 1L]] <- ev(
      dimension = orcid_work_dimension(x$type), type = x$type,
      title = value_of(x$title$title), start = as_year(value_of(x[["publication-date"]]$year)),
      doi = doi, url = if (!is.na(doi)) paste0("https://doi.org/", doi) else value_of(x$url),
      source = "orcid", declared_by = "source")
  }
  for (g in a[["peer-reviews"]]$group %||% list()) {
    for (pg in g[["peer-review-group"]] %||% list()) {
      for (x in pg[["peer-review-summary"]] %||% list()) {
        rows[[length(rows) + 1L]] <- ev(
          dimension = "service", type = "peer-review",
          title = paste("Peer review", if (!is.null(x[["convening-organization"]]$name))
            paste("for", x[["convening-organization"]]$name) else ""),
          start = as_year(value_of(x[["completion-date"]]$year)), role = x[["reviewer-role"]],
          organisation = x[["convening-organization"]]$name, source = "orcid", declared_by = "source")
      }
    }
  }
  bind(rows)
}

#' @rdname ac_normalise
#' @export
ac_normalise_openalex <- function(json, orcid = NULL) {
  rows <- lapply(json$results %||% list(), function(w) {
    countries <- character(0)
    sectors <- character(0)
    position <- NA_character_
    for (au in w$authorships %||% list()) {
      for (inst in au$institutions %||% list()) {
        countries <- c(countries, inst$country_code %||% NA)
        sectors <- c(sectors, inst$type %||% NA)
      }
      if (!is.null(orcid) && identical(sub("^https://orcid.org/", "", au$author$orcid %||% ""), orcid)) {
        position <- au$author_position %||% NA_character_
      }
    }
    ev(dimension = if (identical(w$type, "dataset")) "data" else "research_outputs",
       type = w$type, title = w$title %||% w$display_name, start = as_year(w$publication_year),
       country = collapse(countries), sectors = collapse(sectors),
       open_access = w$open_access$is_oa %||% NA, author_position = position,
       doi = clean_doi(w$doi), url = w$doi %||% w$id, source = "openalex", declared_by = "source")
  })
  bind(rows)
}

#' @rdname ac_normalise
#' @export
ac_normalise_datacite <- function(json) {
  rows <- lapply(json$data %||% list(), function(d) {
    a <- d$attributes
    doi <- clean_doi(a$doi)
    ev(dimension = datacite_dimension(a$types$resourceTypeGeneral),
       type = a$types$resourceTypeGeneral,
       title = if (length(a$titles)) a$titles[[1]]$title else NA,
       start = as_year(a$publicationYear), doi = doi,
       url = paste0("https://doi.org/", doi), source = "datacite", declared_by = "source")
  })
  bind(rows)
}

default_fetch <- function(url, timeout) {
  h <- curl::new_handle(timeout = timeout,
                        useragent = paste0("academiccareer/", utils::packageVersion("academiccareer"),
                                           " (https://pablosancheznunez.com/academiccareer/)"))
  curl::handle_setheaders(h, Accept = "application/json")
  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) e)
  if (inherits(res, "error")) return(list(status = NA_integer_, body = NULL, error = conditionMessage(res)))
  list(status = res$status_code, body = rawToChar(res$content))
}

get_json <- function(url, timeout) {
  fetch <- getOption("academiccareer.fetch", default_fetch)
  res <- fetch(url, timeout)
  if (!identical(as.integer(res$status), 200L)) {
    stop(res$error %||% paste("HTTP", res$status), call. = FALSE)
  }
  jsonlite::fromJSON(res$body, simplifyVector = FALSE)
}

fetch_source <- function(source, orcid, timeout, mailto, api_key, max_works) {
  switch(source,
    orcid = {
      json <- get_json(paste0("https://pub.orcid.org/v3.0/", orcid, "/record"), timeout)
      name <- json$person$name
      attr_name <- trimws(paste(value_of(name[["given-names"]]), value_of(name[["family-name"]])))
      out <- ac_normalise_orcid(json)
      attr(out, "person") <- if (grepl("^NA|NA$", attr_name)) NULL else attr_name
      out
    },
    openalex = {
      extra <- paste0("", if (!is.null(mailto)) paste0("&mailto=", utils::URLencode(mailto, reserved = TRUE)),
                      if (nzchar(api_key)) paste0("&api_key=", api_key))
      out <- list()
      n <- 0L
      for (page in seq_len(ceiling(max_works / 200))) {
        json <- get_json(sprintf(paste0("https://api.openalex.org/works?filter=author.orcid:%s&per-page=200",
                                        "&page=%d&select=id,doi,title,type,publication_year,open_access,authorships%s"),
                                 orcid, page, extra), timeout)
        out[[page]] <- ac_normalise_openalex(json, orcid)
        n <- n + length(json$results)
        if (!length(json$results) || n >= (json$meta$count %||% 0)) break
      }
      res <- do.call(rbind, out)
      attr(res, "total") <- json$meta$count %||% n
      res
    },
    datacite = ac_normalise_datacite(get_json(
      paste0("https://api.datacite.org/dois?user-id=", orcid, "&page[size]=", min(max_works, 1000)), timeout))
  )
}

# The same output found in several sources becomes one event: ORCID first,
# completed with the open access status, countries, sectors and author
# position from OpenAlex.
merge_sources <- function(events) {
  if (!nrow(events)) return(events)
  has_doi <- !is.na(events$doi)
  keyed <- events[has_doi, , drop = FALSE]
  rest <- events[!has_doi, , drop = FALSE]
  rank <- match(keyed$source, c("orcid", "datacite", "openalex"))
  keyed <- keyed[order(keyed$doi, rank), , drop = FALSE]
  merged <- lapply(split(keyed, keyed$doi), function(g) {
    first <- g[1, , drop = FALSE]
    for (col in c("country", "sectors", "open_access", "author_position", "start", "title")) {
      if (is.na(first[[col]])) {
        v <- g[[col]][!is.na(g[[col]])]
        if (length(v)) first[[col]] <- v[1]
      }
    }
    if (first$dimension == "research_outputs" && any(g$dimension != "research_outputs")) {
      first$dimension <- g$dimension[g$dimension != "research_outputs"][1]
    }
    first$source <- paste(unique(g$source), collapse = "; ")
    first
  })
  out <- rbind(rest, do.call(rbind, merged))
  out$event_id <- NA
  rownames(out) <- NULL
  normalise_events(out)
}

#' Collect the evidence of a career from open sources
#'
#' Queries the public APIs of ORCID, OpenAlex and DataCite for one ORCID
#' iD. Outputs found in several sources are merged by DOI. A source that
#' fails is reported in `$sources` and never stops the collection.
#'
#' Use it for your own record, or with the consent of the person: public
#' records are still personal data. The function takes one ORCID iD at a
#' time by design, and the package has no function to compare or rank
#' people.
#'
#' OpenAlex matches works to people automatically and makes mistakes,
#' especially for common names and for ORCID iDs that other people have
#' used by error. Check the evidence before using it.
#'
#' @param orcid An ORCID iD.
#' @param sources Any of `"orcid"`, `"openalex"` and `"datacite"`.
#' @param mailto Optional e-mail address sent to OpenAlex, as it asks.
#'   Defaults to the option `academiccareer.mailto`.
#' @param api_key Optional OpenAlex API key, if you have one. Defaults to
#'   the option `academiccareer.openalex_key` or the environment variable
#'   `OPENALEX_API_KEY`.
#' @param max_works Maximum number of works read from OpenAlex and DataCite.
#' @param timeout Seconds to wait for each request.
#'
#' @return An object of class `ac_career`.
#' @export
#'
#' @examples
#' \donttest{
#' # Josiah Carberry is a fictitious researcher that ORCID keeps for testing.
#' career <- ac_collect("0000-0002-1825-0097", sources = "orcid")
#' career
#' }
ac_collect <- function(orcid, sources = ac_sources, mailto = getOption("academiccareer.mailto"),
                       api_key = getOption("academiccareer.openalex_key", Sys.getenv("OPENALEX_API_KEY")),
                       max_works = 1000, timeout = 30) {
  orcid <- ac_orcid(orcid)
  sources <- match.arg(sources, ac_sources, several.ok = TRUE)
  all <- list()
  name <- NULL
  status <- lapply(sources, function(s) {
    res <- tryCatch(fetch_source(s, orcid, timeout, mailto, api_key %||% "", max_works),
                    error = function(e) e)
    ok <- !inherits(res, "error")
    if (ok) {
      all[[s]] <<- res
      if (!is.null(attr(res, "person", exact = TRUE))) name <<- attr(res, "person", exact = TRUE)
    }
    total <- if (ok) attr(res, "total", exact = TRUE) %||% nrow(res) else 0L
    data.frame(source = s, status = if (ok) "ok" else "unavailable",
               n = if (ok) nrow(res) else 0L, retrieved_at = utc_now(),
               detail = if (!ok) conditionMessage(res) else if (total > nrow(res))
                 sprintf("first %d of %d works", nrow(res), total) else NA_character_,
               stringsAsFactors = FALSE)
  })
  status <- do.call(rbind, status)
  if (any(status$status != "ok")) {
    message("Could not reach: ", paste(status$source[status$status != "ok"], collapse = ", "), ".")
  }
  events <- if (length(all)) merge_sources(do.call(rbind, unname(all))) else empty_events()
  career <- ac_career(events, name = name, orcid = orcid)
  career$sources <- status
  career
}
