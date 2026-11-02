fixture <- function(name) {
  f <- system.file("extdata", "fixtures", paste0(name, ".json"), package = "academiccareer")
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# A fake web service that answers from the synthetic fixtures.
fake_fetch <- function(fail = character(0)) {
  function(url, timeout) {
    src <- if (grepl("orcid.org", url, fixed = TRUE)) "orcid" else
      if (grepl("openalex", url, fixed = TRUE)) "openalex" else "datacite"
    if (src %in% fail) return(list(status = 503L, body = ""))
    list(status = 200L, body = fixture(src))
  }
}

with_fake <- function(expr, fail = character(0)) {
  old <- options(academiccareer.fetch = fake_fetch(fail))
  on.exit(options(old))
  expr
}
