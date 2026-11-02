test_that("ORCID iDs are validated with their check digit", {
  expect_equal(ac_orcid("https://orcid.org/0000-0002-1825-0097"), "0000-0002-1825-0097")
  expect_equal(ac_orcid("0000-0002-9079-593X"), "0000-0002-9079-593X")
  expect_error(ac_orcid("0000-0002-1825-0098"), "Not a valid")
  expect_error(ac_orcid("1234"), "Not a valid")
})

test_that("ORCID records become events in the right dimensions", {
  ev <- ac_normalise_orcid(jsonlite::fromJSON(fixture("orcid"), simplifyVector = FALSE))
  dims <- table(ev$dimension)
  expect_equal(dims[["employment"]], 2)
  expect_equal(dims[["software"]], 1)
  expect_equal(dims[["teaching"]], 1)
  expect_equal(dims[["service"]], 3)
  expect_equal(dims[["distinctions"]], 1)
  expect_equal(ev$organisation[ev$title == "Lecturer"], "University South")
  expect_true(is.na(ev$end[ev$title == "Lecturer"]))
})

test_that("OpenAlex works keep countries, sectors and author position", {
  ev <- ac_normalise_openalex(jsonlite::fromJSON(fixture("openalex"), simplifyVector = FALSE),
                              orcid = "0000-0002-1825-0097")
  first <- ev[ev$doi %in% "10.5555/fx.0001", ]
  expect_equal(first$country, "DE; US")
  expect_match(first$sectors, "company")
  expect_equal(first$author_position, "first")
  expect_true(first$open_access)
})

test_that("ac_collect merges sources by DOI and records their status", {
  k <- with_fake(ac_collect("0000-0002-1825-0097"))
  expect_s3_class(k, "ac_career")
  expect_equal(k$name, "Ana Example")
  expect_equal(k$sources$status, c("ok", "ok", "ok"))
  merged <- k$events[k$events$doi %in% "10.5555/fx.0001", ]
  expect_equal(nrow(merged), 1)
  expect_equal(merged$source, "orcid; openalex")
  expect_equal(merged$country, "DE; US")
  data <- k$events[k$events$doi %in% "10.5555/fx.0003", ]
  expect_equal(nrow(data), 1)
  expect_equal(data$dimension, "data")
  expect_false(anyDuplicated(k$events$event_id) > 0)
})

test_that("a failing source never stops the collection", {
  expect_message(k <- with_fake(ac_collect("0000-0002-1825-0097"), fail = "openalex"), "openalex")
  expect_equal(k$sources$status, c("ok", "unavailable", "ok"))
  expect_gt(nrow(k$events), 5)
  expect_message(k2 <- with_fake(ac_collect("0000-0002-1825-0097"), fail = c("orcid", "openalex", "datacite")))
  expect_equal(nrow(k2$events), 0)
  expect_output(print(k2), "0 items")
})
