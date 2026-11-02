test_that("careers can be built from tables and validate dimensions", {
  ev <- data.frame(dimension = c("employment", "software"), title = c("Post", "pkg"), start = c(2018, 2020))
  k <- ac_career(ev, name = "X")
  expect_equal(nrow(k$events), 2)
  expect_error(ac_career(data.frame(dimension = "papers", title = "x", start = 2020)), "Unknown dimension")
  expect_error(ac_career(data.frame(title = "x")), "columns")
})

test_that("declared evidence is marked as declared", {
  k <- ac_add_evidence(ac_example("traditional"), "supervision_mentoring", "PhD thesis", 2019, 2023)
  last <- k$events[k$events$title == "PhD thesis", ]
  expect_equal(last$declared_by, "self")
  f <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(dimension = c("teaching", "science_policy"), title = c("Course", "Advice"),
                              start = c(2020, 2022)), f, row.names = FALSE)
  k2 <- ac_read_evidence(k, f)
  expect_equal(sum(k2$events$declared_by == "self"), 3)
  expect_false(anyDuplicated(k2$events$event_id) > 0)
})

test_that("the timeline counts periods in every active year and marks breaks", {
  k <- ac_career(data.frame(dimension = c("employment", "research_outputs"), title = c("Post", "Paper"),
                            start = c(2010, 2012), end = c(2013, NA)))
  k <- ac_context(k, breaks = data.frame(start = 2011, end = 2011, reason = "Leave"))
  tl <- ac_timeline(k)
  emp <- tl[tl$dimension == "employment", ]
  expect_equal(emp$n[emp$year %in% 2010:2013], c(1, 1, 1, 1))
  expect_true(all(tl$break_year[tl$year == 2011]))
  expect_output(print(tl), "career break")
  f <- tempfile(fileext = ".png")
  grDevices::png(f)
  expect_silent(plot(tl))
  grDevices::dev.off()
})

test_that("contributions are computed over known values only", {
  ct <- ac_contributions(ac_example("software"))
  expect_s3_class(ct, "ac_contributions")
  expect_true(all(ct$software_data_pct >= 0 & ct$software_data_pct <= 100, na.rm = TRUE))
  expect_equal(sum(ct$outputs), sum(ac_example("software")$events$dimension %in%
                                       c("research_outputs", "software", "data")))
})

test_that("each example career shows its distinctive contribution", {
  hl <- function(w) ac_profile(ac_example(w))$highlights$contribution
  expect_true("software_data" %in% hl("software"))
  expect_true("open_science" %in% hl("software"))
  expect_true("transfer" %in% hl("transfer"))
  expect_true("teaching_mentoring" %in% hl("teaching"))
  expect_false("software_data" %in% hl("traditional"))
})

test_that("profiles never use comparative language", {
  for (w in c("traditional", "software", "transfer", "teaching")) {
    for (lang in c("en", "es")) {
      txt <- paste(ac_profile(ac_example(w), lang = lang)$statements, collapse = " ")
      expect_false(grepl("\\b(better|best|more productive|top|rank|mejor|ranking)\\b", txt,
                         ignore.case = TRUE, perl = TRUE), info = paste(w, lang))
    }
  }
})

test_that("low-coverage dimensions without evidence are reported", {
  p <- ac_profile(ac_example("traditional"))
  expect_true("supervision_mentoring" %in% p$no_evidence)
  expect_match(paste(p$statements, collapse = " "), "not found does not mean")
})

test_that("transitions follow their rules", {
  tr <- ac_transitions(ac_example("software"))
  expect_true(all(c("new_organisation", "output_mix", "career_break", "new_dimension") %in% tr$rule))
  tr2 <- ac_transitions(ac_example("software"), shift = 101)
  expect_false("output_mix" %in% tr2$rule)
})

test_that("the report is self-contained and in both languages", {
  f <- tempfile(fileext = ".html")
  ac_report(ac_example("teaching"), f)
  html <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "<!doctype html>", fixed = TRUE)
  expect_match(html, "declared by the researcher", fixed = TRUE)
  expect_match(html, 'class="l', fixed = TRUE)
  expect_false(grepl("<script|<link ", html))
  f_es <- tempfile(fileext = ".html")
  ac_report(ac_example("teaching"), f_es, lang = "es")
  expect_match(paste(readLines(f_es, encoding = "UTF-8"), collapse = ""), "Perfil de trayectoria")
})

test_that("the dimension vocabulary is complete", {
  d <- ac_dimensions()
  expect_false(anyNA(d$en) || anyNA(d$es))
  expect_true(all(d$coverage %in% c("good", "partial", "low")))
})
