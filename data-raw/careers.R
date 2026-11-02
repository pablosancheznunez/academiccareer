# Builds inst/extdata/careers.csv: four fictitious careers (synthetic data)
# used in examples, tests and the website. Run from the package root:
#   Rscript data-raw/careers.R
set.seed(20261102)

rows <- list()
add <- function(career, dimension, title, start, end = NA, type = NA, role = NA,
                organisation = NA, country = NA, sectors = NA, open_access = NA,
                author_position = NA, doi = NA, source = "orcid", declared_by = "source") {
  rows[[length(rows) + 1L]] <<- data.frame(
    career = career, dimension = dimension, title = title, start = start, end = end,
    type = type, role = role, organisation = organisation, country = country,
    sectors = sectors, open_access = open_access, author_position = author_position,
    doi = doi, url = if (is.na(doi)) NA else paste0("https://doi.org/", doi),
    source = source, declared_by = declared_by, stringsAsFactors = FALSE)
}
doi_n <- 0
new_doi <- function() { doi_n <<- doi_n + 1; sprintf("10.5555/synthetic.%04d", doi_n) }
outputs <- function(career, years, per_year, dimension = "research_outputs", type = "journal-article",
                    oa = 0.5, intl = 0.3, cross = 0.05, home = "ES", label = "Article") {
  for (y in years) for (k in seq_len(per_year)) {
    countries <- if (stats::runif(1) < intl) paste(sort(c(home, sample(c("DE", "GB", "PT", "US", "FR"), 1))), collapse = "; ") else home
    sectors <- if (stats::runif(1) < cross) "company; education" else "education"
    add(career, dimension, sprintf("%s %d.%d (synthetic)", label, y, k), y, type = type,
        country = countries, sectors = sectors, open_access = stats::runif(1) < oa,
        author_position = sample(c("first", "middle", "last"), 1, prob = c(.35, .4, .25)),
        doi = new_doi(), source = "orcid; openalex")
  }
}

# A: a career centred on articles
a <- "traditional"
add(a, "education", "PhD in Sociology", 2006, 2010, type = "education", organisation = "University A", country = "ES")
add(a, "employment", "Postdoctoral researcher", 2011, 2013, type = "employment", organisation = "University B", country = "GB")
add(a, "employment", "Associate professor", 2014, NA, type = "employment", organisation = "University A", country = "ES")
outputs(a, 2009:2025, 3, oa = 0.45, intl = 0.3)
add(a, "funding", "National research project", 2016, 2019, type = "grant", organisation = "National agency", country = "ES")
add(a, "funding", "European network grant", 2021, 2024, type = "grant", organisation = "European funder")
for (y in c(2015, 2017, 2018, 2020, 2022, 2023, 2024)) add(a, "service", "Peer review", y, type = "peer-review", role = "reviewer")

# B: a research software career
b <- "software"
add(b, "education", "PhD in Computational Biology", 2008, 2012, type = "education", organisation = "University C", country = "ES")
add(b, "employment", "Postdoctoral researcher", 2012, 2015, type = "employment", organisation = "Institute D", country = "DE")
add(b, "employment", "Research software engineer", 2016, NA, type = "employment", organisation = "University C", country = "ES")
outputs(b, 2010:2015, 2, oa = 0.6, intl = 0.5, home = "DE")
outputs(b, 2016:2025, 1, oa = 0.95, intl = 0.6)
outputs(b, c(2016, 2017, 2019:2025), 2, dimension = "software", type = "software", oa = 1, intl = 0.2,
        label = "Software release")
outputs(b, c(2017, 2020, 2022, 2024), 1, dimension = "data", type = "data-set", oa = 1, intl = 0.4, label = "Dataset")
add(b, "teaching", "Workshops on reproducible research software", 2019, 2025, declared_by = "self", source = "self")

# C: a career with knowledge transfer
cc <- "transfer"
add(cc, "education", "PhD in Materials Engineering", 2007, 2011, type = "education", organisation = "University E", country = "ES")
add(cc, "employment", "R&D engineer", 2011, 2014, type = "employment", organisation = "Company F", country = "ES")
add(cc, "employment", "Lecturer", 2015, NA, type = "employment", organisation = "University E", country = "ES")
outputs(cc, 2010:2025, 2, oa = 0.4, intl = 0.25, cross = 0.55)
add(cc, "knowledge_transfer", "Patent: coating process (synthetic)", 2016, type = "patent")
add(cc, "knowledge_transfer", "Patent: recycling method (synthetic)", 2021, type = "patent")
add(cc, "knowledge_transfer", "Research contract with industry", 2018, 2020, declared_by = "self", source = "self")
add(cc, "funding", "Industrial doctorate programme", 2019, 2023, type = "contract", organisation = "Regional agency")

# D: a career centred on teaching, with a break
d <- "teaching"
add(d, "education", "PhD in Education", 2005, 2009, type = "education", organisation = "University G", country = "ES")
add(d, "employment", "Lecturer", 2009, NA, type = "employment", organisation = "University G", country = "ES")
outputs(d, c(2008:2015, 2018:2025), 1, oa = 0.7, intl = 0.1)
add(d, "teaching", "Course: Research methods (synthetic)", 2010, 2025, declared_by = "self", source = "self")
add(d, "teaching", "Open textbook on statistics (synthetic)", 2020, type = "learning-object", doi = new_doi())
for (y in c(2012, 2014, 2019, 2021, 2024)) add(d, "supervision_mentoring", sprintf("Master's thesis supervised (%d)", y), y,
                                              declared_by = "self", source = "self")
add(d, "supervision_mentoring", "PhD thesis co-supervised", 2019, 2023, declared_by = "self", source = "self")
add(d, "public_engagement", "Public talks on data literacy", 2021, 2024, declared_by = "self", source = "self")

out <- do.call(rbind, rows)
utils::write.csv(out, "inst/extdata/careers.csv", row.names = FALSE, na = "")
