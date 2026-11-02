#' Example careers
#'
#' Four fictitious careers (synthetic data) that show different profiles:
#' `"traditional"` (centred on articles), `"software"` (a research software
#' engineer, with a career break), `"transfer"` (patents and industry
#' collaboration) and `"teaching"` (teaching, supervision and outreach,
#' with a career break). Names, organisations, titles and DOIs are invented.
#'
#' @param which Which career.
#'
#' @return An object of class `ac_career`.
#' @export
#'
#' @examples
#' ac_example("teaching")
ac_example <- function(which = c("traditional", "software", "transfer", "teaching")) {
  which <- match.arg(which)
  all <- read_extdata_csv("careers.csv")
  ev <- all[all$career == which, setdiff(names(all), "career"), drop = FALSE]
  ev[ev == ""] <- NA
  names <- c(traditional = "Researcher A", software = "Researcher B",
             transfer = "Researcher C", teaching = "Researcher D")
  career <- ac_career(ev, name = paste(names[[which]], "(synthetic)"))
  if (which == "software") {
    career <- ac_context(career, breaks = data.frame(start = 2018, end = 2018, reason = "Parental leave"),
                         field = "Computational biology")
  }
  if (which == "teaching") {
    career <- ac_context(career, breaks = data.frame(start = 2016, end = 2017, reason = "Caring responsibilities"),
                         field = "Education")
  }
  career
}
