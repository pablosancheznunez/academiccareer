# Builds the example reports published on the website, from a fictitious
# career (synthetic data).
library(academiccareer)

out <- "_site/example"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
career <- ac_example("software")
ac_report(career, file.path(out, "report.html"))
ac_report(career, file.path(out, "report-es.html"), lang = "es")
