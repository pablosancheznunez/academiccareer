# academiccareer <img src="man/figures/logo.png" align="right" height="139" alt="academiccareer logo" />

<!-- badges: start -->
[![CI](https://github.com/pablosancheznunez/academiccareer/actions/workflows/ci.yml/badge.svg)](https://github.com/pablosancheznunez/academiccareer/actions/workflows/ci.yml)
[![Website](https://img.shields.io/badge/docs-pkgdown-4b3f72)](https://pablosancheznunez.com/academiccareer/)
<!-- badges: end -->

**Descriptive, evidence-based profiles of academic careers.**

Responsible research assessment, as promoted by DORA and the Coalition for Advancing Research Assessment (CoARA), asks evaluators to look beyond publications and citations. academiccareer describes how a career develops over time across all its dimensions, from open evidence and from what the researcher declares:

- **evidence from open sources**: the public records of ORCID, OpenAlex and DataCite, merged by DOI, with the status and coverage of each source stated;
- **evidence declared by the researcher** for what open sources rarely record: supervision, teaching, mentoring, policy advice, outreach, transfer contracts;
- **context**: career breaks are shown as breaks, not as gaps;
- **a timeline**, **contributions by period** (software and data, open access, international and cross-sector collaboration, funding, service, teaching) and **transitions** found with transparent rules;
- **a descriptive profile** that makes visible careers built on software, data, knowledge transfer, teaching or open science, in English and Spanish, and a self-contained HTML report.

It describes; it does not rank. There is no score, no function to compare people and no journal-based metric. Public records are personal data: use the package for your own record or with the person's consent.

**Website:** <https://pablosancheznunez.com/academiccareer/> · **Example report:** <https://pablosancheznunez.com/academiccareer/example/report.html>

## Installation

```r
# install.packages("remotes")
remotes::install_github("pablosancheznunez/academiccareer")
```

## Example

```r
library(academiccareer)

career <- ac_collect("0000-0002-1825-0097", mailto = "you@university.edu")

career <- ac_add_evidence(career, "supervision_mentoring", "PhD thesis co-supervised",
                          start = 2021, end = 2025)
career <- ac_context(career, breaks = data.frame(start = 2018, end = 2018,
                                                 reason = "Parental leave"))

plot(ac_timeline(career))
ac_contributions(career)
ac_transitions(career)
ac_profile(career)
ac_report(career, "career-report.html")
```

The package includes four fictitious careers (synthetic data) with different profiles: `ac_example("traditional")`, `"software"`, `"transfer"` and `"teaching"`.

```r
ac_profile(ac_example("software"))
#>   Evidence from 2008 to 2025.
#>   Software and data make up 50% of the outputs (22 of 44).
#>   84% of the outputs with known status are open access (37 of 44).
#>   Teaching, supervision or mentoring activities recorded: 1.
#>   No evidence was found for: supervision and mentoring, knowledge transfer,
#>   science policy, public engagement. Open sources rarely record these; not
#>   found does not mean it did not happen.
```

## Main functions

| Step | Functions |
|---|---|
| Collect | `ac_collect()`, `ac_orcid()`, `ac_career()`, `ac_example()` |
| Complete | `ac_add_evidence()`, `ac_read_evidence()`, `ac_context()` |
| Describe | `ac_timeline()`, `ac_contributions()`, `ac_transitions()`, `ac_profile()` |
| Report | `ac_report()`, `ac_dimensions()` |

## Related tools

[Narrative CV Crosswalk](https://pablosancheznunez.com/narrative-cv-crosswalk/) maps the same open evidence to the sections of narrative CVs at one point in time; [OpenResearchMetrics](https://pablosancheznunez.com/openresearchmetrics/) reports open science indicators one by one. academiccareer adds the time dimension.

## Citation

If you use academiccareer in your research, please cite it: `citation("academiccareer")` gives the reference, and the repository's [CITATION.cff](https://github.com/pablosancheznunez/academiccareer/blob/main/CITATION.cff) is read by GitHub's "Cite this repository" button.

## Contributing

Bug reports and suggestions are welcome in the [issues](https://github.com/pablosancheznunez/academiccareer/issues). See the [contributing guide](https://github.com/pablosancheznunez/academiccareer/blob/main/CONTRIBUTING.md).

## License

MIT © Pablo Sánchez-Núñez
