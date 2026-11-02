# academiccareer 0.1.0

* First release.
* Evidence from the public APIs of ORCID, OpenAlex and DataCite, merged by DOI, with the status of each source: `ac_collect()`, `ac_orcid()`, `ac_normalise_orcid()`, `ac_normalise_openalex()`, `ac_normalise_datacite()`.
* Evidence declared by the researcher and career context, including breaks: `ac_add_evidence()`, `ac_read_evidence()`, `ac_context()`, `ac_career()`.
* Timeline, contributions by period, rule-based transitions and a descriptive profile in English and Spanish: `ac_timeline()`, `ac_contributions()`, `ac_transitions()`, `ac_profile()`.
* Self-contained HTML report and the coverage of each source: `ac_report()`, `ac_dimensions()`.
* Four fictitious example careers: `ac_example()`.
