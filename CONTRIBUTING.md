# Contributing

Thank you for helping improve academiccareer. Bug reports with an ORCID record that is read incorrectly (yours, or one you have permission to share), new evidence sources, translations and feedback from research assessment experts are welcome.

## Running the tests

- `R CMD build . && R CMD check --as-cran academiccareer_*.tar.gz`
- or `devtools::check()`.

The tests never use the network: the connectors run against synthetic responses in `inst/extdata/fixtures`.

## Small changes (welcome as pull requests)

- Documentation, examples and the website.
- Translations in `inst/extdata/i18n.csv`.
- Compatibility fixes, as long as every test still passes unchanged.

## Changes to discuss in an issue first

- The connectors in `R/connectors.R` and the dimensions in `inst/extdata/dimensions.csv`.
- The rules of `ac_profile()` and `ac_transitions()`.
- Anything that would compare, rank or score people: the package deliberately does not.

The design notes are in [`dev/DESIGN.md`](dev/DESIGN.md).

Tests are never weakened or deleted to make them pass.
