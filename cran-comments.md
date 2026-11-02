## Submission

First submission of academiccareer.

## Test environments

* GitHub Actions: Ubuntu (R devel, release, oldrel-1), macOS (release), Windows (release).
* Local: Windows 11 x64, R 4.6.2 ucrt.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for CRAN

* Network access is used only by `ac_collect()`, which queries the public APIs of ORCID, OpenAlex and DataCite. Its example is wrapped in `\donttest{}` and uses the fictitious record that ORCID provides for testing (0000-0002-1825-0097). A source that fails is reported and never stops the function.
* The tests never use the network: they run against synthetic recorded responses in `inst/extdata/fixtures`.
* Examples and tests write only to `tempdir()`; functions that write files have no default destination.
