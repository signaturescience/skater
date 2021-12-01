## Test environments

- local OS X install, R 4.0.4
- R hub

## R CMD check results

Status: OK

## Revisions after initial CRAN inspection

- Added more detailed description about package functionality in DESCRIPTION.
- Defined acronyms in DESCRIPTION.
- Added Signature Science, LLC as `cph` in DESCRIPTION Authors.
- Added reference to Description field of DESCRIPTION in the form: `authors (year) <doi:...>` with reference to preprint describing methods.
- Better explanation of identical by descent (IBD) segment to kinship coefficient math in function documentation.
- Stopped exporting two internal functions, but clarified documentation and retained examples.
- Added a return value for `plot_pedigree()` (called for side effects).
- Updated exported functions to ensure `@return` `\value` notes class of the output value and what it means.
