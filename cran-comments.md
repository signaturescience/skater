## Test environments

- Local MacOS install, R 4.0.4
- R hub
    - Fedora Linux, R-devel
    - Ubuntu Linux 20.04.1 LTS, R-release
    - Windows Server 2008 R2 SP1, R-devel

## R CMD check results

- Local `R CMD check`: Status OK, 0 errors, 0 warnings, 0 notes
- R hub: 
    - NOTE, New submission
    - NOTE possibly mis-spelled words in description: IBD, et, al, benchmarking, polymorphism. IBD is defined in the DESCRIPTION as "identical by descent"; benchmarking and polymorphism are spelled correctly; and "et al." is used in a reference before linking to the doi with `<doi:...>`.

## Revisions after initial CRAN inspection

- Added more detailed description about package functionality in DESCRIPTION.
- Defined acronyms in DESCRIPTION.
- Added Signature Science, LLC as `cph` in DESCRIPTION Authors.
- Added reference to Description field of DESCRIPTION in the form: `authors (year) <doi:...>` with reference to preprint describing methods.
- Better explanation of identical by descent (IBD) segment to kinship coefficient math in function documentation.
- Stopped exporting two internal functions, but clarified documentation and retained examples.
- Added a return value for `plot_pedigree()` (called for side effects).
- Updated exported functions to ensure `@return` `\value` notes class of the output value and what it means.
