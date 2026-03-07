# Changelog

## skater 0.2.0

- New maintainer ([@vpnagraj](https://github.com/vpnagraj))
- Updated
  [`read_ibis()`](https://signaturescience.github.io/skater/reference/read_ibis.md)
  and
  [`read_akt()`](https://signaturescience.github.io/skater/reference/read_akt.md)
  to use `read_table()` instead of `read_table2()` (which is now
  deprecated; thanks for the heads up
  [@jennybc](https://github.com/jennybc)).

## skater 0.1.2

CRAN release: 2023-01-31

- Updated to handle future changes in dplyr where `...` will be
  deprecated in `na_if()`, and fixes use of
  [`class()`](https://rdrr.io/r/base/class.html) that caused an issue
  with `R CMD check` (thanks [@hadley](https://github.com/hadley),
  [\#57](https://github.com/signaturescience/skater/issues/57))

## skater 0.1.1

CRAN release: 2022-02-01

- Updated vignette to handle future changes in tidyr (thanks
  [@DavisVaughan](https://github.com/DavisVaughan),
  [\#56](https://github.com/signaturescience/skater/issues/56))
- Updated DESCRIPTION with `URL` and `BugReports` links.

## skater 0.1.0

CRAN release: 2021-12-03

- Initial release
