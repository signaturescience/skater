#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

# Suppress R CMD check note
#' @importFrom lifecycle badge

# Quiet CRAN check notes for unbound global variables
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                        "fid",
                                                        "dadid",
                                                        "momid",
                                                        "dadid",
                                                        "dadid",
                                                        "x",
                                                        "y",
                                                        "r",
                                                        "data"))
