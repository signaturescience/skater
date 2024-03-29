% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ibd.R
\name{ibd2kin}
\alias{ibd2kin}
\title{Compute kinship coefficient from IBD segments}
\usage{
ibd2kin(.ibd_data, .map, type = NULL)
}
\arguments{
\item{.ibd_data}{Tibble with IBD segments created using the \link[skater]{read_ibd} function}

\item{.map}{Tibble with the genetic map data created using the \link[skater]{read_map} function}

\item{type}{Type of IBD to use for kinship coefficient calculation; must be \code{'IBD1'}, \code{'IBD2'}, or \code{NULL} (both IBD1 and IBD2 will be treated the same); default is \code{NULL}}
}
\value{
Tibble with three columns:
\enumerate{
\item id1 (sample identifier 1)
\item id2 (sample identifier 2)
\item kinship (kinship coefficent derived from shared segments)
}
}
\description{
This function is used to retrieve a relatedness measure from IBD segments.
The relatedness value returned is the kinship coefficient.
}
\details{
The input data should be pairwise IBD segments prepared via
\link[skater]{read_ibd}. The function will internally loop over each
chromosome, and use a specified genetic map to convert shared segments to
genetic units. After doing so, the function converts the shared length to a
kinship coefficient by summing \eqn{0.5*IBD2 + 0.25*IBD1}.

Note that the data read in by \link{read_ibd} when \code{source="pedsim"} returns a
list with separate tibbles for IBD1 and IBD2 segments. The current
implementation of this function requires running this function independently
on IBD1 and IBD2 segments, then summarizing (adding) the corresponding
proportions. See examples.
}
\examples{
pedsim_fp <- system.file("extdata", "GBR.sim.seg.gz", package="skater", mustWork=TRUE)
pedsim_seg <- read_ibd(pedsim_fp, source = "pedsim")
gmapfile <- system.file("extdata", "sexspec-avg-min.plink.map", package="skater", mustWork=TRUE)
gmap <- read_map(gmapfile)
ibd1_dat <- ibd2kin(.ibd_data=pedsim_seg$IBD1, .map=gmap, type="IBD1")
ibd2_dat <- ibd2kin(.ibd_data=pedsim_seg$IBD2, .map=gmap, type="IBD2")
dplyr::bind_rows(ibd1_dat,ibd2_dat) \%>\%
  dplyr::group_by(id1,id2) \%>\%
  dplyr::summarise(kinship = sum(kinship), .groups = "drop")

}
\references{
http://faculty.washington.edu/sguy/ibd_relatedness.html
}
