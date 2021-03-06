% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{cancer_statistics}
\alias{cancer_statistics}
\title{Download cancer incidence or mortality data.}
\usage{
cancer_statistics(statistic = "incidence", by = "county", state = NULL,
  cancer = "all", race = "all", sex = "all", age = "all")
}
\arguments{
\item{statistic}{"incidence" or "mortality"}

\item{by}{"county" or "state"}

\item{state}{restrict data download to one state; specify using state abbreviation (e.g. "MA"). Leave as NULL to retrieve data for all states.}

\item{cancer}{download data for a specific cancer type, see details for available options}

\item{race}{download data for a specific race}

\item{sex}{download data for a specific gender}

\item{age}{download data for a specific age group}
}
\description{
Download cancer incidence or mortality data.
}
\details{
Cancers:
\enumerate{
  \item all
  \item bladder
  \item brain & ONS
  \item breast (female)
  \item breast (female in situ)
  \item cervix
  \item colon & rectum
  \item esophagus
  \item kidney & renal pelvis
  \item leukemia
  \item liver & bile duct
  \item lung & bronchus
  \item non-hodgkin lymphoma
  \item melanoma of the skin
  \item oral cavity & pharynx
  \item ovary
  \item pancreas
  \item prostate
  \item stomach
  \item thyroid
  \item uterus"
}
}
\examples{
\dontrun{
# Download county level bladder cancer incidence data
dat <- cancer_statistics(statistic = "incidence", by = "county", cancer = "bladder")
}

}

