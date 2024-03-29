% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_nsofa_child_encounter.R
\name{get_nsofa_child_encounter}
\alias{get_nsofa_child_encounter}
\title{Creates q1hr timepoints within an encounter}
\usage{
get_nsofa_child_encounter(read_child_encounter)
}
\arguments{
\item{read_child_encounter}{the raw child_encounter csv file obtained from the IDR}
}
\value{
A dataframe containing q1hr timepoints
}
\description{
This function ingests the raw child encounter data and performs the following:
\itemize{
\item choose first encounter when a subject has multiple encounters
\item removes rows where both the admit_datetime and dischg_datetime is NA
\item Creates the q1hr time buckets
}
}
