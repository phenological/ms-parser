% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readSCFA.R
\name{readSCFA}
\alias{readSCFA}
\title{readSCFA}
\usage{
readSCFA(file, optns = list())
}
\arguments{
\item{file}{character file path}

\item{optns}{list of options
\itemize{
\item codePosition - position of the code in the AnalysisName for sampleID.
Default is where cal is found in the AnalysisName.
\item platePosition - position of the code in the AnalysisName for the plateID.
The default will use the position prior to that used for sampleID (codePosition).
\item projectName - the name of the project. For example "covid19". The default
will take it from the first position in AnalysisName.
\item cohortName - the name of the cohort. For example "harvardC2". The default
will take it from the second position in AnalysisName
\item sampleMatrixType - sample matrix type, usually "PLA", "SER" or "URI".
The default will take it from the AnalysisName
}}
}
\value{
data frame in long format
}
\description{
readSCFA
}
