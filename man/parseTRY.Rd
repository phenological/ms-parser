% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseTRY.r
\name{parseTRY}
\alias{parseTRY}
\title{extract targeted MS data from task export xml file}
\usage{
parseTRY(path, options = list())
}
\arguments{
\item{path}{\itemize{
\item the path to the expName folder
}}

\item{options}{list of options
\itemize{
\item codePosition - position of the code in the file name
\item columnList - list of columns to be selected
}}
}
\value{
a dataElement
}
\description{
extract targeted MS data from task export xml file
}
