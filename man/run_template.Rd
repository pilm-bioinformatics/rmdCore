% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{run_template}
\alias{run_template}
\title{Main function to render templates from github or local folders}
\usage{
run_template(repo, local = ".", output_file = NULL, options = list(),
  params_file = NULL, ...)
}
\arguments{
\item{repo}{character with the owner and repo names like: foo/bar}

\item{local}{character with the folder where you want to clone the repo}

\item{output_file}{character with the final HTML}

\item{options}{list with options matching the params in the main.R file from the repository}

\item{params_file}{optional YAML file with the options matching the params in the main.R file from the repository}

\item{...}{any option for \code{\link[rmarkdown:render]{rmarkdown::render()}}}
}
\description{
Main function to render templates from github or local folders
}
