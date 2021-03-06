% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prediction.R
\name{get_prediction_variables}
\alias{get_prediction_variables}
\title{Select all variables useful for prediction and compute derived variables}
\usage{
get_prediction_variables(x, var.removed = c("X", "Y", "XM", "YM", "BX",
  "XMg5", "YMg5", "Width", "Height", "Angle", "XStart", "YStart", "Compentropy",
  "Compmean", "Compslope", "CompM1", "CompM2", "CompM2"))
}
\arguments{
\item{x}{data.frame from which to select/compute the variables, usually read with read_pid}

\item{var.removed}{names of variables to remove}
}
\description{
Select all variables useful for prediction and compute derived variables
}

