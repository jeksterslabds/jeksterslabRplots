#' ---
#' title: "Tests: .scatter.plot"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: .scatter.plot}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ include = FALSE
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
#'
#'
#'
#+ echo = FALSE
library(testthat)
library(jeksterslabRplots)
#'
#+ eval = FALSE
.scatter.plot(data = jeksterslabRdatarepo::wages)
#'
#+ testthat
expect_error(
  object = .scatter.plot(data = jeksterslabRdatarepo::wages),
  regexp = NA
)
