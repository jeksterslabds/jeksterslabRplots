#' ---
#' title: "Tests: .hist.plot"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: .hist.plot}
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
.hist.plot(
  x = rnorm(n = 100, mean = 100, sd = 15)
)
#'
#' ## With Normal Curve, Kernel Density Plot, and Legend
#' 
#+
expect_error(
  object = .hist.plot(
    x = rnorm(n = 100, mean = 100, sd = 15)
  ),
  regexp = NA
)
expect_error(
  object = .hist.plot(
    x = rexp(n = 100)
  ),
  regexp = NA
)
#'
#' ## With Normal Curve, and Kernel Density Plot
#' 
#+
expect_error(
  object = .hist.plot(
    x = rnorm(n = 100, mean = 100, sd = 15),
    legend = FALSE
  ),
  regexp = NA
)
expect_error(
  object = .hist.plot(
    x = rexp(n = 100),
    legend = FALSE
  ),
  regexp = NA
)
#'
#' ## With Normal Curve, and Legend
#' 
#+
expect_error(
  object = .hist.plot(
    x = rnorm(n = 100, mean = 100, sd = 15),
    kernel = FALSE
  ),
  regexp = NA
)
expect_error(
  object = .hist.plot(
    x = rexp(n = 100),
    kernel = FALSE
  ),
  regexp = NA
)
#'
#' ## With Kernel Density Plot, and Legend
#' 
#+
expect_error(
  object = .hist.plot(
    x = rnorm(n = 100, mean = 100, sd = 15),
    normalcurve = FALSE
  ),
  regexp = NA
)
expect_error(
  object = .hist.plot(
    x = rexp(n = 100),
    normalcurve = FALSE
  ),
  regexp = NA
)
#'
#' ## plot = FALSE
#'
#+
expect_error(
  object = .hist.plot(
    x = rexp(n = 100),
    plot = FALSE
  ),
  regexp = NA
)
#'
#' ## freq = TRUE
#'
#+
expect_error(
  object = .hist.plot(
    x = rexp(n = 100),
    freq = TRUE
  ),
  regexp = NA
)
