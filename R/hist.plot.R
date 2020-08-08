#' @title Histogram
#'
#' @description Adapted
#'   from <https://stackoverflow.com/questions/20078107/overlay-normal-curve-to-histogram-in-r> .
#'
#' @inheritParams graphics::hist
#' @inheritParams graphics::plot
#' @importFrom stats sd dnorm
#' @param normalcurve Logical.
#'   Add normal curve.
#' @param kernel Logical.
#'   Add kernel density plot.
#'   This only works if `freq = FALSE`.
#' @param legend Logical.
#'   Add legend.
#' @examples
#' x <- rnorm(n = 1000, mean = 100, sd = 15)
#' .hist.plot(x = x, freq = FALSE)
#' @export
.hist.plot <- function(x,
                       breaks = "Sturges",
                       freq = FALSE,
                       include.lowest = TRUE,
                       normalcurve = TRUE,
                       kernel = TRUE,
                       legend = TRUE,
                       right = TRUE,
                       density = NULL,
                       angle = 45,
                       col = NULL,
                       border = NULL,
                       main = paste("Histogram of", xname),
                       ylim = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       axes = TRUE,
                       plot = TRUE,
                       labels = FALSE,
                       warn.unused = TRUE,
                       ...) {
  xname <- paste(deparse(substitute(x), 500), collapse = "\n")
  if (is.null(ylab)) {
    if (freq) {
      ylab <- "Frequency"
    } else {
      ylab <- "Density"
    }
  }
  if (is.null(xlab)) {
    xlab <- xname
  }
  suppressWarnings(
    h <- graphics::hist.default(
      x = x,
      breaks = breaks,
      freq = freq,
      include.lowest = include.lowest,
      right = right,
      density = density,
      angle = angle,
      col = col,
      border = border,
      main = main,
      ylim = ylim,
      xlab = xlab,
      ylab = ylab,
      axes = axes,
      plot = plot,
      labels = labels,
      warn.unused = warn.unused,
      ...
    )
  )
  if (isTRUE(normalcurve) & isTRUE(plot)) {
    x <- x[!is.na(x)]
    xfit <- seq(min(x), max(x), length = 1000)
    yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
    if (isTRUE(freq) | (is.null(freq) & is.null(density))) {
      yfit <- yfit * diff(h$mids[1:2]) * length(x)
    }
    lines(x = xfit, y = yfit, col = "blue", lty = 2)
  }
  if (isTRUE(kernel) & isTRUE(plot) & isFALSE(freq)) {
    lines(x = density(x), col = "red", lty = 1)
  }
  if (legend) {
    if (isTRUE(normalcurve) & isTRUE(plot) & isTRUE(kernel) & isFALSE(freq)) {
      legend("topright", c("Normal Curve", "Kernel Density"), col = c("blue", "red"), lty = 2:1)
    }
    if (isTRUE(normalcurve) & isTRUE(plot) & isFALSE(kernel)) {
      legend("topright", "Normal Curve", col = "blue", lty = 2)
    }
    if (isFALSE(normalcurve) & isTRUE(plot) & isTRUE(kernel) & isFALSE(freq)) {
      legend("topright", "Kernel Density", col = "red", lty = 1)
    }
  }
  if (plot == TRUE) {
    invisible(h)
  } else {
    h
  }
}
