#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residual Plots
#'
#' @description Displays a plot with four panels:
#'   - residuals vs. fitted values
#'   - normal qq plot of residuals
#'   - scale location
#'   - residuals vs. leverage
#'
#' @details Based on the diagnostic plots in the `car` package.
#'
#' @family plotting functions
#' @keywords plot
#' @import graphics
#' @importFrom stats qqline qqnorm
#' @param yhat Numeric vector of length `n` or `n` by `1` numeric matrix.
#'   \eqn{n \times 1} vector of predicted values of \eqn{\mathbf{y}}
#'   \eqn{\left( \mathbf{\hat{y}} \right)}.
#' @param epsilonhat Numeric vector of length `n` or `n` by 1 numeric matrix.
#'   \eqn{n \times 1} vector of residuals \eqn{\left( \boldsymbol{\hat{\epsilon}} \right)}.
#' @param tepsilonhat Numeric vector of length `n` or `n` by 1 numeric matrix.
#'   \eqn{n \times 1} vector of studentized residuals.
#' @param h Numeric vector of length `n` or `n` by 1 numeric matrix.
#'   \eqn{n \times 1} vector of leverage values.
#' @examples
#' model <- lm(
#'   wages ~ gender + race + union + education + experience,
#'   data = jeksterslabRdatarepo::wages
#' )
#' yhat <- as.vector(predict(model))
#' epsilonhat <- as.vector(residuals(model))
#' tepsilonhat <- as.vector(rstudent(model))
#' h <- as.vector(hatvalues(model))
#' .residual.plot(
#'   yhat = yhat, tepsilonhat = tepsilonhat,
#'   epsilonhat = epsilonhat, h = h
#' )
#' @export
.residual.plot <- function(yhat,
                           epsilonhat,
                           tepsilonhat,
                           h) {
  usr <- par("usr")
  on.exit(par(usr))
  par(mfrow = c(2, 2))
  lw1 <- suppressWarnings(loess(epsilonhat ~ yhat))
  j <- order(yhat)
  plot(
    x = yhat,
    y = epsilonhat,
    xlab = "Fitted",
    ylab = "Residuals",
    main = "Residuals vs. Fitted Values"
  )
  lines(yhat[j], lw1$fitted[j], col = "red")
  qqnorm(
    epsilonhat,
    main = "Normal Q-Q Plot of Residuals"
  )
  qqline(
    epsilonhat,
    col = "red"
  )
  sqrtabsstd <- sqrt(abs(tepsilonhat))
  lw2 <- suppressWarnings(loess(sqrtabsstd ~ yhat))
  j <- order(yhat)
  plot(
    x = yhat,
    y = sqrtabsstd,
    xlab = "Fitted",
    ylab = expression(sqrt(abs(Std. ~ Residuals))),
    main = "Scale-Location"
  )
  lines(yhat[j], lw2$fitted[j], col = "red")
  lw3 <- suppressWarnings(loess(tepsilonhat ~ h))
  j <- order(h)
  plot(
    x = h,
    y = tepsilonhat,
    xlab = "Leverage",
    ylab = "Std. Residuals",
    main = "Residuals vs. Leverage"
  )
  lines(h[j], lw2$fitted[j], col = "red")
}
