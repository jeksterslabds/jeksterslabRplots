#' @title Scatter Plot Matrix
#'
#' @description Adapted
#'   from <http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs> .
#'
#' @family plotting functions
#' @keywords plot
#' @import graphics
#' @importFrom stats cor loess cor.test
#' @param data Matrix or data frame.
#' @examples
#' .scatter.plot(data = jeksterslabRdatarepo::wages)
#' @export
.scatter.plot <- function(data) {
  panel.hist <- function(x, ...) {
    usr <- par("usr")
    on.exit(
      par(usr)
    )
    par(
      usr = c(usr[1:2], 0, 1.5)
    )
    h <- hist(
      x,
      plot = FALSE
    )
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y / max(y)
    rect(
      xleft = breaks[-nB],
      ybottom = 0,
      xright = breaks[-1],
      ytop = y,
      col = "cyan",
      ...
    )
  }
  panel.cor <- function(x, y) {
    usr <- par("usr")
    on.exit(
      par(usr)
    )
    par(
      usr = c(0, 1, 0, 1)
    )
    r <- round(
      x = cor(
        x = x,
        y = y
      ),
      digits = 2
    )
    p <- cor.test(
      x = x,
      y = y
    )
    r.p <- round(
      x = p$p.value,
      digits = 3
    )
    txt <- paste0(
      "r = ",
      r,
      "\np = ",
      r.p
    )
    text(
      x = 0.5,
      y = 0.5,
      labels = txt
    )
  }
  # Customize upper panel
  upper.panel <- function(x, y) {
    lw1 <- suppressWarnings(
      loess(
        formula = y ~ x
      )
    )
    j <- order(x)
    points(
      x = x,
      y = y
    )
    lines(
      x = x[j],
      y = lw1$fitted[j],
      type = "l",
      col = "red"
    )
  }
  # Create the plots
  pairs(
    data,
    lower.panel = panel.cor,
    upper.panel = upper.panel,
    diag.panel = panel.hist
  )
}
