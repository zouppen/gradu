read.zipola <- function(filename)
{
  cols <- c("i1","i2","i3","x1","x2","x3","density")
  read.table(file(filename), sep=",",col.names=cols)
}

densityplot <-
  function (zipola, xval, xlim, ylim = range(c(y, Fn.kn)), xlab = "x", 
            ylab = "Fn(x)", main = NULL, add = FALSE, verticals = TRUE, 
            do.points = T, do.limits = T, pch = 1, col = par("col"), 
            col.points = col, cex.points = par("cex"), col.hor = col, 
            col.vert = col, lty = par("lty"), lwd = par("lwd"), ...) 
{
  # Anomaly limit is 3 times the standard deviation of densities
  anosd <- sd(zipola$density)
  anolim <- 3 * anosd
  x <- ecdf(zipola$density)

  if (missing(main)) 
    main <- {
      cl <- attr(x, "call")
      deparse(if (!is.null(cl)) 
              cl
      else sys.call())
    }
  knF <- knots(x)
  xval <- if (missing(xval)) 
    knF
  else sort(xval)
  if (missing(xlim)) {
    rx <- range(xval)
    dr <- if (length(xval) > 1) 
      max(0.08 * diff(rx), median(diff(xval)))
    else abs(xval)/16
    xlim <- rx + dr * c(-1, 1)
  }
  else dr <- diff(xlim)
  knF <- knF[xlim[1L] - dr <= knF & knF <= xlim[2L] + dr]
  ti <- c(xlim[1L] - dr, knF, xlim[2L] + dr)
  ti.l <- ti[-length(ti)]
  ti.r <- ti[-1L]
  y <- x(0.5 * (ti.l + ti.r))
  n <- length(y)
  Fn.kn <- x(knF)
  if (add) 
    segments(ti.l, y, ti.r, y, col = col.hor, lty = lty, 
             lwd = lwd, ...)
  else {
    if (missing(ylim)) 
      ylim <- range(c(y, Fn.kn))
    plot(0, 0, type = "n", xlim = xlim, ylim = ylim, xlab = xlab, 
         ylab = ylab, main = main, ...)
    segments(ti.l, y, ti.r, y, col = col.hor, lty = lty, 
             lwd = lwd)
  }
  if (do.limits) {
    # Plot anomaly limits
    abline(v=anosd,lty=2)
    abline(v=anolim,lty=1)
  }
  if (do.points) {
    # Plot anomalous points
    anomalyx <- zipola$density[zipola$density > anolim]
    anomalyy <- x(anomalyx)
    points(anomalyx, anomalyy, pch = pch, col = col.points, cex = cex.points)
  }
  if (verticals) 
    segments(knF, y[-n], knF, y[-1L], col = col.vert, lty = lty, 
             lwd = lwd)
  invisible(list(t = ti, y = y))
}
