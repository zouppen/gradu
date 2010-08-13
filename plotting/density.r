read.zipola <- function(filename)
{
  cols <- c("i1","i2","i3","x1","x2","x3","density")
  read.table(file(filename), sep=",",col.names=cols)
}

# This function is a derivate work from plot.stepfun
densityplot.combi <-
  function (zipola, xval, xlim, ylim = range(c(y, Fn.kn)), xlab = "x", 
            ylab = "Fn(x)", main = NULL, add = FALSE, verticals = TRUE, 
            do.points = T, do.limits = T, do.lineinfo = T,
            pch = 1, col = par("col"),
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

  # Common variables for plotting. Anomaly filter contains TRUE if i an anomaly.
  anofilter <- zipola$density > anolim
  anomalyx <- zipola$density[anofilter]
  anomalyy <- x(anomalyx)
  
  if (do.limits) {
    # Plot anomaly limits
    abline(v=anosd,lty=2)
    abline(v=anolim,lty=1)
  }
  if (do.points) {
    # Plot anomalous points
    points(anomalyx, anomalyy, pch = pch, col = col.points, cex = cex.points)
  }
  if (do.lineinfo) {
    # Plot lineinfo tuples for each anomaly
    lineinfo <- paste(" ", paste(zipola$i1[anofilter],
                                 zipola$i2[anofilter],
                                 zipola$i3[anofilter],sep=","))
    text(anomalyx, anomalyy,lineinfo,srt="-45",adj=0)
  }
  if (verticals) 
    segments(knF, y[-n], knF, y[-1L], col = col.vert, lty = lty, 
             lwd = lwd)
  invisible(list(t = ti, y = y))
}

# This function is a derivate work from plot.stepfun.
# Produces only graph with densities 0...3*sd
densityplot.normal <-
  function (zipola, xval, xlim, ylim = range(c(y, Fn.kn)), xlab = "x", 
            ylab = "Fn(x)", main = NULL, add = FALSE, verticals = TRUE, 
            do.limits = T,
            pch = 1, col = par("col"),
            col.points = col, cex.points = par("cex"), col.hor = col, 
            col.vert = col, lty = par("lty"), lwd = par("lwd"), ...) 
{
  # Anomaly limit is 3 times the standard deviation of densities
  anosd <- sd(zipola$density)
  anolim <- 3 * anosd

  anofilter <- zipola$density <= anolim
  rawdata <- zipola$density[anofilter]

  x <- ecdf(rawdata)

  # The following is basically copypaste from the internal plotter.
  
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
  }
  if (verticals) 
    segments(knF, y[-n], knF, y[-1L], col = col.vert, lty = lty, 
             lwd = lwd)
  invisible(list(t = ti, y = y))
}

# Plots densities the simple way.
densityplot.classic <- function(zipola, do.lineinfo = T, do.random = F) {
  if (do.random) {
    zipola <- sample.table(zipola)
  }
  ano.sd <- sd(zipola$density)
  ano.lim <- 3 * ano.sd

  ano.filter <- zipola$density > ano.lim

  ano.x <- (1:length(zipola$density))[ano.filter]
  ano.y <- zipola$density[ano.filter]

  normal.x <- (1:length(zipola$density))[!ano.filter]
  normal.y <- zipola$density[!ano.filter]
  
  # Plot nothing, just ensure axes are scaled nicely
  plot(zipola$density, pch=NA_integer_)

  # Add anomaly limits
  abline(h=ano.sd,lty=2)
  abline(h=ano.lim,lty=1)

  # Plot normal activity
  points(normal.x, normal.y, pch=19)

  # Plot anomalies
  points(ano.x, ano.y, pch=2)
  
  if (do.lineinfo) {
    # Plot lineinfo tuples for each anomaly
    lineinfo <- paste(zipola$i1[ano.filter],
                      zipola$i2[ano.filter],
                      zipola$i3[ano.filter],sep=",")
    text(ano.x, ano.y,lineinfo,srt="0",pos=1)
  }  
}

# Randomizes the order of samples in a table.
sample.table <- function(tbl) {
  size <- length(tbl[,1])
  if (!exists(".Random.seed")) sample(1) # Make sure there is an RNG ready.
  
  # Using the same seed to get identical sampling order for every columns.
  oldseed <- .Random.seed
  for (i in (1:length(tbl))) {
    .Random.seed <- oldseed
    tbl[,i] <- sample(tbl[,i])
  }
  # keep the new seed. (otherwise rng doesn't advance)
  tbl
}
