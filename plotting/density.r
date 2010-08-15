read.zipola <- function(filename)
{
  cols <- c("i1","i2","i3","x1","x2","x3","density")
  read.table(file(filename), sep=",",col.names=cols)
}

group.anomalies <- function(zipola) {
  ano <- vector(6,mode="list")
  names(ano) <- c("sd","limit","ano.x","ano.y","normal.x","normal.y")

  ano$sd <- sd(zipola$density)
  ano$limit <- 3 * ano$sd

  anofilter <- zipola$density > ano$limit

  ano$ano.x <- (1:length(zipola$density))[anofilter]
  ano$ano.y <- zipola$density[anofilter]

  ano$normal.x <- (1:length(zipola$density))[!anofilter]
  ano$normal.y <- zipola$density[!anofilter]

  ano
}

# Plots densities the simple way.
densityplot.classic <- function(zipola, do.lineinfo = T, do.debug = F, main="Anomaliat") {

  ano.sd <- sd(zipola$density)
  ano.lim <- 3 * ano.sd

  ano.filter <- zipola$density > ano.lim

  ano.x <- (1:length(zipola$density))[ano.filter]
  ano.y <- zipola$density[ano.filter]

  normal.x <- (1:length(zipola$density))[!ano.filter]
  normal.y <- zipola$density[!ano.filter]
  
  # Plot nothing, just ensure axes are scaled nicely
  plot(zipola$density, pch=NA_integer_,xaxt="n",xlab=NA,ylab="etäisyys",main=main)

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

    if (do.debug) {
      print(lineinfo)
    }
  }
}

plot.service <- function(service, filename, do.svg=TRUE, do.pdf=FALSE) {

  print(paste("plotataan: ",service," ",filename))
  
  rawdata <- read.zipola(paste("../anomaliat/resource_coords/",filename,sep=""))
  n <- length(rawdata[,1])
  main <- paste("Poikkeavuudet resurssissa ",service, "  (n=",n,")", sep="")

  if (do.svg) {
    ## Plot SVG
    svg(paste("../pics/tiheyskuvat/service_orig_",service,".svg",sep=""))
    densityplot.classic(rawdata, main=main)
    dev.off()
  }

  if (do.pdf) {
    ## Plot PDF
    pdf(paste("../pics/tiheyskuvat/service_orig_",service,".pdf",sep=""))
    densityplot.classic(rawdata, main=main)
    dev.off()
  }
}

plot.services <- function(tbl) {
  
  for (i in (1:length(tbl[,1]))) {
    plot.service(tbl$service[i],tbl$file[i])
  }
}

## Diffusion functions

optimal.variation <- function(v, randomness=0.01) {
  variation <- max(v) - min(v)
  randomness * variation
}

variate <- function(v, ...) {
  rnorm(v,mean=v,sd=optimal.variation(v))
}

## plot.diffusion <- function(tbl, randomness=0) {
  
