read.zipola <- function(filename)
{
  cols <- c("i1","i2","i3","x1","x2","x3","density")
  read.table(file(filename), sep=",",col.names=cols)
}

group.anomalies <- function(zipola) {
  a <- vector(5,mode="list")
  names(a) <- c("sd","limit","filter","anomalies","normal")

  a$anomalies <- vector(2,mode="list")
  a$normal <- vector(2,mode="list")
  names(a$anomalies) <- c("x","y")
  names(a$normal) <- c("x","y")

  ## Anomaly limit is 3 times the standard deviation of densities.
  a$sd <- sd(zipola$density)
  a$limit <- 3 * a$sd

  a$filter <- zipola$density > a$limit

  a$anomalies$x <- (1:length(zipola$density))[a$filter]
  a$anomalies$y <- zipola$density[a$filter]

  a$normal$x <- (1:length(zipola$density))[!a$filter]
  a$normal$y <- zipola$density[!a$filter]

  ## Returning the master of the vectors.
  a
}

# Plots densities the simple way.
densityplot.classic <- function(zipola, do.lineinfo = T, do.debug = F) {

  a <- group.anomalies(zipola)
  
  # Plot nothing, just ensure axes are scaled nicely
  plot(zipola$density, pch=NA_integer_,xaxt="n",xlab=NA,ylab="etäisyys")

  # Print n to a corner
  mtext(paste("n = ",length(zipola$i1),sep=""), adj=1)
  
  # Add anomaly limits
  abline(h=a$sd,lty=2)
  abline(h=a$limit,lty=1)

  # Plot normal activity
  points(a$normal$x, a$normal$y, pch=19)

  # Plot anomalies
  points(a$anomalies$x, a$anomalies$y, pch=2)
  
  if (do.lineinfo) {
    # Plot lineinfo tuples for each anomaly
    lineinfo <- paste(zipola$i1[a$filter],
                      zipola$i2[a$filter],
                      zipola$i3[a$filter],sep=",")
    text(a$anomalies$x, a$anomalies$y,lineinfo,srt="0",pos=1)

    if (do.debug) {
      print(lineinfo)
    }
  }
}

plot.service <- function(service, filename, do.svg=FALSE, do.pdf=TRUE) {

  print(paste("plotataan: ",service," ",filename))
  
  rawdata <- read.zipola(paste("../anomaliat/matlab_output/",filename,sep=""))
  n <- length(rawdata[,1])

  ## Plot SVG
  if (do.svg) {
    ## Density map
    svg(paste("../pics/tiheyskuvat/service_",service,".svg",sep=""))
    par(family="serif")
    densityplot.classic(rawdata)
    dev.off()

    # Diffusion map
    svg(paste("../pics/diffuusiokuvat/service_",service,".svg",sep=""))
    par(family="serif")
    diffusionplot(rawdata, randomness=0.01)
    dev.off()
  }

  ## Plot PDF
  if (do.pdf) {
    ## Density map
    pdf(paste("../pics/tiheyskuvat/service_",service,".pdf",sep=""))
    par(family="serif",mar=c(0.3,4,1,0.3))
    densityplot.classic(rawdata)
    dev.off()

    # Diffusion map
    pdf(paste("../pics/diffuusiokuvat/service_",service,".pdf",sep=""))
    par(family="serif",mar=c(4,4,1,0.3))
    diffusionplot(rawdata, randomness=0.01)
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

variate <- function(v, randomness=0.01) {
  rnorm(v,mean=v,sd=optimal.variation(v,randomness=randomness))
}

diffusionplot <- function(zipola, randomness=0, do.lineinfo=T) {
  a <- group.anomalies(zipola)

  ## Variate original data
  variated.x <- variate(zipola$x1,randomness)
  variated.y <- variate(zipola$x2,randomness)
  
  ## Plot nothing, just ensure axes are scaled nicely
  plot(zipola$x1, zipola$x2, pch=NA_integer_,xlab="2. ominaisvektori",
       ylab="3. ominaisvektori",
       xlim=limits.textfriendly(zipola$x1),
       ylim=limits.textfriendly(zipola$x2))

  # Print n to a corner
  mtext(paste("n = ",length(zipola$i1),sep=""), adj=1)
  
  ## Plot normal activity
  points(variated.x[!a$filter],
         variated.y[!a$filter])

  ## Plot anomalies
  points(variated.x[a$filter],
         variated.y[a$filter],
         pch=2)

  if (do.lineinfo) {
    # Plot lineinfo tuples for each anomaly
    lineinfo <- paste(zipola$i1[a$filter],
                      zipola$i2[a$filter],
                      zipola$i3[a$filter],sep=",")
    text(variated.x[a$filter], variated.y[a$filter],lineinfo,srt="0",pos=1)
  }
}

limits.textfriendly <- function(v, mult=1.1) {
  rawlimits <- c(min(v),max(v))
  center <- ave(rawlimits)
  nicelimits <- (mult * (rawlimits-center)) + center
  
  nicelimits
}
  
