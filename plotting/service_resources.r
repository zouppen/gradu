# Plots service resource plot. Modified manually then.

svg("../pics/service_resources-orig.svg")

palvelu <- read.csv("../anomaliat/a.csv",col.names=c("resource","count","paramCount","sd","grams"),header=F)
plot(palvelu$grams,palvelu$paramCount,xlab="N-grammien lukumäärä",ylab="GET-parametrien lukumäärä")
text(palvelu$grams,palvelu$paramCount,palvelu$resource,srt="0")

dev.off()
