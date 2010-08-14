pdf("../pics/service_resources.pdf")

palvelu <- read.csv("../anomaliat/a.csv",col.names=c("resource","count","paramCount","sd","grams"),header=F)
plot(palvelu$grams,palvelu$paramCount,main="Palvelun resurssien ominaisuudet",xlab="N-grammien lukumäärä",ylab="GET-parametrien lukumäärä")

dev.off()
