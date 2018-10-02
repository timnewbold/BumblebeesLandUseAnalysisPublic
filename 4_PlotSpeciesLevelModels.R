suppressMessages(suppressWarnings(library(StatisticalModels)))

inDir <- "3_RunSpeciesLevelModels/"

outDir <- "4_PlotSpeciesLevelModels/"

load(paste(inDir,"TemperatureModels.rd",sep=""))

xlab <- "Thermal position change"
ylab <- "Relative occurrence prob. (%)"

tiff(paste(outDir,"LandUse.tif",sep=""),
    width = 8.5,height = 8.5,units = "cm",res = 300,compression = "lzw")

PlotGLMERFactor(model = m_lu$model,data = m_lu$data,responseVar = "",
                logLink = "b",catEffects = "LandUse",seMultiplier = 1.96)
title(ylab=ylab)

invisible(dev.off())

cr <- colorRampPalette(colors = c("#0000ff","#ff0000"))

tiff(paste(outDir,"TemperatureResultsInteraction.tif",sep=""),
    width = 17.5,height = 6,units = "cm",res = 300,compression = "lzw")

ylims <- c(-100,180)

layout.mat <- matrix(data = 1:3,nrow = 1,ncol = 3,byrow = TRUE)
layout(mat = layout.mat,widths = c(5.5,5.5,6.5),heights = 6)

# par(mfrow=c(1,3))
par(tck=-0.01)
par(mgp=c(1.6,0.2,0))
par(mai=c(0.792,0.343,0.132,0.0264))
print(par("mai"))
par(las=1)

xVals <- seq(
  from=quantile(m_full$data$TEI_delta,0.025),
  to=quantile(m_full$data$TEI_delta,0.975),
  length.out = 100)

nd <- data.frame(
  TEI_delta=xVals,
  occur=0,
  LandUse=factor("Natural",levels=levels(m_full$data$LandUse)),
  TEI_BL=quantile(m_full$data$TEI_BL,0.1),
  row.names = NULL)

preds.LowTEI.LUNatural <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1.96)
preds.LowTEI.LUNatural <- 1/(1+exp(-preds.LowTEI.LUNatural))
# 
# plot(xVals,preds.LowTEI.LUNatural$y,type="l",ylim=c(0,1),col="#1b9e77",xlab="Delta TEI",ylab="P. occur",main="Low Baseline TEI")
# points(xVals,preds.LowTEI.LUNatural$yplus,type="l",lty=2,col="#1b9e77")
# points(xVals,preds.LowTEI.LUNatural$yminus,type="l",lty=2,col="#1b9e77")


nd <- data.frame(
  TEI_delta=xVals,
  occur=0,
  LandUse=factor("Human",levels=levels(m_full$data$LandUse)),
  TEI_BL=quantile(m_full$data$TEI_BL,0.1),
  row.names = NULL)

preds.LowTEI.LUHuman <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1.96)
preds.LowTEI.LUHuman <- 1/(1+exp(-preds.LowTEI.LUHuman))
# 
# points(xVals,preds.LowTEI.LUHuman$y,type="l",ylim=c(0,1),col="#d95f02")
# points(xVals,preds.LowTEI.LUHuman$yplus,type="l",lty=2,col="#d95f02")
# points(xVals,preds.LowTEI.LUHuman$yminus,type="l",lty=2,col="#d95f02")

preds.LowTEI.LUHumanRel <- ((preds.LowTEI.LUHuman/preds.LowTEI.LUNatural$y)*100)-100

plot(xVals,rep(-9e99,length(xVals)),ylim=ylims,xlab=xlab,ylab=ylab,main="Toward lower thermal limit",col.main=cr(300)[1])
invisible(sapply(X = 1:(length(xVals)-1),FUN = function(i) {
  points(c(xVals[i],xVals[i+1]),c(
    preds.LowTEI.LUHumanRel$y[i],preds.LowTEI.LUHumanRel$y[i+1]),
    type="l",col=cr(300)[i])
  }
))
invisible(sapply(X = seq(from=1,to=length(xVals-1),by=2),FUN = function(i) {
  points(c(xVals[i],xVals[i+1]),c(
    preds.LowTEI.LUHumanRel$yplus[i],preds.LowTEI.LUHumanRel$yplus[i+1]),
    type="l",col=cr(300)[i],lty=2)
}
))
invisible(sapply(X = seq(from=1,to=length(xVals-1),by=2),FUN = function(i) {
  points(c(xVals[i],xVals[i+1]),c(
    preds.LowTEI.LUHumanRel$yminus[i],preds.LowTEI.LUHumanRel$yminus[i+1]),
    type="l",col=cr(300)[i],lty=2)
}
))

abline(h=0,lty=2,col="#999999")

xVals <- seq(
  from=quantile(m_full$data$TEI_delta,0.025),
  to=quantile(m_full$data$TEI_delta,0.975),
  length.out = 100)

nd <- data.frame(
  TEI_delta=xVals,
  occur=0,
  LandUse=factor("Natural",levels=levels(m_full$data$LandUse)),
  TEI_BL=quantile(m_full$data$TEI_BL,0.5),
  row.names = NULL)

preds.MedTEI.LUNatural <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1.96)
preds.MedTEI.LUNatural <- 1/(1+exp(-preds.MedTEI.LUNatural))
# 
# plot(xVals,preds.MedTEI.LUNatural$y,type="l",ylim=c(0,1),col="#1b9e77",xlab="Delta TEI",ylab="P. occur",main="Median Baseline TEI")
# points(xVals,preds.MedTEI.LUNatural$yplus,type="l",lty=2,col="#1b9e77")
# points(xVals,preds.MedTEI.LUNatural$yminus,type="l",lty=2,col="#1b9e77")


nd <- data.frame(
  TEI_delta=xVals,
  occur=0,
  LandUse=factor("Human",levels=levels(m_full$data$LandUse)),
  TEI_BL=quantile(m_full$data$TEI_BL,0.5),
  row.names = NULL)

preds.MedTEI.LUHuman <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1.96)
preds.MedTEI.LUHuman <- 1/(1+exp(-preds.MedTEI.LUHuman))
# 
# points(xVals,preds.MedTEI.LUHuman$y,type="l",ylim=c(0,1),col="#d95f02")
# points(xVals,preds.MedTEI.LUHuman$yplus,type="l",lty=2,col="#d95f02")
# points(xVals,preds.MedTEI.LUHuman$yminus,type="l",lty=2,col="#d95f02")

preds.MedTEI.LUHumanRel <- ((preds.MedTEI.LUHuman/preds.MedTEI.LUNatural$y)*100)-100

plot(xVals,rep(-9e99,length(xVals)),ylim=ylims,xlab=xlab,ylab=NA,main="Middle of thermal range",col.main=cr(300)[150])
invisible(sapply(X = 1:(length(xVals)-1),FUN = function(i) {
  points(c(xVals[i],xVals[i+1]),c(
    preds.MedTEI.LUHumanRel$y[i],preds.MedTEI.LUHumanRel$y[i+1]),
    type="l",col=cr(300)[i+100])
}
))
invisible(sapply(X = seq(from=1,to=length(xVals-1),by=2),FUN = function(i) {
  points(c(xVals[i],xVals[i+1]),c(
    preds.MedTEI.LUHumanRel$yplus[i],preds.MedTEI.LUHumanRel$yplus[i+1]),
    type="l",col=cr(300)[i+100],lty=2)
}
))
invisible(sapply(X = seq(from=1,to=length(xVals-1),by=2),FUN = function(i) {
  points(c(xVals[i],xVals[i+1]),c(
    preds.MedTEI.LUHumanRel$yminus[i],preds.MedTEI.LUHumanRel$yminus[i+1]),
    type="l",col=cr(300)[i+100],lty=2)
}
))

abline(h=0,lty=2,col="#999999")

arr.x <- seq(from=0.01,to=0.032,length.out=100)
invisible(sapply(X = 1:98,FUN = function(i) segments(x0 = arr.x[i],y0 = -195,x1 = arr.x[i+1],y1 = -195,xpd=TRUE,col=cr(100)[i])))
arrows(x0 = arr.x[99],y0 = -195,x1 = arr.x[100],y1 = -195,xpd=TRUE,col=cr(100)[99],angle = 20,length = 0.1)
text(x = 0.009,y = -230,labels = "Stable\nclimate",xpd=TRUE,pos=4)
text(x = 0.033,y = -230,labels = "Rapidly warming\nclimate",xpd=TRUE,pos=2)

xVals <- seq(
  from=quantile(m_full$data$TEI_delta,0.025),
  to=quantile(m_full$data$TEI_delta,0.975),
  length.out = 100)

nd <- data.frame(
  TEI_delta=xVals,
  occur=0,
  LandUse=factor("Natural",levels=levels(m_full$data$LandUse)),
  TEI_BL=quantile(m_full$data$TEI_BL,0.9),
  row.names = NULL)

preds.HighTEI.LUNatural <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1.96)
preds.HighTEI.LUNatural <- 1/(1+exp(-preds.HighTEI.LUNatural))
# 
# plot(xVals,preds.HighTEI.LUNatural$y,type="l",ylim=c(0,1),col="#1b9e77",xlab="Delta TEI",ylab="P. occur",main="High Baseline TEI")
# points(xVals,preds.HighTEI.LUNatural$yplus,type="l",lty=2,col="#1b9e77")
# points(xVals,preds.HighTEI.LUNatural$yminus,type="l",lty=2,col="#1b9e77")


nd <- data.frame(
  TEI_delta=xVals,
  occur=0,
  LandUse=factor("Human",levels=levels(m_full$data$LandUse)),
  TEI_BL=quantile(m_full$data$TEI_BL,0.9),
  row.names = NULL)

preds.HighTEI.LUHuman <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1.96)
preds.HighTEI.LUHuman <- 1/(1+exp(-preds.HighTEI.LUHuman))
# 
# points(xVals,preds.HighTEI.LUHuman$y,type="l",ylim=c(0,1),col="#d95f02")
# points(xVals,preds.HighTEI.LUHuman$yplus,type="l",lty=2,col="#d95f02")
# points(xVals,preds.HighTEI.LUHuman$yminus,type="l",lty=2,col="#d95f02")


preds.HighTEI.LUHumanRel <- ((preds.HighTEI.LUHuman/preds.HighTEI.LUNatural$y)*100)-100

par(mai=c(0.792,0.343,0.132,0.42))

plot(xVals,rep(-9e99,length(xVals)),ylim=ylims,xlab=xlab,ylab=NA,main="Toward upper thermal limit",col.main=cr(300)[300])
invisible(sapply(X = 1:(length(xVals)-1),FUN = function(i) {
  points(c(xVals[i],xVals[i+1]),c(
    preds.HighTEI.LUHumanRel$y[i],preds.HighTEI.LUHumanRel$y[i+1]),
    type="l",col=cr(300)[i+200])
}
))
invisible(sapply(X = seq(from=1,to=length(xVals-1),by=2),FUN = function(i) {
  points(c(xVals[i],xVals[i+1]),c(
    preds.HighTEI.LUHumanRel$yplus[i],preds.HighTEI.LUHumanRel$yplus[i+1]),
    type="l",col=cr(300)[i+200],lty=2)
}
))
invisible(sapply(X = seq(from=1,to=length(xVals-1),by=2),FUN = function(i) {
  points(c(xVals[i],xVals[i+1]),c(
    preds.HighTEI.LUHumanRel$yminus[i],preds.HighTEI.LUHumanRel$yminus[i+1]),
    type="l",col=cr(300)[i+200],lty=2)
}
))

abline(h=0,lty=2,col="#999999")

arrows(x0 = 0.035,y0 = 20,x1 = 0.035,y1 = 170,xpd=TRUE,angle=20,length=0.1)
text(x = 0.037,y = 95,labels = "Human land \nuse better",srt=90,xpd=TRUE)
arrows(x0 = 0.035,y0 = -20,x1 = 0.035,y1 = -120,xpd=TRUE,angle=20,length=0.1)
text(x = 0.037,y = -70,labels = "Human land \nuse worse",srt=90,xpd=TRUE)

invisible(dev.off())
