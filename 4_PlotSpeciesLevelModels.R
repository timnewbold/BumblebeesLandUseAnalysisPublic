suppressMessages(suppressWarnings(library(StatisticalModels)))

inDir <- "3_RunSpeciesLevelModels/"

outDir <- "4_PlotSpeciesLevelModels/"

load(paste(inDir,"TemperatureModels.rd",sep=""))

png(paste(outDir,"LandUse.png",sep=""),
    width = 8.5,height = 8.5,units = "cm",res = 1200)

PlotGLMERFactor(model = m_lu$model,data = m_lu$data,responseVar = "P. occur",
                logLink = "b",catEffects = "LandUse",seMultiplier = 1.96)

invisible(dev.off())

png(paste(outDir,"TemperatureResultsInteraction.png",sep=""),
    width = 17.5,height = 5,units = "cm",res = 1200)

ylims <- c(15,270)

par(mfrow=c(1,3))
par(tck=-0.01)
par(mgp=c(1.4,0.2,0))
par(mar=c(2.6,2.6,1,0.2))

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

preds.LowTEI.LUNatural <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1)
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

preds.LowTEI.LUHuman <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1)
preds.LowTEI.LUHuman <- 1/(1+exp(-preds.LowTEI.LUHuman))
# 
# points(xVals,preds.LowTEI.LUHuman$y,type="l",ylim=c(0,1),col="#d95f02")
# points(xVals,preds.LowTEI.LUHuman$yplus,type="l",lty=2,col="#d95f02")
# points(xVals,preds.LowTEI.LUHuman$yminus,type="l",lty=2,col="#d95f02")

preds.LowTEI.LUHumanRel <- preds.LowTEI.LUHuman/preds.LowTEI.LUNatural$y

plot(xVals,preds.LowTEI.LUHumanRel$y*100,type="l",ylim=ylims,col="#1b9e77",xlab="Delta TEI",ylab="P. occur",main="Low Baseline TEI")
points(xVals,preds.LowTEI.LUHumanRel$yplus*100,type="l",lty=2,col="#1b9e77")
points(xVals,preds.LowTEI.LUHumanRel$yminus*100,type="l",lty=2,col="#1b9e77")

abline(h=100,lty=2,col="#999999")

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

preds.MedTEI.LUNatural <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1)
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

preds.MedTEI.LUHuman <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1)
preds.MedTEI.LUHuman <- 1/(1+exp(-preds.MedTEI.LUHuman))
# 
# points(xVals,preds.MedTEI.LUHuman$y,type="l",ylim=c(0,1),col="#d95f02")
# points(xVals,preds.MedTEI.LUHuman$yplus,type="l",lty=2,col="#d95f02")
# points(xVals,preds.MedTEI.LUHuman$yminus,type="l",lty=2,col="#d95f02")

preds.MedTEI.LUHumanRel <- preds.MedTEI.LUHuman/preds.MedTEI.LUNatural$y

plot(xVals,preds.MedTEI.LUHumanRel$y*100,type="l",ylim=ylims,col="#1b9e77",xlab="Delta TEI",ylab="P. occur",main="Low Baseline TEI")
points(xVals,preds.MedTEI.LUHumanRel$yplus*100,type="l",lty=2,col="#1b9e77")
points(xVals,preds.MedTEI.LUHumanRel$yminus*100,type="l",lty=2,col="#1b9e77")

abline(h=100,lty=2,col="#999999")

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

preds.HighTEI.LUNatural <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1)
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

preds.HighTEI.LUHuman <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1)
preds.HighTEI.LUHuman <- 1/(1+exp(-preds.HighTEI.LUHuman))
# 
# points(xVals,preds.HighTEI.LUHuman$y,type="l",ylim=c(0,1),col="#d95f02")
# points(xVals,preds.HighTEI.LUHuman$yplus,type="l",lty=2,col="#d95f02")
# points(xVals,preds.HighTEI.LUHuman$yminus,type="l",lty=2,col="#d95f02")


preds.HighTEI.LUHumanRel <- preds.HighTEI.LUHuman/preds.HighTEI.LUNatural$y

plot(xVals,preds.HighTEI.LUHumanRel$y*100,type="l",ylim=ylims,col="#1b9e77",xlab="Delta TEI",ylab="P. occur",main="Low Baseline TEI")
points(xVals,preds.HighTEI.LUHumanRel$yplus*100,type="l",lty=2,col="#1b9e77")
points(xVals,preds.HighTEI.LUHumanRel$yminus*100,type="l",lty=2,col="#1b9e77")

abline(h=100,lty=2,col="#999999")

invisible(dev.off())
