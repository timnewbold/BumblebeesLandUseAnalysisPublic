suppressMessages(suppressWarnings(library(StatisticalModels)))

inDir <- "3_RunSpeciesLevelModels/"

outDir <- "4_PlotSpeciesLevelModels/"

load(paste(inDir,"TemperatureModels.rd",sep=""))

png(paste(outDir,"TemperatureResultsInteraction.png",sep=""),
    width = 17.5,height = 5,units = "cm",res = 1200)

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

plot(xVals,preds.LowTEI.LUNatural$y,type="l",ylim=c(0,1),col="#00ff00",xlab="Delta TEI",ylab="P. occur",main="Low Baseline TEI")
points(xVals,preds.LowTEI.LUNatural$yplus,type="l",lty=2,col="#00ff00")
points(xVals,preds.LowTEI.LUNatural$yminus,type="l",lty=2,col="#00ff00")


nd <- data.frame(
  TEI_delta=xVals,
  occur=0,
  LandUse=factor("Human",levels=levels(m_full$data$LandUse)),
  TEI_BL=quantile(m_full$data$TEI_BL,0.1),
  row.names = NULL)

preds.LowTEI.LUHuman <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1)
preds.LowTEI.LUHuman <- 1/(1+exp(-preds.LowTEI.LUHuman))

points(xVals,preds.LowTEI.LUHuman$y,type="l",ylim=c(0,1),col="#ff0000")
points(xVals,preds.LowTEI.LUHuman$yplus,type="l",lty=2,col="#ff0000")
points(xVals,preds.LowTEI.LUHuman$yminus,type="l",lty=2,col="#ff0000")


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

preds.HighTEI.LUNatural <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1)
preds.HighTEI.LUNatural <- 1/(1+exp(-preds.HighTEI.LUNatural))

plot(xVals,preds.HighTEI.LUNatural$y,type="l",ylim=c(0,1),col="#00ff00",xlab="Delta TEI",ylab="P. occur",main="Median Baseline TEI")
points(xVals,preds.HighTEI.LUNatural$yplus,type="l",lty=2,col="#00ff00")
points(xVals,preds.HighTEI.LUNatural$yminus,type="l",lty=2,col="#00ff00")


nd <- data.frame(
  TEI_delta=xVals,
  occur=0,
  LandUse=factor("Human",levels=levels(m_full$data$LandUse)),
  TEI_BL=quantile(m_full$data$TEI_BL,0.5),
  row.names = NULL)

preds.HighTEI.LUHuman <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1)
preds.HighTEI.LUHuman <- 1/(1+exp(-preds.HighTEI.LUHuman))

points(xVals,preds.HighTEI.LUHuman$y,type="l",ylim=c(0,1),col="#ff0000")
points(xVals,preds.HighTEI.LUHuman$yplus,type="l",lty=2,col="#ff0000")
points(xVals,preds.HighTEI.LUHuman$yminus,type="l",lty=2,col="#ff0000")



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

plot(xVals,preds.HighTEI.LUNatural$y,type="l",ylim=c(0,1),col="#00ff00",xlab="Delta TEI",ylab="P. occur",main="High Baseline TEI")
points(xVals,preds.HighTEI.LUNatural$yplus,type="l",lty=2,col="#00ff00")
points(xVals,preds.HighTEI.LUNatural$yminus,type="l",lty=2,col="#00ff00")


nd <- data.frame(
  TEI_delta=xVals,
  occur=0,
  LandUse=factor("Human",levels=levels(m_full$data$LandUse)),
  TEI_BL=quantile(m_full$data$TEI_BL,0.9),
  row.names = NULL)

preds.HighTEI.LUHuman <- PredictGLMER(model = m_full$model,data = nd,se.fit = TRUE,seMultiplier = 1)
preds.HighTEI.LUHuman <- 1/(1+exp(-preds.HighTEI.LUHuman))

points(xVals,preds.HighTEI.LUHuman$y,type="l",ylim=c(0,1),col="#ff0000")
points(xVals,preds.HighTEI.LUHuman$yplus,type="l",lty=2,col="#ff0000")
points(xVals,preds.HighTEI.LUHuman$yminus,type="l",lty=2,col="#ff0000")

invisible(dev.off())
