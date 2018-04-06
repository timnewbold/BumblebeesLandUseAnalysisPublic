suppressMessages(suppressWarnings(library(StatisticalModels)))

inDir <- "2_PrepareDiversityData/"

outDir <- "3_RunSpeciesLevelModels/"

load(paste(inDir,"diversity_data.Rd",sep=""))

modelData <- diversity[,c('occur','LandUse','TEI_BL',
                          'TEI_delta','SS','SSBS',
                          'Taxon_name_entered')]
modelData <- na.omit(modelData)

cat('Temperature models - null\n')

m_null <- GLMER(modelData = modelData,responseVar = "occur",
                fitFamily = "binomial",
                fixedStruct = "1",
                randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")

cat('Temperature models - land use\n')

m_lu <- GLMER(modelData = modelData,responseVar = "occur",
              fitFamily = "binomial",
              fixedStruct = "LandUse",
              randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")

cat('Temperature models - climate\n')

m_clim <- GLMER(modelData = modelData,responseVar = "occur",
                fitFamily = "binomial",
                fixedStruct = "poly(TEI_BL,2)+poly(TEI_delta,2)+poly(TEI_BL,2):poly(TEI_delta,2)",
                randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")

cat('Temperature models - combined\n')

m_full <- GLMER(modelData = modelData,responseVar = "occur",
                fitFamily = "binomial",
                fixedStruct = "LandUse+poly(TEI_BL,2)+poly(TEI_delta,2)+LandUse:poly(TEI_BL,2)+LandUse:poly(TEI_delta,2)+poly(TEI_BL,2):poly(TEI_delta,2)+LandUse:poly(TEI_BL,2):poly(TEI_delta,2)",
                randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")

print(AIC(m_null$model,m_lu$model,m_clim$model,m_full$model))

save(m_null,m_lu,m_clim,m_full,file = paste(outDir,"TemperatureModels.rd",sep=""))

rm(m_null,m_lu,m_clim,m_full)


modelData <- diversity[,c('occur','LandUse','PEI_BL',
                          'PEI_delta','SS','SSBS',
                          'Taxon_name_entered')]
modelData <- na.omit(modelData)


cat('Precipitation models - null\n')

m_null <- GLMER(modelData = modelData,responseVar = "occur",
                fitFamily = "binomial",
                fixedStruct = "1",
                randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")

cat('Precipitation models - land use\n')

m_lu <- GLMER(modelData = modelData,responseVar = "occur",
              fitFamily = "binomial",
              fixedStruct = "LandUse",
              randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")

cat('Precipitation models - climate\n')

m_clim <- GLMER(modelData = modelData,responseVar = "occur",
                fitFamily = "binomial",
                fixedStruct = "poly(PEI_BL,2)+poly(PEI_delta,2)+poly(PEI_BL,2):poly(PEI_delta,2)",
                randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")

cat('Precipitation models - combined\n')

m_full <- GLMER(modelData = modelData,responseVar = "occur",
                fitFamily = "binomial",
                fixedStruct = "LandUse+poly(PEI_BL,2)+poly(PEI_delta,2)+LandUse:poly(PEI_BL,2)+LandUse:poly(PEI_delta,2)+poly(PEI_BL,2):poly(PEI_delta,2)+LandUse:poly(PEI_BL,2):poly(PEI_delta,2)",
                randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")

AIC(m_null,m_lu,m_clim,m_full)

save(m_null,m_lu,m_clim,m_full,file = paste(outDir,"PrecipitationModels.rd",sep=""))

rm(m_null,m_lu,m_clim,m_full)

# 
# 
# png("ProvisionalResults.png",width = 8.5,height = 14,units = "cm",res = 1200)
# 
# par(mfrow=c(2,1))
# 
# m1 <- GLMER(modelData = diversity,responseVar = "occur",fitFamily = "binomial",fixedStruct = "LandUse",randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# m2 <- GLMER(modelData = diversity,responseVar = "occur",fitFamily = "binomial",fixedStruct = "LandUse+poly(TEI_BL,2)",randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# m3 <- GLMER(modelData = diversity,responseVar = "occur",fitFamily = "binomial",fixedStruct = "LandUse+poly(TEI_BL,2)+LandUse:poly(TEI_BL,2)",randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# 
# PlotGLMERContinuous(model = m3$model,data = m3$data,effects = "TEI_BL",byFactor = "LandUse",xlab = "Baseline TEI",ylab = "Occurrence",logLink = "b",seMultiplier = 1,line.cols = c("#00ff00","#ff0000"))
# # 
# # m4 <- GLMER(modelData = diversity,responseVar = "occur",fitFamily = "binomial",fixedStruct = "LandUse",randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# # m5 <- GLMER(modelData = diversity,responseVar = "occur",fitFamily = "binomial",fixedStruct = "LandUse+poly(PEI_BL,2)",randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# # m6 <- GLMER(modelData = diversity,responseVar = "occur",fitFamily = "binomial",fixedStruct = "LandUse+poly(PEI_BL,2)+LandUse:poly(PEI_BL,2)",randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# # 
# # PlotGLMERContinuous(model = m6$model,data = m6$data,effects = "PEI_BL",byFactor = "LandUse",xlab = "Baseline PEI",ylab = "Occurrence",logLink = "b",seMultiplier = 1,line.cols = c("#00ff00","#ff0000"))
# 
# m7 <- GLMER(modelData = diversity,responseVar = "occur",fitFamily = "binomial",fixedStruct = "LandUse",randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# m8 <- GLMER(modelData = diversity,responseVar = "occur",fitFamily = "binomial",fixedStruct = "LandUse+poly(TEI_delta,2)",randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# m9 <- GLMER(modelData = diversity,responseVar = "occur",fitFamily = "binomial",fixedStruct = "LandUse+poly(TEI_delta,2)+LandUse:poly(TEI_delta,2)",randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# 
# PlotGLMERContinuous(model = m9$model,data = m9$data,effects = "TEI_delta",byFactor = "LandUse",xlab = "Delta TEI",ylab = "Occurrence",logLink = "b",seMultiplier = 1,line.cols = c("#00ff00","#ff0000"))
# 
# # m10 <- GLMER(modelData = diversity,responseVar = "occur",fitFamily = "binomial",fixedStruct = "LandUse",randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# # m11 <- GLMER(modelData = diversity,responseVar = "occur",fitFamily = "binomial",fixedStruct = "LandUse+poly(PEI_delta,2)",randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# # m12 <- GLMER(modelData = diversity,responseVar = "occur",fitFamily = "binomial",fixedStruct = "LandUse+poly(PEI_delta,2)+LandUse:poly(PEI_delta,2)",randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# # 
# # PlotGLMERContinuous(model = m12$model,data = m12$data,effects = "PEI_delta",byFactor = "LandUse",xlab = "Delta PEI",ylab = "Occurrence",logLink = "b",seMultiplier = 1,line.cols = c("#00ff00","#ff0000"))
# 
# dev.off()

# m13 <- GLMER(modelData = diversity,responseVar = "occur",fitFamily = "binomial",fixedStruct = "LandUse+poly(TEI_BL,2)+poly(TEI_delta,2)+LandUse:poly(TEI_BL,2)+LandUse:poly(TEI_delta,2)+poly(TEI_BL,2):poly(TEI_delta,2)+LandUse:poly(TEI_BL,2):poly(TEI_delta,2)",randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# 
# 





# 
# m14 <- GLMER(modelData = diversity,responseVar = "occur",fitFamily = "binomial",fixedStruct = "LandUse+poly(PEI_BL,2)+poly(PEI_delta,2)+LandUse:poly(PEI_BL,2)+LandUse:poly(PEI_delta,2)+poly(PEI_BL,2):poly(PEI_delta,2)+LandUse:poly(PEI_BL,2):poly(PEI_delta,2)",randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# 
# 
# 
# png("TempResultsInteractionPrecipitation.png",width = 17.5,height = 5,units = "cm",res = 1200)
# 
# par(mfrow=c(1,3))
# 
# xVals <- seq(
#   from=quantile(m14$data$PEI_delta,0.025),
#   to=quantile(m14$data$PEI_delta,0.975),
#   length.out = 100)
# 
# nd <- data.frame(
#   PEI_delta=xVals,
#   occur=0,
#   LandUse=factor("Natural",levels=levels(m14$data$LandUse)),
#   PEI_BL=quantile(m14$data$PEI_BL,0.025),
#   row.names = NULL)
# 
# preds.LowPEI.LUNatural <- PredictGLMER(model = m14$model,data = nd,se.fit = TRUE,seMultiplier = 1)
# preds.LowPEI.LUNatural <- 1/(1+exp(-preds.LowPEI.LUNatural))
# 
# plot(xVals,preds.LowPEI.LUNatural$y,type="l",ylim=c(0,1),col="#00ff00",xlab="Delta PEI",ylab="P. occur",main="Low Baseline PEI")
# points(xVals,preds.LowPEI.LUNatural$yplus,type="l",lty=2,col="#00ff00")
# points(xVals,preds.LowPEI.LUNatural$yminus,type="l",lty=2,col="#00ff00")
# 
# 
# nd <- data.frame(
#   PEI_delta=xVals,
#   occur=0,
#   LandUse=factor("Human",levels=levels(m14$data$LandUse)),
#   PEI_BL=quantile(m14$data$PEI_BL,0.025),
#   row.names = NULL)
# 
# preds.LowPEI.LUHuman <- PredictGLMER(model = m14$model,data = nd,se.fit = TRUE,seMultiplier = 1)
# preds.LowPEI.LUHuman <- 1/(1+exp(-preds.LowPEI.LUHuman))
# 
# points(xVals,preds.LowPEI.LUHuman$y,type="l",ylim=c(0,1),col="#ff0000")
# points(xVals,preds.LowPEI.LUHuman$yplus,type="l",lty=2,col="#ff0000")
# points(xVals,preds.LowPEI.LUHuman$yminus,type="l",lty=2,col="#ff0000")
# 
# 
# xVals <- seq(
#   from=quantile(m14$data$PEI_delta,0.025),
#   to=quantile(m14$data$PEI_delta,0.975),
#   length.out = 100)
# 
# nd <- data.frame(
#   PEI_delta=xVals,
#   occur=0,
#   LandUse=factor("Natural",levels=levels(m14$data$LandUse)),
#   PEI_BL=quantile(m14$data$PEI_BL,0.5),
#   row.names = NULL)
# 
# preds.HighPEI.LUNatural <- PredictGLMER(model = m14$model,data = nd,se.fit = TRUE,seMultiplier = 1)
# preds.HighPEI.LUNatural <- 1/(1+exp(-preds.HighPEI.LUNatural))
# 
# plot(xVals,preds.HighPEI.LUNatural$y,type="l",ylim=c(0,1),col="#00ff00",xlab="Delta PEI",ylab="P. occur",main="Median Baseline PEI")
# points(xVals,preds.HighPEI.LUNatural$yplus,type="l",lty=2,col="#00ff00")
# points(xVals,preds.HighPEI.LUNatural$yminus,type="l",lty=2,col="#00ff00")
# 
# 
# nd <- data.frame(
#   PEI_delta=xVals,
#   occur=0,
#   LandUse=factor("Human",levels=levels(m14$data$LandUse)),
#   PEI_BL=quantile(m14$data$PEI_BL,0.5),
#   row.names = NULL)
# 
# preds.HighPEI.LUHuman <- PredictGLMER(model = m14$model,data = nd,se.fit = TRUE,seMultiplier = 1)
# preds.HighPEI.LUHuman <- 1/(1+exp(-preds.HighPEI.LUHuman))
# 
# points(xVals,preds.HighPEI.LUHuman$y,type="l",ylim=c(0,1),col="#ff0000")
# points(xVals,preds.HighPEI.LUHuman$yplus,type="l",lty=2,col="#ff0000")
# points(xVals,preds.HighPEI.LUHuman$yminus,type="l",lty=2,col="#ff0000")
# 
# 
# 
# xVals <- seq(
#   from=quantile(m14$data$PEI_delta,0.025),
#   to=quantile(m14$data$PEI_delta,0.975),
#   length.out = 100)
# 
# nd <- data.frame(
#   PEI_delta=xVals,
#   occur=0,
#   LandUse=factor("Natural",levels=levels(m14$data$LandUse)),
#   PEI_BL=quantile(m14$data$PEI_BL,0.975),
#   row.names = NULL)
# 
# preds.HighPEI.LUNatural <- PredictGLMER(model = m14$model,data = nd,se.fit = TRUE,seMultiplier = 1)
# preds.HighPEI.LUNatural <- 1/(1+exp(-preds.HighPEI.LUNatural))
# 
# plot(xVals,preds.HighPEI.LUNatural$y,type="l",ylim=c(0,1),col="#00ff00",xlab="Delta PEI",ylab="P. occur",main="High Baseline PEI")
# points(xVals,preds.HighPEI.LUNatural$yplus,type="l",lty=2,col="#00ff00")
# points(xVals,preds.HighPEI.LUNatural$yminus,type="l",lty=2,col="#00ff00")
# 
# 
# nd <- data.frame(
#   PEI_delta=xVals,
#   occur=0,
#   LandUse=factor("Human",levels=levels(m14$data$LandUse)),
#   PEI_BL=quantile(m14$data$PEI_BL,0.975),
#   row.names = NULL)
# 
# preds.HighPEI.LUHuman <- PredictGLMER(model = m14$model,data = nd,se.fit = TRUE,seMultiplier = 1)
# preds.HighPEI.LUHuman <- 1/(1+exp(-preds.HighPEI.LUHuman))
# 
# points(xVals,preds.HighPEI.LUHuman$y,type="l",ylim=c(0,1),col="#ff0000")
# points(xVals,preds.HighPEI.LUHuman$yplus,type="l",lty=2,col="#ff0000")
# points(xVals,preds.HighPEI.LUHuman$yminus,type="l",lty=2,col="#ff0000")
# 
# dev.off()
