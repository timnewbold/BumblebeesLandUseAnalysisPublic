suppressMessages(suppressWarnings(library(raster)))

currentProjDir <- "7_ProjectModelsLandUseAndClimate/"
futureProjDir <- "12_ProjectModelsLandUseAndClimateFuture/"
cropProdDir <- "18_MapBumblebeeDependentCrops/"

outDir <- "19_CompareProjectionsWithBumblebeeDependentCropProduction/"

proj2005 <- readRDS(paste0(currentProjDir,"2005MapLandUseAndClimate.rds"))
proj2070 <- readRDS(paste0(futureProjDir,"2070MapLandUseAndClimateScenario85.rds"))

cropProd <- raster(paste0(cropProdDir,"ProductionMap.tif"))

cropProd <- cropProd/(area(cropProd)*100)

projData <- data.frame(proj2005=values(proj2005),
                       proj2070=values(proj2070),
                       cropProd=values(cropProd))
projData$cropProd[is.na(projData$cropProd)] <- 0

projData$cropProdCut <- cut(x = projData$cropProd,breaks=c(-1,0,0.001,0.01,0.1,2.26))

ylims <- c(0,180)

png(filename = paste0(
  outDir,"BumblebeeRichnessChangeVersusCropProduction.png"),
  width = 12.5,height = 10,units = "cm",
  res = 1200)

par(las=1)
par(tck=-0.01)
par(mgp=c(1.6,0.2,0))
par(mar=c(2.6,2.6,0.2,0.2))

bCurr <- boxplot((projData$proj2005*100)~projData$cropProdCut,outline=FALSE,range=1,xaxt="n",
        ylim=ylims,at=(1:length(levels(projData$cropProdCut)))-0.2,
        xlim=c(0.75,length(levels(projData$cropProdCut))+0.5),boxwex=0.25,
        ylab="Bumblebee assemblage intactness (%)",col="#7570b3")

bFuture <- boxplot((projData$proj2070*100)~projData$cropProdCut,outline=FALSE,
        range=1,xaxt="n",ylim=ylims,add=TRUE,at=(1:length(levels(projData$cropProdCut)))+0.2,
        boxwex=0.25,col="#d95f02")
axis(side = 1,at = 1:5,labels=c("0","0 - 1","1 - 10","10 - 100","> 100"))
title(xlab="Bumblebee dependent crop production (kg per ha)")
abline(h=100,lty=2,col="#666666")

legend(0.8,180,c("2005","2070"),fill=c("#7570b3","#d95f02"),bty="n")

invisible(dev.off())

# currLM <- lm(projData$proj2005~projData$cropProdCut-1)
# 
# y <- summary(currLM)$coefficients[,'Estimate']
# yplus <- y + 1.96 * summary(currLM)$coefficients[,'Std. Error']
# yminus <- y - 1.96 * summary(currLM)$coefficients[,'Std. Error']
# 
# errbar(x = 1:length(levels(projData$cropProdCut)),
#        y = y,yplus = yplus,yminus = yminus)
# 
# currMeans <- tapply(X = projData$proj2005,INDEX = projData$cropProdCut,FUN = mean,na.rm=TRUE)
# currStDevs <- tapply(X = projData$proj2005,INDEX = projData$cropProdCut,FUN = sd,na.rm=TRUE)
# 
# y <- currMeans
# yplus <- y + 1.96 * currStDevs
# yminus <- y - 1.96 * currStDevs
# 
# errbar(x = 1:length(levels(projData$cropProdCut)),
#        y = y,yplus = yplus,yminus = yminus)
# 

# 
# cropProd <- raster(paste0(cropProdDir,"ProductionMapPercentage.tif"))
# 
# projData <- data.frame(proj2005=values(proj2005),
#                        proj2070=values(proj2070),
#                        cropProd=values(cropProd))
# projData$cropProd[is.na(projData$cropProd)] <- 0
# 
# projData$cropProdCut <- cut(x = projData$cropProd,breaks=c(-1,0,0.5,5,10,100))
# 
# ylims <- c(0,180)
# 
# par(las=1)
# 
# bCurr <- boxplot((projData$proj2005*100)~projData$cropProdCut,outline=FALSE,range=1,xaxt="n",
#                  ylim=ylims,at=(1:length(levels(projData$cropProdCut)))-0.2,
#                  xlim=c(0.75,length(levels(projData$cropProdCut))+0.5),boxwex=0.25,
#                  ylab="Bumblebee community intactness (%)")
# 
# bFuture <- boxplot((projData$proj2070*100)~projData$cropProdCut,outline=FALSE,
#                    range=1,xaxt="n",ylim=ylims,add=TRUE,at=(1:length(levels(projData$cropProdCut)))+0.2,
#                    boxwex=0.25)
# axis(side = 1,at = 1:5,labels=c("0","0 - 0.5","0.5 - 5","5 - 10","> 10"))
# title(xlab="Bumblebee dependent crop production (%)")
# abline(h=100,lty=2,col="#666666")


