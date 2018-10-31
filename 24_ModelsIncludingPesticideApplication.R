
suppressMessages(suppressWarnings(library(StatisticalModels)))
suppressMessages(suppressWarnings(library(raster)))

dataDir <- "0_data/"
inDir <- "2_PrepareDiversityData/"
modelsDir <- "3_RunSpeciesLevelModels/"

outDir <- "24_ModelsIncludingPesticideApplication/"

load(paste(inDir,"diversity_data.Rd",sep=""))

diversity$LogElevation <- log(diversity$Elevation+2)

modelData <- diversity[,c('occur','LandUse','TEI_BL',
                          'TEI_delta','LogElevation','SS','SSBS',
                          'Taxon_name_entered','Longitude','Latitude',
                          'Predominant_land_use')]
modelData <- na.omit(modelData)

pest.appl <- raster(paste0(dataDir,"theil-senNeonics.img"))
pest.appl <- raster::projectRaster(
  from = pest.appl,res = 0.5,crs = CRS(
    '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

modelData$PesticideApplication <- raster::extract(x = pest.appl,y = modelData[,c('Longitude','Latitude')])
modelData$PesticideApplication[(modelData$PesticideApplication)<0] <- 0

modelData$LogPesticideApplication <- log(modelData$PesticideApplication+1)

modelData <- na.omit(modelData)

modelData$PesticideCut <- cut(modelData$LogPesticideApplication,breaks=c(-1,0,0.0002,0.015),labels=c("0_None","1_Low","2_High"))

modelData$PesticideBinary <- factor(ifelse(modelData$PesticideApplication>0,1,0))

modelData$LUPesticide <- NA
modelData$LUPesticide <- paste(modelData$LandUse,modelData$PesticideCut)
modelData$LUPesticide[(modelData$LUPesticide=="Natural 1_Low")] <- "Natural 1_Low_High"
modelData$LUPesticide[(modelData$LUPesticide=="Natural 2_High")] <- "Natural 1_Low_High"

# modelData$LUPesticide[(modelData$LandUse=="Natural")] <- "Natural"
# modelData$LUPesticide[(modelData$LandUse=="Human")] <- paste(
#   modelData$LandUse,modelData$PesticideBinary)[(modelData$LandUse=="Human")]
modelData$LUPesticide <- factor(modelData$LUPesticide)
modelData$LUPesticide <- relevel(modelData$LUPesticide,ref="Natural 0_None")

load(paste0(modelsDir,"TemperatureModels.rd"))

model_lu_sub <- glmer(formula = occur ~ LandUse+LogElevation+ 
                        (1 | SS)+(1 | SSBS)+(1 | Taxon_name_entered),
                      data = modelData,family = "binomial")

model_pest <- glmer(formula = occur ~ LUPesticide+LogElevation+ 
                      (1 | SS)+(1 | SSBS)+(1 | Taxon_name_entered),
                    data = modelData,family = "binomial")

model_clim <- glmer(formula = occur ~ LandUse+poly(TEI_BL,2)+
                      poly(TEI_delta,2)+LandUse:poly(TEI_BL,2)+LandUse:poly(TEI_delta,2)+
                      LandUse:poly(TEI_BL,2):poly(TEI_delta,2)+LogElevation+ 
                      (1 | SS)+(1 | SSBS)+(1 | Taxon_name_entered),
                    data = modelData,family = "binomial")

model_pest_clim <- glmer(formula = occur ~ LandUse+LUPesticide+poly(TEI_BL,2)+
                           poly(TEI_delta,2)+LandUse:poly(TEI_BL,2)+LandUse:poly(TEI_delta,2)+
                           LandUse:poly(TEI_BL,2):poly(TEI_delta,2)+LogElevation+ 
                           (1 | SS)+(1 | SSBS)+(1 | Taxon_name_entered),
                         data = modelData,family = "binomial")

print(cor.test(fixef(model_pest_clim)[c(1,2,4:20)],fixef(model_clim)))
print(cor.test(fitted(model_pest_clim),fitted(model_clim)))

png(filename = paste0(outDir,"PesticideResults.png"),width = 12.5,height = 11,units = "cm",res = 1200)

PlotGLMERFactor(model = model_pest,data = modelData,responseVar = "P. occ",logLink = "b",
                catEffects = "LUPesticide",seMultiplier = 1,order=c(1,5,2,3,4))

PlotGLMERContinuous(model = model_pest,data = modelData,effects = "LogPesticideApplication",
                    otherContEffects = "LogElevation",xlab = "Pesticide",ylab = "P. occ",
                    byFactor = "LandUse",line.cols = c("#66A61E","#D95F02"),logLink = "b",
                    seMultiplier = 1)

invisible(dev.off())

