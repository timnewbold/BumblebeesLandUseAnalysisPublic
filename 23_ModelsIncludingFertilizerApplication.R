suppressMessages(suppressWarnings(library(StatisticalModels)))
suppressMessages(suppressWarnings(library(raster)))

dataDir <- "0_data/"
inDir <- "2_PrepareDiversityData/"
modelsDir <- "3_RunSpeciesLevelModels/"

outDir <- "23_ModelsIncludingFertilizerApplication/"

load(paste(inDir,"diversity_data.Rd",sep=""))

diversity$LogElevation <- log(diversity$Elevation+2)

modelData <- diversity[,c('occur','LandUse','TEI_BL',
                          'TEI_delta','LogElevation','SS','SSBS',
                          'Taxon_name_entered','Longitude','Latitude',
                          'Predominant_land_use')]
modelData <- na.omit(modelData)

fert.appl <- raster(paste0(dataDir,"FertilizerMap.tif"))

modelData$FertilizerApplication <- raster::extract(x = fert.appl,y = modelData[,c('Longitude','Latitude')])

modelData$FertilizerApplication[(modelData$FertilizerApplication==0)] <- NA

modelData <- na.omit(modelData)

modelData$LogFertilizerApplication <- log(modelData$FertilizerApplication)

load(paste0(modelsDir,"TemperatureModels.rd"))

model_lu_sub <- glmer(formula = occur ~ LandUse+LogElevation+ 
                        (1 | SS)+(1 | SSBS)+(1 | Taxon_name_entered),
                      data = modelData,family = "binomial")

model_fert <- glmer(formula = occur ~ LandUse+poly(LogFertilizerApplication,2)+
                      LandUse:poly(LogFertilizerApplication,2)+LogElevation+ 
                      (1 | SS)+(1 | SSBS)+(1 | Taxon_name_entered),
                    data = modelData,family = "binomial")

model_clim <- glmer(formula = occur ~ LandUse+poly(TEI_BL,2)+
                      poly(TEI_delta,2)+LandUse:poly(TEI_BL,2)+LandUse:poly(TEI_delta,2)+
                      LandUse:poly(TEI_BL,2):poly(TEI_delta,2)+LogElevation+ 
                      (1 | SS)+(1 | SSBS)+(1 | Taxon_name_entered),
                    data = modelData,family = "binomial")

model_fert_clim <- glmer(formula = occur ~ LandUse+poly(LogFertilizerApplication,2)+
                           LandUse:poly(LogFertilizerApplication,2)+poly(TEI_BL,2)+
                           poly(TEI_delta,2)+LandUse:poly(TEI_BL,2)+LandUse:poly(TEI_delta,2)+
                           LandUse:poly(TEI_BL,2):poly(TEI_delta,2)+LogElevation+ 
                           (1 | SS)+(1 | SSBS)+(1 | Taxon_name_entered),
                         data = modelData,family = "binomial")

print(cor.test(fixef(model_fert_clim)[c(1,2,5:9,12:23)],fixef(model_clim)))
print(cor.test(fitted(model_fert_clim),fitted(model_clim)))

png(filename = paste0(outDir,"FertilizerResults.png"),width = 12.5,height = 11,units = "cm",res = 1200)

PlotGLMERContinuous(model = model_fert,data = modelData,effects = "LogFertilizerApplication",
                    otherContEffects = "LogElevation",xlab = "Fertilizer",ylab = "P. occ",
                    byFactor = "LandUse",line.cols = c("#66A61E","#D95F02"),logLink = "b",
                    seMultiplier = 1)

invisible(dev.off())