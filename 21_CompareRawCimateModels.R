suppressMessages(suppressWarnings(library(StatisticalModels)))
suppressMessages(suppressWarnings(library(raster)))

inDir <- "2_PrepareDiversityData/"
modelsDir <- "3_RunSpeciesLevelModels/"
climDir <- "20_PrepareRawClimateData/"

outDir <- "21_CompareRawCimateModels/"

load(paste(inDir,"diversity_data.Rd",sep=""))

diversity$LogElevation <- log(diversity$Elevation+2)

modelData <- diversity[,c('occur','LandUse','TEI_BL',
                          'TEI_delta','LogElevation','SS','SSBS',
                          'Taxon_name_entered','Longitude','Latitude')]
modelData <- na.omit(modelData)

max.temp.bl <- raster(paste0(climDir,"MaxTempBL.tif"))
max.temp.delta <- raster(paste0(climDir,"MaxTempDelta.tif"))
precip.bl <- raster(paste0(climDir,"AnnPrecipBL.tif"))
precip.delta <- raster(paste0(climDir,"AnnPrecipDelta.tif"))

modelData$MaxTempBL <- raster::extract(x = max.temp.bl,y = modelData[,c('Longitude','Latitude')])
modelData$MaxTempDelta <- raster::extract(x = max.temp.delta,y = modelData[,c('Longitude','Latitude')])
modelData$PrecipBL <- raster::extract(x = precip.bl,y = modelData[,c('Longitude','Latitude')])
modelData$PrecipDelta <- raster::extract(x = precip.delta,y = modelData[,c('Longitude','Latitude')])

load(paste0(modelsDir,"TemperatureModels.rd"))

m_raw_clim <- glmer(formula = occur ~ LandUse + poly(MaxTempBL, 2) + poly(MaxTempDelta, 2) + 
                      LandUse:poly(MaxTempBL, 2) + LandUse:poly(MaxTempDelta, 2) + 
                      LandUse:poly(MaxTempBL, 2):poly(MaxTempDelta, 2) + LogElevation + 
                      (1 | SS) + (1 | SSBS) + (1 | Taxon_name_entered), data = modelData, 
                    family = "binomial")

cat("AIC - original TEI model:\n")
cat(paste0(round(AIC(m_full$model),1),"\n"))

cat("AIC - raw climate model:\n")
cat(paste0(round(AIC(m_raw_clim),1),"\n"))

cat("AIC difference:\n")
cat(paste0(round(AIC(m_raw_clim) - AIC(m_full$model),1),"\n"))
