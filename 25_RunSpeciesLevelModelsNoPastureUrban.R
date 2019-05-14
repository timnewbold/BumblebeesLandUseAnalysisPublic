suppressMessages(suppressWarnings(library(StatisticalModels)))

inDir <- "2_PrepareDiversityData/"

outDir <- "3_RunSpeciesLevelModels/"

load(paste(inDir,"diversity_data.Rd",sep=""))

diversity <- diversity[(
  diversity$Predominant_land_use != "Pasture"),]
diversity <- diversity[(
  diversity$Predominant_land_use != "Urban"),]

diversity$LogElevation <- log(diversity$Elevation+2)

modelData <- diversity[,c('occur','LandUse','TEI_BL',
                          'TEI_delta','LogElevation','SS','SSBS',
                          'Taxon_name_entered','Longitude','Latitude')]
modelData <- na.omit(modelData)

cat('Temperature models - null\n')

m_null <- GLMER(modelData = modelData,responseVar = "occur",
                fitFamily = "binomial",
                fixedStruct = "LogElevation",
                randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)",
                saveVars = c("Longitude","Latitude"))

cat('Temperature models - land use\n')

m_lu <- GLMER(modelData = modelData,responseVar = "occur",
              fitFamily = "binomial",
              fixedStruct = "LandUse+LogElevation",
              randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)",
              saveVars = c("Longitude","Latitude"))

cat('Temperature models - climate\n')

m_clim <- GLMER(modelData = modelData,responseVar = "occur",
                fitFamily = "binomial",
                fixedStruct = "poly(TEI_BL,2)+poly(TEI_delta,2)+poly(TEI_BL,2):poly(TEI_delta,2)+LogElevation",
                randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)",
                saveVars = c("Longitude","Latitude"))

cat('Temperature models - combined\n')

m_full <- GLMER(modelData = modelData,responseVar = "occur",
                fitFamily = "binomial",
                fixedStruct = "LandUse+poly(TEI_BL,2)+poly(TEI_delta,2)+LandUse:poly(TEI_BL,2)+LandUse:poly(TEI_delta,2)+LandUse:poly(TEI_BL,2):poly(TEI_delta,2)+LogElevation",
                randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)",
                saveVars = c("Longitude","Latitude"))

m_spat <- glmer(formula = occur~LandUse+
                  poly(Longitude,3)+poly(Latitude,3)+
                  LandUse:poly(Longitude,3)+LandUse:poly(Latitude,3)+
                  LandUse:poly(Longitude,3):poly(Latitude,3)+
                  (1|SS)+(1|SSBS)+(1|Taxon_name_entered),
                data = modelData,family = "binomial")

print(AIC(m_null$model,m_lu$model,m_clim$model,m_full$model,m_spat))

save(m_null,m_lu,m_clim,m_full,file = paste(outDir,"TemperatureModels.rd",sep=""))

rm(m_null,m_lu,m_clim,m_full)
