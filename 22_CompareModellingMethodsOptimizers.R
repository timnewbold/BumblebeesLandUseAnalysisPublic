suppressMessages(suppressWarnings(library(StatisticalModels)))
suppressMessages(suppressWarnings(library(glmmADMB)))

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

load(paste0(modelsDir,"TemperatureModels.rd"))

m_full_NM <- glmer(formula = occur ~ LandUse + poly(TEI_BL, 2) + poly(TEI_delta, 2) + 
                     LandUse:poly(TEI_BL, 2) + LandUse:poly(TEI_delta, 2) + 
                     LandUse:poly(TEI_BL, 2):poly(TEI_delta, 2) + LogElevation + 
                     (1 | SS) + (1 | SSBS) + (1 | Taxon_name_entered), data = modelData, 
                   family = "binomial", control = glmerControl(
                     optimizer = "Nelder_Mead"))

m_full_ADMB <- glmmadmb(formula = occur ~ LandUse + poly(TEI_BL, 2) + poly(TEI_delta, 2) + 
                          LandUse:poly(TEI_BL, 2) + LandUse:poly(TEI_delta, 2) + 
                          LandUse:poly(TEI_BL, 2):poly(TEI_delta, 2) + LogElevation + 
                          (1 | SS) + (1 | SSBS) + (1 | Taxon_name_entered),data = modelData,
                        family = "binomial")

print(cor.test(fixef(m_full$model),fixef(m_full_NM)))
print(cor.test(fixef(m_full$model),fixef(m_full_ADMB)))
