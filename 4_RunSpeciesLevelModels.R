suppressMessages(suppressWarnings(library(yarg)))
suppressMessages(suppressWarnings(library(StatisticalModels)))

inDir <- "1_PrepareDiversityData/"

outDir <- "4_RunSpeciesLevelModels/"

load(paste(inDir,"diversity_data.Rd",sep=""))

diversity$LandUse<-paste(diversity$Predominant_land_use)
diversity$LandUse[which(diversity$LandUse=="Primary vegetation")]<-"Natural"
diversity$LandUse[which(diversity$LandUse=="Mature secondary vegetation")]<-"Natural"
diversity$LandUse[which(diversity$LandUse=="Intermediate secondary vegetation")]<-"Natural"
diversity$LandUse[which(diversity$LandUse=="Young secondary vegetation")]<-"Natural"
diversity$LandUse[which(diversity$LandUse=="Secondary vegetation (indeterminate age)")]<-"Natural"
diversity$LandUse[which(diversity$LandUse=="Cropland")]<-"Human"
diversity$LandUse[which(diversity$LandUse=="Pasture")]<-"Human"
diversity$LandUse[which(diversity$LandUse=="Urban")]<-"Human"
diversity$LandUse[which(diversity$LandUse=="Cannot decide")]<-NA
diversity$LandUse<-factor(diversity$LandUse)
diversity$LandUse<-relevel(diversity$LandUse,ref="Natural")


diversity$UseIntensity<-paste(diversity$Use_intensity)
diversity$UseIntensity[which(diversity$Use_intensity=="Light use")]<-"Intense use"
diversity$UseIntensity[which(diversity$Use_intensity=="Cannot decide")]<-NA
diversity$UseIntensity<-factor(diversity$UseIntensity)
diversity$UseIntensity<-relevel(diversity$UseIntensity,ref="Minimal use")

diversity$UI<-paste(diversity$LandUse,diversity$UseIntensity)
diversity$UI[grep("NA",diversity$UI)]<-NA

diversity$UI<-factor(diversity$UI)
diversity$UI<-relevel(diversity$UI,ref="Natural Minimal use")

# diversity$Measurement <- diversity$Measurement.rs

# diversity$Abund <- round(diversity$Measurement*1000)

diversity$occur <- ifelse(diversity$Measurement>0,1,0)

diversityAbund <- droplevels(diversity[(diversity$Measurement>0),])
diversityAbund$LogAbund <- log(diversityAbund$Measurement)

mOccur <- GLMERSelect(modelData = diversity,responseVar = "occur",
                      fitFamily = "binomial",fixedFactors = c("LandUse","UseIntensity"),
                      fixedInteractions = "LandUse:UseIntensity",
                      randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")

mAbund <- GLMERSelect(modelData = diversityAbund,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedFactors = c("LandUse","UseIntensity"),
                      fixedInteractions = "LandUse:UseIntensity",
                      randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")
# 
# mAbund2 <- ModelSelectADMB(all.data = diversity,responseVar = "Abund",fitFamily = "nbinom",
#                            fixedFactors = c("LandUse","UseIntensity"),otherRandoms = 'Taxon_name_entered',
#                            fixedInteractions = "LandUse:UseIntensity",randomStruct = ~ (1|SS) + (1|SSBS) + (1|Taxon_name_entered))
