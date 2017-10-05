suppressMessages(suppressWarnings(library(roquefort)))

dataDir <- "2_PrepareSiteData/"

outDir <- "3_RunModels/"

load(paste(dataDir,"modelling_data.Rd",sep=""))

sites.div$LogAbund <- log(sites.div$Total_abundance+1)
sites.div$LogSimpson <- log(sites.div$Simpson_diversity)

rc.sr <- CompareRandoms(dataset = sites.div,responseVar = "Species_richness",
                        fitFamily = "poisson",fixedFactors = c("UI"),
                        siteRandom = TRUE,blockRandom = FALSE,verbose = TRUE,
                        optimizer = "bobyqa",randomSlopes = FALSE)

model.sr <- ModelSelect(all.data = sites.div,responseVar = "Species_richness",
                        fitFamily = "poisson",fixedFactors = c("UI"),
                        randomStruct = rc.sr$best.random,verbose = TRUE,
                        optimizer = "bobyqa")

rc.ta <- CompareRandoms(dataset = sites.div,responseVar = "LogAbund",
                        fitFamily = "gaussian",fixedFactors = c("UI"),
                        siteRandom = FALSE,blockRandom = FALSE,verbose = TRUE,
                        optimizer = "bobyqa",randomSlopes = FALSE)

model.ta <- ModelSelect(all.data = sites.div,responseVar = "LogAbund",
                        fitFamily = "gaussian",fixedFactors = c("UI"),
                        randomStruct = rc.sr$best.random,verbose = TRUE,
                        optimizer = "bobyqa")

rc.sd <- CompareRandoms(dataset = sites.div,responseVar = "LogSimpson",
                        fitFamily = "gaussian",fixedFactors = c("UI"),
                        siteRandom = FALSE,blockRandom = FALSE,verbose = TRUE,
                        optimizer = "bobyqa",randomSlopes = FALSE)

model.sd <- ModelSelect(all.data = sites.div,responseVar = "LogSimpson",
                        fitFamily = "gaussian",fixedFactors = c("UI"),
                        randomStruct = rc.sr$best.random,verbose = TRUE,
                        optimizer = "bobyqa")

saveRDS(object = model.sr,file = paste(outDir,"modelRichness.rds",sep=""))
saveRDS(object = model.ta,file = paste(outDir,"modelAbundance.rds",sep=""))

png(filename = paste(outDir,"IntensityEffectAbundance.png",sep=""),width = 12.5,height = 8,units = "cm",res = 1200)

PlotErrBar(model = model.ta$model,data = model.ta$data,
           responseVar = "Abundance",logLink = "e",
           catEffects = "UseIntensity",forPaper = TRUE)

invisible(dev.off())
