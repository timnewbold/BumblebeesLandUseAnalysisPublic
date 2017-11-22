suppressMessages(suppressWarnings(library(roquefort)))

dataDir <- "3_PrepareSiteData/"

outDir <- "4_RunModels/"

load(paste(dataDir,"modelling_data.Rd",sep=""))

sites.div$LogAbund <- log(sites.div$Total_abundance+1)
sites.div$LogSimpson <- log(sites.div$Simpson_diversity)

sites.div$LogPesticide <- log(sites.div$pesticide)
sites.div$PesticideCut <- cut(sites.div$pesticide,breaks=c(-1,0,4.748017,53.42977,3010),labels=c("None","Low","Medium","High"))

sites.div$LU_Pest <- paste(sites.div$LandUse,sites.div$PesticideCut)
sites.div$LU_Pest[grep("NA",sites.div$LU_Pest)] <- NA
sites.div$LU_Pest[grep("Natural",sites.div$LU_Pest)] <- "Natural"
sites.div$LU_Pest[(sites.div$LU_Pest=="Cropland None")] <- "Cropland Low"
sites.div$LU_Pest[grep("Urban",sites.div$LU_Pest)] <- "Urban"

# 
# rc.sr <- CompareRandoms(dataset = sites.div,responseVar = "Species_richness",
#                         fitFamily = "poisson",fixedFactors = c("UI"),
#                         fixedTerms = list(temp=2,precip=2,elev=2,habdiv=2,percnatural=2),
#                         fixedInteractions = c("LandUse:habdiv",
#                                               "LandUse:percnatural",
#                                               "habdiv:percnatural"),
#                         siteRandom = TRUE,blockRandom = FALSE,verbose = TRUE,
#                         optimizer = "bobyqa",randomSlopes = FALSE)

model.sr <- ModelSelect(all.data = sites.div,responseVar = "Species_richness",
                        fitFamily = "poisson",fixedFactors = c("UI"),
                        fixedTerms = list(habdiv=1,percnatural=1,temp=1,precip=1,elev=1),
                        fixedInteractions = c("LandUse:poly(habdiv,1)",
                                              "LandUse:poly(percnatural,1)",
                                              "poly(habdiv,1):poly(percnatural,1)"),
                        randomStruct = "(1|SS)+(1|SSBS)",verbose = TRUE,
                        optimizer = "bobyqa")

# rc.ta <- CompareRandoms(dataset = sites.div,responseVar = "LogAbund",
#                         fitFamily = "gaussian",fixedFactors = c("UI"),
#                         siteRandom = FALSE,blockRandom = FALSE,verbose = TRUE,
#                         optimizer = "bobyqa",randomSlopes = FALSE)

model.ta <- ModelSelect(all.data = sites.div,responseVar = "LogAbund",
                        fitFamily = "gaussian",fixedFactors = c("UI"),
                        fixedTerms = list(habdiv=1,percnatural=1,temp=1,precip=1,elev=1),
                        fixedInteractions = c("LandUse:poly(habdiv,1)",
                                              "LandUse:poly(percnatural,1)",
                                              "poly(habdiv,1):poly(percnatural,1)"),
                        randomStruct = "(1|SS)",verbose = TRUE,
                        optimizer = "bobyqa")
# 
# rc.sd <- CompareRandoms(dataset = sites.div,responseVar = "LogSimpson",
#                         fitFamily = "gaussian",fixedFactors = c("UI"),
#                         siteRandom = FALSE,blockRandom = FALSE,verbose = TRUE,
#                         optimizer = "bobyqa",randomSlopes = FALSE)
# 
# model.sd <- ModelSelect(all.data = sites.div,responseVar = "LogSimpson",
#                         fitFamily = "gaussian",fixedFactors = c("UI"),
#                         randomStruct = rc.sr$best.random,verbose = TRUE,
#                         optimizer = "bobyqa")

saveRDS(object = model.sr,file = paste(outDir,"modelRichness.rds",sep=""))
saveRDS(object = model.ta,file = paste(outDir,"modelAbundance.rds",sep=""))

png(filename = paste(outDir,"Results.png",sep=""),width = 12.5,height = 22,
    units = "cm",res = 1200)

par(mfrow=c(3,1))

PlotErrBar(model = model.ta$model,data = model.ta$data,responseVar = "Abundance",
           logLink = "e",catEffects = "UI",order=c(7,8,1,2,3,4,5,6),forPaper=TRUE,
           seMultiplier = 1)

PlotContEffects(model = model.ta$model,data = model.ta$data,effects = "percnatural",
                otherContEffects = c("precip","elev"),byFactor = "LandUse",
                xlab = "% natural",ylab = "Abundance",seMultiplier = 1,
                otherFactors = list(UI="Primary vegetation Minimal use"))

PlotContEffects(model = model.sr$model,data = model.sr$data,effects = "percnatural",
                otherContEffects = c("precip"),byFactor = "LandUse",xlab = "% natural",
                ylab = "Species richness",seMultiplier = 1)

invisible(dev.off())

model.data <- sites.div[,c('Species_richness','LandUse','PesticideCut','LU_Pest','SS','SSBS'),]
model.data <- na.omit(model.data)
model.data$LU_Pest <- factor(model.data$LU_Pest)
model.data$LU_Pest <- relevel(model.data$LU_Pest,ref="Natural")

m3 <- glmer(Species_richness~LU_Pest+(1|SS)+(1|SSBS),data=model.data,family=poisson)

png(filename = paste(outDir,"PesticideResults.png",sep=""),width = 12.5,height = 8,
    units = "cm",res = 1200)

PlotErrBar(model=m3,data=model.data,responseVar = "SR",logLink = "e",
           catEffects = "LU_Pest",seMultiplier = 1,order=c(1,3,4,2,5,6),
           errbar.cols = c("#66A61E",rep("#E6AB02",3), "#D95F02","#E7298A"))

invisible(dev.off())

