
dataDir <- "0_data/"
modelDataDir <- "2_PrepareDiversityData/"

outDir <- "16_ReferencesPreprocess/"

bib <- readRDS(paste(dataDir,"bib-2015-09-29-02-34-28.rds",sep=""))

load(paste(modelDataDir,"diversity_data.Rd",sep=""))

bib <- bib[(bib$Source_ID %in% diversity$Source_ID),]

write.csv(bib,file=paste(outDir,"bib.csv",sep=""),row.names=FALSE)