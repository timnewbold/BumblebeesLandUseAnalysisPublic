suppressMessages(suppressWarnings(library(yarg)))

dataDir <- "0_data/"
outDir <- "2_PrepareDiversityData/"

cat('Loading database extracts\n')
diversity<-readRDS(paste(dataDir,"database.rds",sep=""))

cat('Selecting appropriate data\n')
diversity <- diversity[apply(diversity[,c('Longitude','Latitude')],1,function(r) all(!is.na(r))),]

diversity <- droplevels(diversity[(diversity$Genus=="Bombus"),])

diversity <- droplevels(diversity[(diversity$UN_subregion %in% 
                                     c("Northern Europe","Southern Europe",
                                       "Western Europe","North America")),])

# Drop a study with uncertain permission
diversity<-diversity[(diversity$Source_ID!="VK1_2007__StLaurent"),]

# Drop some studies where the land-use classification is problematic
problem.studies<-c("AD1_2011__Hanley","AD1_2011__Yoon","VK1_2007__StLaurent",
                   "VK1_2013__ABMIboreal","SE1_2012__Rosselli","HW1_2008__Pillsbury",
                   "MG1_2008__Boutin","MG1_2011__Schon","MG1_2012__Schmidt",
                   "DI1_2013__Munyuli")
diversity<-diversity[!(diversity$Source_ID %in% problem.studies),]

# Drop studies that focused on a single species
diversity <- diversity[!diversity$Rank_of_study_common_taxon %in% c('Infraspecies','Species'),] 

cat('Correcting for sampling effort\n')
diversity <- CorrectSamplingEffort(diversity)

cat('Merging sites\n')
diversity <- MergeSites(diversity,public = TRUE,silent = TRUE)

diversity <- do.call('rbind',lapply(X = split(diversity,diversity$SS),FUN = function(s){
  s$Measurement.rs <- s$Measurement/max(s$Measurement)
  
  return(s)
}))

cat('Saving diversity data\n')
save(diversity,file=paste(outDir,"diversity_data.Rd",sep=""))