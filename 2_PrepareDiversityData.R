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

diversity <- diversity[(diversity$Best_guess_binomial!=""),]
diversity <- diversity[(diversity$Best_guess_binomial!="Unknown bombus"),]

diversity <- droplevels(diversity)

sp.names <- read.table(paste(dataDir,"bombus_stacknames.txt",sep=""))

TEI_bl <- readRDS(paste(dataDir,"BaselineTEI_Spp.rds",sep=""))
names(TEI_bl) <- paste("Bombus_",sp.names$x,sep="")
PEI_bl <- readRDS(paste(dataDir,"BaselinePEI_Spp.rds",sep=""))
names(PEI_bl) <- paste("Bombus_",sp.names$x,sep="")
TEI_delta <- readRDS(paste(dataDir,"DeltaTEI_Spp_Period3.rds",sep=""))
names(TEI_delta) <- paste("Bombus_",sp.names$x,sep="")
PEI_delta <- readRDS(paste(dataDir,"DeltaPEI_Spp_Period3.rds",sep=""))
names(PEI_delta) <- paste("Bombus_",sp.names$x,sep="")

diversity <- do.call('rbind',lapply(X = split(x = diversity,f = diversity$Best_guess_binomial),FUN = function(div.sp){
  
  sp <- gsub(" ","_",div.sp$Best_guess_binomial[1])
  
  if (sp %in% names(TEI_bl)){
    div.sp$TEI_BL <- raster::extract(x = TEI_bl[[sp]],y = div.sp[,c('Longitude','Latitude')])
  } else {
    div.sp$TEI_BL <- NA
  }
  
  if (sp %in% names(PEI_bl)){
    div.sp$PEI_BL <- raster::extract(x = PEI_bl[[sp]],y = div.sp[,c('Longitude','Latitude')])
  } else {
    div.sp$PEI_BL <- NA
  }
  
  if (sp %in% names(TEI_delta)){
    div.sp$TEI_delta <- raster::extract(x = TEI_delta[[sp]],y = div.sp[,c('Longitude','Latitude')])
  } else {
    div.sp$TEI_delta <- NA
  }
  
  if (sp %in% names(PEI_delta)){
    div.sp$PEI_delta <- raster::extract(x = PEI_delta[[sp]],y = div.sp[,c('Longitude','Latitude')])
  } else {
    div.sp$PEI_delta <- NA
  }
  
  return(div.sp)
}))

diversity$LandUse <- paste(diversity$Predominant_land_use)
diversity$LandUse[(diversity$LandUse=="Primary vegetation")] <- "Natural"
diversity$LandUse[(diversity$LandUse=="Mature secondary vegetation")] <- "Natural"
diversity$LandUse[(diversity$LandUse=="Intermediate secondary vegetation")] <- "Natural"
diversity$LandUse[(diversity$LandUse=="Young secondary vegetation")] <- "Natural"
diversity$LandUse[(diversity$LandUse=="Secondary vegetation (indeterminate age)")] <- "Natural"
diversity$LandUse[(diversity$LandUse=="Cropland")] <- "Human"
diversity$LandUse[(diversity$LandUse=="Pasture")] <- "Human"
diversity$LandUse[(diversity$LandUse=="Urban")] <- "Human"
diversity$LandUse[(diversity$LandUse=="Cannot decide")] <- NA
diversity$LandUse <- factor(diversity$LandUse,levels=c("Natural","Human"))

diversity$occur <- ifelse(diversity$Measurement>0,1,0)



cat('Saving diversity data\n')
save(diversity,file=paste(outDir,"diversity_data.Rd",sep=""))
