suppressMessages(suppressWarnings(library(yarg)))

dataDir <- "0_data"

inDir <- "1_PrepareDiversityData/"

outDir <- "2_PrepareSiteData/"

load(paste(inDir,"diversity_data.Rd",sep=""))

# diversity$Measurement <- diversity$Measurement.rs

sites.div<-SiteMetrics(diversity=diversity,
                       extra.cols=c("SSB","SSBS","Biome","Sampling_method",
                                    "Study_common_taxon","Sampling_effort",
                                    "Sampling_effort_unit","Realm",
                                    "Predominant_land_use","Class"),
                       sites.are.unique=TRUE,
                       srEstimators=FALSE)

write.csv(sites.div[,c('SSS','Longitude','Latitude')],
          file=paste(outDir,"sites.csv",sep=""),
          row.names=FALSE)

cat('Arranging land-use and intensity classification\n')

sites.div$LandUse<-paste(sites.div$Predominant_land_use)
sites.div$LandUse[which(sites.div$LandUse=="Primary vegetation")]<-"Natural"
sites.div$LandUse[which(sites.div$LandUse=="Mature secondary vegetation")]<-"Natural"
sites.div$LandUse[which(sites.div$LandUse=="Intermediate secondary vegetation")]<-"Natural"
sites.div$LandUse[which(sites.div$LandUse=="Young secondary vegetation")]<-"Natural"
sites.div$LandUse[which(sites.div$LandUse=="Secondary vegetation (indeterminate age)")]<-"Natural"
sites.div$LandUse[which(sites.div$LandUse=="Cropland")]<-"Human"
sites.div$LandUse[which(sites.div$LandUse=="Pasture")]<-"Human"
sites.div$LandUse[which(sites.div$LandUse=="Urban")]<-"Human"
sites.div$LandUse[which(sites.div$LandUse=="Cannot decide")]<-NA
sites.div$LandUse<-factor(sites.div$LandUse)
sites.div$LandUse<-relevel(sites.div$LandUse,ref="Natural")


sites.div$UseIntensity<-paste(sites.div$Use_intensity)
sites.div$UseIntensity[which(sites.div$Use_intensity=="Light use")]<-"Intense use"
sites.div$UseIntensity[which(sites.div$Use_intensity=="Cannot decide")]<-NA
sites.div$UseIntensity<-factor(sites.div$UseIntensity)
sites.div$UseIntensity<-relevel(sites.div$UseIntensity,ref="Minimal use")

sites.div$UI<-paste(sites.div$LandUse,sites.div$UseIntensity)
sites.div$UI[grep("NA",sites.div$UI)]<-NA

sites.div$UI<-factor(sites.div$UI)
sites.div$UI<-relevel(sites.div$UI,ref="Natural Minimal use")

save(sites.div,file=paste(outDir,"modelling_data.Rd",sep=""))

sitesMap <- SpatialPointsDataFrame(coords = data.frame(x=sites.div$Longitude,y=sites.div$Latitude),
                                 data = sites.div,
                                 proj4string = CRS(
                                   "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

un_sub <- readOGR(dsn = dataDir,layer = "UN_subregion",verbose = FALSE)

un_sub <- un_sub[(un_sub$SUBREGION %in% c(21,13,154,155,39)),]

cols <- colorRampPalette(colors = c("#000000","#ffffff"))(5)

png(filename = paste(outDir,"MapSites.png",sep=""),width = 17.5,height = 8,units = "cm",res = 1200)

par(mar=c(0,0,0,0))

plot(un_sub,col=cols,xlim=c(-180,30))

plot(sitesMap,add=TRUE,pch=16,cex=0.5,col="#cc0000")

invisible(dev.off())



