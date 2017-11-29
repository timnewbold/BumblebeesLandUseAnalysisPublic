suppressMessages(suppressWarnings(library(yarg)))

dataDir <- "0_data"
mapDir <- "1_PrepareMapData/"

divDir <- "2_PrepareDiversityData/"
outDir <- "3_PrepareSiteData/"

cat('Preparing site data\n')

load(paste(divDir,"diversity_data.Rd",sep=""))

# diversity$Measurement <- diversity$Measurement.rs

cat('Compiling site data\n')

sites.div<-SiteMetrics(diversity=diversity,
                       extra.cols=c("SSB","SSBS","Biome","Sampling_method",
                                    "Study_common_taxon","Sampling_effort",
                                    "Sampling_effort_unit","Realm",
                                    "Predominant_land_use","Class",
                                    "Country"),
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
# sites.div$LandUse[which(sites.div$LandUse=="Cropland")]<-"Human"
# sites.div$LandUse[which(sites.div$LandUse=="Pasture")]<-"Human"
# sites.div$LandUse[which(sites.div$LandUse=="Urban")]<-"Human"
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

cat('Adding other explanatory variables...\n')

wgsCRS <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
behrCRS <- CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs')

cat('...habitat diversity\n')

hd <- raster(paste(mapDir,"HabitatDiversity.tif",sep=""))
hd <- setExtent(x = hd,ext = extent(-17367530,17367530,-7342230,7342230))
hd <- projectRaster(from = hd,crs = wgsCRS,res = 0.05)
hd <- SpatialGridDataFrame(grid = GridTopology(cellcentre.offset = c(hd@extent@xmin+0.025,
                                                                     hd@extent@ymin+0.025),
                                               cellsize = c(0.05,0.05),
                                               cells.dim = c(ncol(hd),nrow(hd))),
                           data = data.frame(band1=values(hd)),
                           proj4string = wgsCRS)

sites.div <- AddGridData(gridData = hd,dataFrame = sites.div,
                         columnName = "habdiv",silent = TRUE)
rm(hd)
gc()

cat('...percent natural habitat\n')

pn <- raster(paste(mapDir,"PercentNatural.tif",sep=""))
pn <- setExtent(x = pn,ext = extent(-17367530,17367530,-7342230,7342230))
pn <- projectRaster(from = pn,crs = wgsCRS,res = 0.05)
pn <- SpatialGridDataFrame(grid = GridTopology(cellcentre.offset = c(pn@extent@xmin+0.025,
                                                                     pn@extent@ymin+0.025),
                                               cellsize = c(0.05,0.05),
                                               cells.dim = c(ncol(pn),nrow(pn))),
                           data = data.frame(band1=values(pn)),
                           proj4string = wgsCRS)

sites.div <- AddGridData(gridData = pn,dataFrame = sites.div,
                         columnName = "percnatural",silent = TRUE)
rm(pn)
gc()

cat('...temperature\n')

temp <- readGDAL(paste(dataDir,"/bio_1",sep=""),silent = TRUE)
sites.div <- AddGridData(gridData = temp,dataFrame = sites.div,columnName = "temp",silent = TRUE)
rm(temp)
gc()

cat('...precipitation\n')

precip <- readGDAL(paste(dataDir,"/bio_12",sep=""),silent = TRUE)
sites.div <- AddGridData(gridData = precip,dataFrame = sites.div,columnName = "precip",silent = TRUE)
rm(precip)
gc()

cat('...elevation\n')

elev <- readGDAL(paste(dataDir,"/alt",sep=""),silent = TRUE)
sites.div <- AddGridData(gridData = elev,dataFrame = sites.div,columnName = "elev",silent = TRUE)
rm(elev)
gc()

cat('...community thermal index\n')

tei <- raster(paste(dataDir,"/Bumblebees.asc",sep=""))
tei <- setExtent(x = tei,ext = extent(-17367530,17367530,-7342230,7342230))
crs(tei) <- behrCRS
tei <- projectRaster(from = tei,crs = wgsCRS,res = 0.1)
tei <- SpatialGridDataFrame(grid = GridTopology(cellcentre.offset = c(tei@extent@xmin+0.05,
                                                                      tei@extent@ymin+0.05),
                                               cellsize = c(0.1,0.1),
                                               cells.dim = c(ncol(tei),nrow(tei))),
                           data = data.frame(band1=values(tei)),
                           proj4string = wgsCRS)
sites.div <- AddGridData(gridData = tei,dataFrame = sites.div,columnName = "TEI",silent = TRUE)

cat('...pesticide application\n')

pest <- raster(paste(dataDir,"/PesticidesSum.tif",sep=""))
pest <- extend(pest,extent(-180,180,-90,90))
pest <- SpatialGridDataFrame(grid = GridTopology(cellcentre.offset = c(pest@extent@xmin+0.05,
                                                                     pest@extent@ymin+0.05),
                                               cellsize = c(0.1,0.1),
                                               cells.dim = c(ncol(pest),nrow(pest))),
                           data = data.frame(band1=values(pest)),
                           proj4string = wgsCRS)
sites.div <- AddGridData(gridData = pest,dataFrame = sites.div,columnName = "pesticide")


save(sites.div,file=paste(outDir,"modelling_data.Rd",sep=""))

cat('Plotting sites\n')

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



