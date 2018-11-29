suppressMessages(suppressWarnings(library(RColorBrewer)))
suppressMessages(suppressWarnings(library(rgdal)))

source("../MapPollinatorDependentCrops/ReadKleinPollinatorData.R")
source("../MapPollinatorDependentCrops/MapPollinationDependentCropYields.R")

dataDir <- "0_data"
maskDir <- "5_ProjectModelsLandUse/"

outDir <- "18_MapBumblebeeDependentCrops/"

computer <- Sys.info()['nodename']

if (computer == "UCBTTNE-PC2"){
  monfreda.dir <- "E:/Dropbox/BiodiversityFoodProduction/HarvestedAreaYield175Crops_NetCDF/"
} else if (computer == "UCBTTNE-LT2"){
  monfreda.dir <- "C:/Users/tim_n/Dropbox/BiodiversityFoodProduction/HarvestedAreaYield175Crops_NetCDF/"
} else {
  stop("Computer not recognized")
}

mask <- readRDS(paste(maskDir,"Mask.rds",sep=""))

klein.path <- paste0(dataDir,"/data_cleaned.csv")

klein.data <- ReadKleinPollinatorData(path = klein.path)

prod.map.bumblebees <- MapPollinationDependentCropYields(
  yield.dir = monfreda.dir,klein.data = klein.data,
  percentage = FALSE,importance = 'modest',
  taxon.group='Bumblebees',exclude.crop.groups = TRUE)

prod.map.bumblebees <- raster::aggregate(x = prod.map.bumblebees,fact=6,fun=sum)

values(prod.map.bumblebees)[values(prod.map.bumblebees)==0] <- NA

values(prod.map.bumblebees)[is.na(values(mask))] <- NA

yield.map <- prod.map.bumblebees/(area(prod.map.bumblebees)*100)

brks <- c(0,5e-5,1e-4,1e-3,5e-3,1e-2,2e-2,5e-2,1e-1,5e-1,2.5)
cols <- rev(brewer.pal(n = length(brks)-1,name = "RdYlBu"))

un_sub <- readOGR(dsn = dataDir,layer = "UN_subregion",verbose = FALSE)
study_region <- un_sub[(un_sub$SUBREGION %in% c(39,154,155,21)),]

tiff(filename = paste0(outDir,"ProductionMapFigure.tif"),
     width = 17.5,height = 8,units = "cm",res = 300,compression = "lzw")

par(mar=c(0.2,0.2,0.2,6.5))

plot(yield.map,breaks=brks,col=cols,xlim=c(-180,40),ylim=c(10,78),xaxt="n",yaxt="n",
     legend=FALSE,box=FALSE,bty="n")

plot(study_region,col="#aaaaaa",border=NA,add=TRUE)

plot(yield.map,breaks=brks,col=cols,xlim=c(-180,40),ylim=c(10,78),xaxt="n",yaxt="n",
     legend=FALSE,box=FALSE,bty="n",add=TRUE)

legend(x = 45,y = 80,title="Production \n(kg per ha)",
       legend = c("< 0.05","0.05 - 0.1","0.1 - 1","1 - 5",
                  "5 - 10","10 - 20","20 - 50",
                  "50 - 100","100 - 200","> 500"),
       xpd=TRUE,bty="n",
       fill=cols)

invisible(dev.off())

writeRaster(x = prod.map.bumblebees,filename = paste0(outDir,"ProductionMap.tif"),format="GTiff")

prod.map.bumblebees.pc <- MapPollinationDependentCropYields(
  yield.dir = monfreda.dir,klein.data = klein.data,
  percentage = TRUE,importance = 'modest',
  taxon.group='Bumblebees',exclude.crop.groups = TRUE)

prod.map.bumblebees.pc <- raster::aggregate(x = prod.map.bumblebees.pc,fact=6,fun=mean)

values(prod.map.bumblebees.pc)[values(prod.map.bumblebees.pc)==0] <- NA

values(prod.map.bumblebees.pc)[is.na(values(mask))] <- NA

brks <- c(0,0.1,0.25,0.5,1,2.5,5,7.5,10,20,100)
cols <- rev(brewer.pal(n = length(brks)-1,name = "RdYlBu"))

tiff(filename = paste0(outDir,"ProductionMapPercentageFigure.tif"),
     width = 17.5,height = 8,units = "cm",res = 300,compression = "lzw")

par(mar=c(0.2,0.2,0.2,6.5))

plot(prod.map.bumblebees.pc,breaks=brks,col=cols,xlim=c(-180,40),ylim=c(10,78),xaxt="n",yaxt="n",legend=FALSE)

legend(x = 45,y = 80,title="Production (% of \nall crop production)",
       legend = c("< 0.1","0.1 - 0.25","0.25 - 0.5","0.5 - 1",
                  "1 - 2.5","2.5 - 5","5 - 7.5","7.5 - 10",
                  "10 - 20","20 - 100"),
       xpd=TRUE,bty="n",
       fill=cols)

invisible(dev.off())

writeRaster(x = prod.map.bumblebees.pc,filename = paste0(outDir,"ProductionMapPercentage.tif"),format="GTiff")

