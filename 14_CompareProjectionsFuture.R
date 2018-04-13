suppressMessages(suppressWarnings(library(raster)))
suppressMessages(suppressWarnings(library(RColorBrewer)))

luDir <-  "10_ProjectModelsLandUseFuture/"
luClimDir <- "12_ProjectModelsLandUseAndClimateFuture/"

outDir <- "14_CompareProjectionsFuture/"

luMap <- readRDS(paste(luDir,"2070MapLandUse.rds",sep=""))
luClimMap <- readRDS(paste(luClimDir,"2070MapLandUseAndClimate.rds",sep=""))

diffMap <- luClimMap - luMap

brks <- c(-1,-0.75,-0.5,-0.25,-0.1,-0.01,0.01,0.1111,1.33333,2,4,10)
brks[length(brks)] <- max(values(diffMap),na.rm=TRUE)
cols <- c(rev(brewer.pal(n = floor((length(brks)-1)/2),name = "Reds")),
         "#dddddd",
         brewer.pal(n = floor((length(brks)-1)/2),name = "Blues"))



png(filename = paste(outDir,"DifferenceMap.png",sep=""),
    width = 17.5,height = 8,units = "cm",res = 1200)

par(mar=c(0.5,0.5,0.5,4))

plot(diffMap,breaks=brks,col=cols,xlim=c(-180,40),ylim=c(10,78),xaxt="n",yaxt="n")

worse <- round((length(which(values(diffMap) <= -0.01))/length(
  which(!is.na(values(diffMap)))))*100,0)
better <- round((length(which(values(diffMap) >= 0.01))/length(
  which(!is.na(values(diffMap)))))*100,0)
no.change = 100 - worse - better

text(-180,45,paste(worse,"% worse"),pos=4)
text(-180,40,paste(no.change,"% the same"),pos=4)
text(-180,35,paste(better,"% better"),pos=4)

invisible(dev.off())