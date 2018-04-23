suppressMessages(suppressWarnings(library(raster)))
suppressMessages(suppressWarnings(library(RColorBrewer)))

luDir <-  "5_ProjectModelsLandUse/"
luClimDir <- "7_ProjectModelsLandUseAndClimate/"

outDir <- "9_CompareProjections/"

luMap <- readRDS(paste(luDir,"2005MapLandUse.rds",sep=""))
luClimMap <- readRDS(paste(luClimDir,"2005MapLandUseAndClimate.rds",sep=""))

diffMap <- luClimMap - luMap

brks <- c(-1,-0.75,-0.5,-0.25,-0.1,-0.01,0.01,0.1111,1.33333,2,4,10)
brks[length(brks)] <- max(values(diffMap),na.rm=TRUE)
cols <- c(rev(brewer.pal(n = floor((length(brks)-2)/2),name = "Reds")),
         "#dddddd",
         brewer.pal(n = floor((length(brks)-2)/2),name = "Blues"))

png(filename = paste(outDir,"DifferenceMap.png",sep=""),
    width = 17.5,height = 8,units = "cm",res = 1200)

par(mar=c(0.2,0.2,0.2,6.5))

plot(diffMap,breaks=brks,col=cols,xlim=c(-180,40),ylim=c(10,78),
     xaxt="n",yaxt="n",legend=FALSE)

worse <- round((length(which(values(diffMap) <= -0.01))/length(
  which(!is.na(values(diffMap)))))*100,0)
better <- round((length(which(values(diffMap) >= 0.01))/length(
  which(!is.na(values(diffMap)))))*100,0)
no.change = 100 - worse - better

text(-180,45,paste(worse,"% worse"),pos=4)
text(-180,40,paste(no.change,"% the same"),pos=4)
text(-180,35,paste(better,"% better"),pos=4)

legend(x = 45,y = 80,
       legend = c("< -75%","-75 : -50%","-50 : -25%","-25 : -10%","-10 : -1%",
                  "-1 : +1%",
                  "+1 : +11%","+11 : +33%","+33 : +200%","+200 : +400%","> +400%"),
       xpd=TRUE,bty="n",
       fill=cols)

invisible(dev.off())