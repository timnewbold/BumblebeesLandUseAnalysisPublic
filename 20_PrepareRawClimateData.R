suppressMessages(suppressWarnings(library(abind)))
suppressMessages(suppressWarnings(library(raster)))

climateDataDir <- "ProcessTemporalClimate/"

outDir <- "20_PrepareRawClimateData/"

bl.yrs <- 1:74
final.yrs <- 100:115

yr.summary <- function(i,func,data){
  
  months <- (((i-1)*12)+1):(((i-1)*12)+12)
  
  yr.data <- data[,,months]
  
  yr.summ <- apply(X = yr.data,MARGIN = c(1,2),FUN = func)
  
  return(yr.summ)
}

cat('Processing maximum temperature\n')

max.temp.data <- readRDS(paste0(climateDataDir,"MaxTempData.rds"))

yr.max.temp.bl <- do.call(what = 'abind',args = list(sapply(
  X = bl.yrs,FUN = yr.summary,func=max,data=max.temp.data,simplify = FALSE),
  along=3))

yr.max.temp.final <- do.call(what = 'abind',args = list(sapply(
  X = final.yrs,FUN = yr.summary,func=max,data=max.temp.data,simplify = FALSE),
  along=3))

max.temp.bl <- apply(X = yr.max.temp.bl,MARGIN = c(1,2),FUN = mean)
max.temp.final <- apply(X = yr.max.temp.final,MARGIN = c(1,2),FUN = mean)

max.temp.delta <- max.temp.final - max.temp.bl

max.temp.bl.map <- raster(t(max.temp.bl[,ncol(max.temp.bl):1]),
                          xmn=-180,xmx=180,ymn=-90,ymx=90)
max.temp.delta.map <- raster(t(max.temp.delta[,ncol(max.temp.delta):1]),
                             xmn=-180,xmx=180,ymn=-90,ymx=90)

writeRaster(x = max.temp.bl.map,filename = paste0(outDir,"MaxTempBL.tif"),format='GTiff')
writeRaster(x = max.temp.delta.map,filename = paste0(outDir,"MaxTempDelta.tif"),format='GTiff')

cat('Annual precipitation\n')

precip.data <- readRDS(paste0(climateDataDir,"PrecipitationData.rds"))

yr.precip.bl <- do.call(what = 'abind',args = list(sapply(
  X = bl.yrs,FUN = yr.summary,func=sum,data=precip.data,simplify=FALSE),
  along=3))

yr.precip.final <- do.call(what = 'abind',args = list(sapply(
  X = final.yrs,FUN = yr.summary,func=sum,data=precip.data,simplify=FALSE),
  along=3))

precip.bl <- apply(X = yr.precip.bl,MARGIN = c(1,2),FUN = mean)
precip.final <- apply(X = yr.precip.final,MARGIN = c(1,2),FUN = mean)

precip.delta <- precip.final - precip.bl

precip.bl.map <- raster(t(precip.bl[,ncol(precip.bl):1]),
                        xmn=-180,xmx=180,ymn=-90,ymx=90)
precip.delta.map <- raster(t(precip.delta[,ncol(precip.delta):1]),
                           xmn=-180,xmx=180,ymn=-90,ymx=90)

writeRaster(x = precip.bl.map,filename = paste0(outDir,"AnnPrecipBL.tif"),format='GTiff')
writeRaster(x = precip.delta.map,filename = paste0(outDir,"AnnPrecipDelta.tif"),format='GTiff')


