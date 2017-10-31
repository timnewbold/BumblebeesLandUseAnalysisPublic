suppressMessages(suppressWarnings(library(rgdal)))
suppressMessages(suppressWarnings(library(raster)))

dataDir <- "0_data/"

behrCRS <- CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs')

pri <- readGDAL(paste(
  dataDir,"pri_1km_int",sep=""),
  silent = TRUE)

nat <- pri

rm(pri)
gc()

sec <- readGDAL(paste(
  dataDir,"sec_1km_int",sep=""),
  silent = TRUE)

nat$band1 <- nat$band1 + sec$band1

rm(sec)
gc()

nat <- raster(nat)

nat <- projectRaster(from = nat,res = 1000,crs = behrCRS)

values(nat) <- round(values(nat))

toRas <- nat

step <- ncol(nat)/5

nBlocks <- ceiling(nrow(nat)/5)

for (block in 1:nBlocks){
  
  blockStart <- ((block-1) * step) + 1

  inds <- rep(blockStart:(blockStart+step - 1),each=5)
  
  if (block != nBlocks){
    inds <- rep(inds,5)
  } else {
    inds <- rep(inds,4)
  }
  
  if(block==1){
    blockVals <- inds
  } else {
    blockVals <- c(blockVals,inds)
  }
}

values(toRas) <- blockVals

test <- zonal(nat,toRas)
