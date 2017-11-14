suppressMessages(suppressWarnings(library(rgdal)))
suppressMessages(suppressWarnings(library(raster)))

dataDir <- "0_data/"

outDir <- "1_PrepareMapData/"

behrCRS <- CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs')

cat('% natural habitat\n')

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

cat('Splitting map\n')

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

cat('Calculating values\n')

zm <- zonal(nat,toRas)

e <- extent(nat)
e@ymin <- e@ymin-1000

newRas <- raster(nrows=nBlocks,ncols=step,ext=e,crs=behrCRS)
values(newRas) <- zm[,2]

writeRaster(x = newRas,filename = paste(outDir,"PercentNatural.tif",sep=""),format="GTiff")

rm(newRas,blockVals,nat,zm)
gc()

cat('Habitat diversity\n')

clc.files <- dir(paste(dataDir,"consensuslandcover",sep=""))

clc.map <- raster(paste(dataDir,"consensuslandcover/consensus_full_class_1.tif",sep=""))
clc.map <- projectRaster(from = clc.map,res = 1000,crs = behrCRS)
values(clc.map) <- round(values(clc.map))

toRas <- clc.map

step <- ncol(clc.map)/5

nBlocks <- ceiling(nrow(clc.map)/5)

cat('Splitting map\n')

for (block in 1:nBlocks){
  
  print(block)
  
  blockStart <- ((block-1) * step) + 1
  
  inds <- rep(blockStart:(blockStart+step - 1),each=5)
  
  if (block != nBlocks){
    inds <- rep(inds,5)
  } else {
    inds <- rep(inds,1)
  }
  
  if(block==1){
    blockVals <- inds
  } else {
    blockVals <- c(blockVals,inds)
  }
}

values(toRas) <- blockVals

rm(blockVals,clc.map,inds)
gc()

clc.vals <- array(data = NA,dim = c(12,25,18671963))

cat('Processing land-cover data\n')

for(i in 1:length(clc.files)){
  
  cat('Processing map',i,'\n')
  
  map <- raster(paste(dataDir,"consensuslandcover/",clc.files[i],sep=""))
  
  map <- projectRaster(from = map,res = 1000,crs = behrCRS)
  
  values(map) <- round(values(map))
  
  vals <- do.call('cbind',split(values(map),values(toRas)))
  
  clc.vals[i,,] <- vals
  
  rm(vals)
  
  gc()
}

e <- extent(toRas)
e@ymin <- e@ymin-4000

newRas <- raster(nrows=nBlocks,ncols=step,ext=e,crs=behrCRS)

cat('Calculating values\n')

values(newRas) <- apply(X = clc.vals,MARGIN = 3,FUN = function(x){
  
  x <- x/100
  
  y <- apply(X = x,MARGIN = 1,FUN = mean,na.rm = TRUE)
  
  si <- -sum(y[y>0] * log(y[y>0]))
  
  return(si)
})

writeRaster(x = newRas,filename = paste(outDir,"HabitatDiversity.tif",sep=""),format="GTiff")

