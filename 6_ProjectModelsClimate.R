suppressMessages(suppressWarnings(library(raster)))
suppressMessages(suppressWarnings(library(StatisticalModels)))

dataDir <- "0_data/"
modelsDir <- "3_RunSpeciesLevelModels/"
maskDir <- "5_ProjectModelsLandUse/"

outDir <- "6_ProjectModelsClimate/"

mask <- readRDS(paste(maskDir,"Mask.rds",sep=""))

load(paste(modelsDir,"TemperatureModels.rd",sep=""))

tei_bl <- readRDS(paste(dataDir,"BaselineTEI_Spp_RangesCut.rds",sep=""))
tei_delta <- readRDS(paste(dataDir,"DeltaTEI_Spp_Period3RangesCut.rds",sep=""))

pred_bl <- stack(mapply(function(bl,delta){
  
  sp <- bl
  values(sp)[!is.na(values(sp))] <- 1
  
  new_ras <- sp
  
  df <- data.frame(pres=values(sp))
  
  df$TEI_BL <- values(bl)
  df$TEI_delta <- 0
  
  df[is.na(df)] <- NA

  df$occur <- 0
  
  non.na.row <- which(apply(df,1,function(r) all(!is.na(r))))
  
  df$pred <- NA
  df$pred[non.na.row] <- PredictGLMER(model = m_clim$model,data = df,se.fit = FALSE)$y
  
  df$pred <- 1/(1+exp(-(df$pred)))
  
  df$pred <- df$pred * df$pres
  
  values(new_ras) <- df$pred
  
  return(new_ras)
  
},tei_bl@layers,tei_delta@layers))

pred_2005 <- stack(mapply(function(bl,delta){
  
  sp <- bl
  values(sp)[!is.na(values(sp))] <- 1
  
  new_ras <- sp
  
  df <- data.frame(pres=values(sp))
  
  df$TEI_BL <- values(bl)
  df$TEI_delta <- values(delta)
  
  df[is.na(df)] <- NA
  
  df$occur <- 0
  
  non.na.row <- which(apply(df,1,function(r) all(!is.na(r))))
  
  df$pred <- NA
  df$pred[non.na.row] <- PredictGLMER(model = m_clim$model,data = df,se.fit = FALSE)$y
  
  df$pred <- 1/(1+exp(-(df$pred)))
  
  df$pred <- df$pred * df$pres
  
  values(new_ras) <- df$pred
  
  return(new_ras)
  
},tei_bl@layers,tei_delta@layers))

pred_2005_propn <- sum(pred_2005,na.rm=TRUE)/sum(pred_bl,na.rm=TRUE)

values(pred_2005_propn) <- values(pred_2005_propn) * values(mask)

brks <- c(0,0.85,0.9,0.95,0.975,1,1.025,1.05,1.1,1.15,9e99)
brks[length(brks)] <- max(1.151,max(values(pred_2005_propn),na.rm=TRUE))
brks[1] <- min(0.849,min(values(pred_2005_propn),na.rm=TRUE))

png(filename = paste(outDir,"2005MapClimate.png",sep=""),width = 17.5,
    height = 8,units = "cm",res = 1200)

par(mar=c(0.5,0.5,0.5,4))

plot(pred_2005_propn,breaks=brks,col=brewer.pal(n = length(brks),name = "RdYlBu"),
     xlim=c(-180,40),ylim=c(10,78),xaxt="n",yaxt="n")

invisible(dev.off())

saveRDS(object = pred_2005_propn,file = paste(outDir,"2005MapClimate.rds",sep=""))



