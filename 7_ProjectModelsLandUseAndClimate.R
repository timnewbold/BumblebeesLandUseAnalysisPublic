suppressMessages(suppressWarnings(library(raster)))
suppressMessages(suppressWarnings(library(StatisticalModels)))
suppressMessages(suppressWarnings(library(dovedale)))

dataDir <- "0_data/"
modelsDir <- "3_RunSpeciesLevelModels/"
maskDir <- "5_ProjectModelsLandUse/"

if(Sys.info()['nodename']=='UCBTTNE-PC2'){
  RCPDir <- "F:/RCP data/"
} else if (Sys.info()['nodename']=='UCBTTNE-LT2') {
  RCPDir <- "C:/RCP data/"
} else {
  stop("Computer not recognized, so can't find data!")
}

outDir <- "7_ProjectModelsLandUseAndClimate/"

mask <- readRDS(paste(maskDir,"Mask.rds",sep=""))

load(paste(modelsDir,"TemperatureModels.rd",sep=""))

tei_bl <- readRDS(paste(dataDir,"BaselineTEI_Spp_RangesCut.rds",sep=""))
tei_delta <- readRDS(paste(dataDir,"DeltaTEI_Spp_Period3RangesCut.rds",sep=""))

pred_bl <- stack(mapply(function(bl,delta){
  
  sp <- bl
  values(sp)[!is.na(values(sp))] <- 1
  
  new_ras <- sp
  
  df <- data.frame(pres=values(sp))
  
  df$LandUse <- factor("Natural",levels=levels(m_full$data$LandUse))
  df$TEI_BL <- values(bl)
  df$TEI_delta <- values(delta)
  
  df[is.na(df)] <- NA
  
  df$occur <- 0
  
  non.na.row <- which(apply(df,1,function(r) all(!is.na(r))))
  
  df$pred <- NA
  df$pred[non.na.row] <- PredictGLMER(model = m_full$model,data = df,se.fit = FALSE)$y
  
  df$pred <- 1/(1+exp(-(df$pred)))
  
  df$pred <- df$pred * df$pres
  
  values(new_ras) <- df$pred
  
  return(new_ras)
  
},tei_bl@layers,tei_delta@layers))

ExtractRCP(wDir = RCPDir,zipName = "RCP data.zip",scenario = "HYDE",years = c(1500,2005))

lu_2005 <- get_rcp_grid(wdir = RCPDir,scenario = "HYDE",years = 2005)

natural_2005 <- raster(lu_2005$primary[[1]])+raster(lu_2005$secondary[[1]])
human_2005 <- raster(lu_2005$cropland[[1]])+raster(lu_2005$pasture[[1]])+raster(lu_2005$urban[[1]])+raster(lu_2005$plantation[[1]])

total <- natural_2005 + human_2005

values(natural_2005) <- values(natural_2005)/values(total)
values(human_2005) <- values(human_2005)/values(total)

pred_2005 <- stack(mapply(function(bl,delta){
  
  sp <- bl
  values(sp)[!is.na(values(sp))] <- 1
  
  new_ras <- sp
  
  df <- data.frame(pres=values(sp))
  
  df$LandUse <- factor("Natural",levels=levels(m_full$data$LandUse))
  df$TEI_BL <- values(bl)
  df$TEI_delta <- values(delta)
  
  df[is.na(df)] <- NA
  
  df$occur <- 0
  
  non.na.row <- which(apply(df,1,function(r) all(!is.na(r))))
  
  df$pred <- NA
  df$pred[non.na.row] <- PredictGLMER(model = m_full$model,data = df,se.fit = FALSE)$y
  
  df$pred <- 1/(1+exp(-(df$pred)))
  
  df$pred <- df$pred * df$pres
  
  values(new_ras) <- df$pred * values(natural_2005)
  
  
  df$LandUse <- factor("Human",levels=levels(m_full$data$LandUse))
  df$TEI_BL <- values(bl)
  df$TEI_delta <- values(delta)
  
  df[is.na(df)] <- NA
  
  df$occur <- 0
  
  non.na.row <- which(apply(df,1,function(r) all(!is.na(r))))
  
  df$pred <- NA
  df$pred[non.na.row] <- PredictGLMER(model = m_full$model,data = df,se.fit = FALSE)$y
  
  df$pred <- 1/(1+exp(-(df$pred)))
  
  df$pred <- df$pred * df$pres
  
  values(new_ras) <- values(new_ras) + (df$pred * values(human_2005))
  
  return(new_ras)
  
},tei_bl@layers,tei_delta@layers))

pred_2005_propn <- sum(pred_2005,na.rm=TRUE)/sum(pred_bl,na.rm=TRUE)

values(pred_2005_propn) <- values(pred_2005_propn) * values(mask)

brks <- c(0,0.85,0.9,0.95,0.975,1,1.025,1.05,1.1,1.15,9e99)

png(filename = paste(outDir,"2005MapLandUseAndClimate.png",sep=""),width = 17.5,height = 13.5,units = "cm",res = 1200)

plot(pred_2005_propn,breaks=brks,col=brewer.pal(n = length(brks),name = "RdYlBu"),
     xlim=c(-180,40),ylim=c(10,78))

text(-180,30,paste("Average intactness\n= ",round(mean(values(pred_2005_propn),na.rm=TRUE)*100,0),"%",sep=""))

invisible(dev.off())

saveRDS(object = pred_2005_propn,file = paste(outDir,"2005MapLandUseAndClimate.rds",sep=""))



