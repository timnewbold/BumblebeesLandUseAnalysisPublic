suppressMessages(suppressWarnings(library(raster)))
suppressMessages(suppressWarnings(library(StatisticalModels)))
suppressMessages(suppressWarnings(library(dovedale)))

dataDir <- "0_data/"
modelsDir <- "3_RunSpeciesLevelModels/"

outDir <- "5_ProjectModelsLandUse/"

if(Sys.info()['nodename']=='UCBTTNE-PC2'){
  RCPDir <- "F:/RCP data/"
} else if (Sys.info()['nodename']=='UCBTTNE-LT2') {
  RCPDir <- "C:/RCP data/"
} else {
  stop("Computer not recognized, so can't find data!")
}

load(paste(modelsDir,"TemperatureModels.rd",sep=""))

bombus.ranges <- stack(paste(dataDir,"bombus_0.tif",sep=""))

wgsCRS <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

bombus.ranges <- stack(lapply(bombus.ranges@layers,function(sp){
  crs(sp) <- wgsCRS
  
  return(sp)
}))

pred_bl <- stack(lapply(bombus.ranges@layers,function(sp){
  
  new_ras <- sp
  
  df <- data.frame(pres=values(sp))
  
  df$LandUse <- factor("Natural",levels=levels(m_lu$data$LandUse))
  df$occur <- 0
  
  df$pred <- PredictGLMER(model = m_lu$model,data = df,se.fit = FALSE)$y
  
  df$pred <- 1/(1+exp(-(df$pred)))

  df$pred <- df$pred * df$pres
  
  values(new_ras) <- df$pred
  
  return(new_ras)
  
}))

ExtractRCP(wDir = RCPDir,zipName = "RCP data.zip",scenario = "HYDE",years = c(1500,2005))

lu_2005 <- get_rcp_grid(wdir = RCPDir,scenario = "HYDE",years = 2005)

natural_2005 <- raster(lu_2005$primary[[1]])+raster(lu_2005$secondary[[1]])
human_2005 <- raster(lu_2005$cropland[[1]])+raster(lu_2005$pasture[[1]])+raster(lu_2005$urban[[1]])+raster(lu_2005$plantation[[1]])

total <- natural_2005 + human_2005

values(natural_2005) <- values(natural_2005)/values(total)
values(human_2005) <- values(human_2005)/values(total)

mask <- natural_2005 + human_2005
values(mask)[!is.na(values(mask))] <- 1
 
un_sub <- raster(paste0(dataDir,"un_subregions"))
values(mask)[!(values(un_sub) %in% c(21,39,154,155))] <- NA

pred_2005 <- stack(lapply(bombus.ranges@layers,function(sp){
  
  new_ras <- sp
  
  df <- data.frame(pres=values(sp))
  
  df$LandUse <- factor("Natural",levels=levels(m_lu$data$LandUse))
  df$occur <- 0
  
  df$pred <- PredictGLMER(model = m_lu$model,data = df,se.fit = FALSE)$y
  
  df$pred <- 1/(1+exp(-(df$pred)))
  
  df$pred <- df$pred * df$pres
  
  values(new_ras) <- df$pred * values(natural_2005)
  
  df$LandUse <- factor("Human",levels=levels(m_lu$data$LandUse))
  df$occur <- 0
  
  df$pred <- PredictGLMER(model = m_lu$model,data = df,se.fit = FALSE)$y
  
  df$pred <- 1/(1+exp(-(df$pred)))
  
  df$pred <- df$pred * df$pres
  
  values(new_ras) <- values(new_ras) + (df$pred * values(human_2005))
  
  return(new_ras)
  
}))

pred_bl_rich <- sum(pred_bl,na.rm=TRUE)*mask
# pred_bl_rich[values(pred_bl_rich)<1] <- NA

pred_2005_rich <- sum(pred_2005,na.rm=TRUE)*mask

pred_2005_propn <- pred_2005_rich/pred_bl_rich

values(pred_2005_propn) <- values(pred_2005_propn) * values(mask)

brks <- c(0,0.5,0.75,0.95,0.975,1.025,1.05,1.25,1.5,9e99)
brks[length(brks)] <- max(1.151,max(values(pred_2005_propn),na.rm=TRUE))
brks[1] <- min(0.849,min(values(pred_2005_propn),na.rm=TRUE))
cols <- c(rev(brewer.pal(n = floor((length(brks)-2)/2),name = "Reds")),
          "#dddddd",
          brewer.pal(n = floor((length(brks)-2)/2),name = "Blues"))

tiff(filename = paste(outDir,"2005MapLandUse.tif",sep=""),
    width = 17.5,height = 8,units = "cm",res = 300,compression = "lzw")

par(mar=c(0.2,0.2,0.2,6.5))

plot(pred_2005_propn,breaks=brks,col=cols,
     xlim=c(-180,40),ylim=c(10,78),xaxt="n",yaxt="n",legend=FALSE)

text(-160,30,paste("Average\nintactness\n= ",
                   round(mean(values(pred_2005_propn),na.rm=TRUE)*100,0),"%",sep=""))

legend(x = 45,y = 80,
       legend = c("< 50%","50 - 75%","75 - 95%","95 - 97.5%",
                  "97.5 - 102.5%",
                  "102.5 - 105%","105 - 125%","125 - 150%","> 150%"),
       xpd=TRUE,bty="n",
       fill=cols)

invisible(dev.off())

saveRDS(object = pred_2005_propn,file = paste(outDir,"2005MapLandUse.rds",sep=""))
saveRDS(object = mask,file = paste(outDir,"Mask.rds",sep=""))


