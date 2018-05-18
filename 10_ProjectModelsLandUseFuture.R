suppressMessages(suppressWarnings(library(raster)))
suppressMessages(suppressWarnings(library(StatisticalModels)))
suppressMessages(suppressWarnings(library(dovedale)))

dataDir <- "0_data/"
modelsDir <- "3_RunSpeciesLevelModels/"

outDir <- "10_ProjectModelsLandUseFuture/"

if(Sys.info()['nodename']=='UCBTTNE-PC2'){
  RCPDir <- "F:/RCP data/"
} else if (Sys.info()['nodename']=='UCBTTNE-LT2') {
  RCPDir <- "C:/RCP data/"
} else {
  stop("Computer not recognized, so can't find data!")
}

scenarios <- c("26","85")
scenario.labels <- c("IMAGE","MESSAGE")

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

invisible(mapply(function(scenario,scenario.label){
  
  cat(paste('Projecting future scenario ',scenario.label,' ',scenario,'\n',sep=''))
  
  ExtractRCP(wDir = RCPDir,zipName = "RCP data.zip",scenario = scenario.label,years = c(2070))
  
  lu_2070 <- get_rcp_grid(wdir = RCPDir,scenario = scenario.label,years = 2070)
  
  natural_2070 <- raster(lu_2070$primary[[1]])+raster(lu_2070$secondary[[1]])
  human_2070 <- raster(lu_2070$cropland[[1]])+raster(lu_2070$pasture[[1]])+raster(lu_2070$urban[[1]])+raster(lu_2070$plantation[[1]])
  
  total <- natural_2070 + human_2070
  
  values(natural_2070) <- values(natural_2070)/values(total)
  values(human_2070) <- values(human_2070)/values(total)
  
  mask <- natural_2070 + human_2070
  values(mask)[!is.na(values(mask))] <- 1
  
  pred_2070 <- stack(lapply(bombus.ranges@layers,function(sp){
    
    new_ras <- sp
    
    df <- data.frame(pres=values(sp))
    
    df$LandUse <- factor("Natural",levels=levels(m_lu$data$LandUse))
    df$occur <- 0
    
    df$pred <- PredictGLMER(model = m_lu$model,data = df,se.fit = FALSE)$y
    
    df$pred <- 1/(1+exp(-(df$pred)))
    
    df$pred <- df$pred * df$pres
    
    values(new_ras) <- df$pred * values(natural_2070)
    
    df$LandUse <- factor("Human",levels=levels(m_lu$data$LandUse))
    df$occur <- 0
    
    df$pred <- PredictGLMER(model = m_lu$model,data = df,se.fit = FALSE)$y
    
    df$pred <- 1/(1+exp(-(df$pred)))
    
    df$pred <- df$pred * df$pres
    
    values(new_ras) <- values(new_ras) + (df$pred * values(human_2070))
    
    return(new_ras)
    
  }))
  
  pred_2070_propn <- sum(pred_2070,na.rm=TRUE)/sum(pred_bl,na.rm=TRUE)
  
  values(pred_2070_propn) <- values(pred_2070_propn) * values(mask)
  
  brks <- c(0,0.85,0.9,0.95,0.975,1.025,1.05,1.1,1.15,9e99)
  brks[length(brks)] <- max(1.151,max(values(pred_2070_propn),na.rm=TRUE))
  brks[1] <- min(0.849,min(values(pred_2070_propn),na.rm=TRUE))
  cols <- c(rev(brewer.pal(n = floor((length(brks)-2)/2),name = "Reds")),
            "#dddddd",
            brewer.pal(n = floor((length(brks)-2)/2),name = "Blues"))
  
  png(filename = paste(outDir,"2070MapLandUseScenario",scenario,".png",sep=""),
      width = 17.5,height = 8,units = "cm",res = 1200)
  
  par(mar=c(0.2,0.2,0.2,6.5))
  
  plot(pred_2070_propn,breaks=brks,col=cols,
       xlim=c(-180,40),ylim=c(10,78),xaxt="n",yaxt="n",legend=FALSE)
  
  text(-160,30,paste("Average\nintactness\n= ",
                     round(mean(values(pred_2070_propn),na.rm=TRUE)*100,0),"%",sep=""))
  
  legend(x = 45,y = 80,
         legend = c("< 85%","85 - 90%","90 - 95%","95 - 97.5%",
                    "97.5 - 102.5%",
                    "102.5 - 105%","105 - 110%","110 - 115%","> 115%"),
         xpd=TRUE,bty="n",
         fill=cols)
  
  invisible(dev.off())
  
  saveRDS(object = pred_2070_propn,file = paste(
    outDir,"2070MapLandUseScenario",scenario,".rds",sep=""))
  saveRDS(object = mask,file = paste(outDir,"Mask.rds",sep=""))
  
},scenarios,scenario.labels))




