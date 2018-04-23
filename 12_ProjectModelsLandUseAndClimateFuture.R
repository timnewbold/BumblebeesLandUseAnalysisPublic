suppressMessages(suppressWarnings(library(raster)))
suppressMessages(suppressWarnings(library(StatisticalModels)))
suppressMessages(suppressWarnings(library(dovedale)))

dataDir <- "0_data/"
modelsDir <- "3_RunSpeciesLevelModels/"
maskDir <- "10_ProjectModelsLandUseFuture/"

if(Sys.info()['nodename']=='UCBTTNE-PC2'){
  RCPDir <- "F:/RCP data/"
} else if (Sys.info()['nodename']=='UCBTTNE-LT2') {
  RCPDir <- "C:/RCP data/"
} else {
  stop("Computer not recognized, so can't find data!")
}

outDir <- "12_ProjectModelsLandUseAndClimateFuture/"

scenarios <- c("26","85")
scenario.labels <- c("IMAGE","MESSAGE")

mask <- readRDS(paste(maskDir,"Mask.rds",sep=""))

load(paste(modelsDir,"TemperatureModels.rd",sep=""))

tei_bl <- readRDS(paste(dataDir,"BaselineTEI_Spp_RangesCut.rds",sep=""))

invisible(mapply(function(scenario,scenario.label){
  
  cat(paste('Projecting future scenario ',scenario.label,' ',scenario,'\n',sep=''))
  
  tei_delta <- readRDS(paste(dataDir,"DeltaTEI_Spp_",scenario,"_2070.rds",sep=""))
  
  pred_bl <- stack(mapply(function(bl,delta){
    
    sp <- bl
    values(sp)[!is.na(values(sp))] <- 1
    
    new_ras <- sp
    
    df <- data.frame(pres=values(sp))
    
    df$LandUse <- factor("Natural",levels=levels(m_full$data$LandUse))
    df$TEI_BL <- values(bl)
    df$TEI_delta <- values(delta)
    
    df[is.na(df)] <- NA
    # 
    #   df$TEI_BL[which(df$TEI_BL < 0.58)] <- NA
    #   df$TEI_BL[which(df$TEI_BL > 0.74)] <- NA
    #   df$TEI_delta[which(df$TEI_delta < 0.005)] <- NA
    #   df$TEI_delta[which(df$TEI_delta > 0.035)] <- NA
    # 
    # df$TEI_BL[which(df$TEI_BL < 0.5)] <- NA
    # df$TEI_BL[which(df$TEI_BL > 0.8)] <- NA
    # df$TEI_delta[which(df$TEI_delta < -0.01)] <- NA
    # df$TEI_delta[which(df$TEI_delta > 0.04)] <- NA
    
    
    df$occur <- 0
    
    non.na.row <- which(apply(df,1,function(r) all(!is.na(r))))
    
    df$pred <- NA
    df$pred[non.na.row] <- PredictGLMER(model = m_full$model,data = df,se.fit = FALSE)$y
    
    df$pred <- 1/(1+exp(-(df$pred)))
    
    df$pred <- df$pred * df$pres
    
    values(new_ras) <- df$pred
    
    return(new_ras)
    
  },tei_bl@layers,tei_delta@layers))
  
  ExtractRCP(wDir = RCPDir,zipName = "RCP data.zip",scenario = scenario.label,years = c(2070))
  
  lu_2070 <- get_rcp_grid(wdir = RCPDir,scenario = scenario.label,years = 2070)
  
  natural_2070 <- raster(lu_2070$primary[[1]])+raster(lu_2070$secondary[[1]])
  human_2070 <- raster(lu_2070$cropland[[1]])+raster(lu_2070$pasture[[1]])+
    raster(lu_2070$urban[[1]])+raster(lu_2070$plantation[[1]])
  
  total <- natural_2070 + human_2070
  
  values(natural_2070) <- values(natural_2070)/values(total)
  values(human_2070) <- values(human_2070)/values(total)
  
  mask <- natural_2070 + human_2070
  values(mask)[!is.na(values(mask))] <- 1
  
  pred_2070 <- stack(mapply(function(bl,delta){
    
    sp <- bl
    values(sp)[!is.na(values(sp))] <- 1
    
    new_ras <- sp
    
    df <- data.frame(pres=values(sp))
    
    df$LandUse <- factor("Natural",levels=levels(m_full$data$LandUse))
    df$TEI_BL <- values(bl)
    df$TEI_delta <- values(delta)
    
    df[is.na(df)] <- NA
    # 
    #   df$TEI_BL[which(df$TEI_BL < 0.5)] <- NA
    #   df$TEI_BL[which(df$TEI_BL > 0.8)] <- NA
    #   df$TEI_delta[which(df$TEI_delta < -0.01)] <- NA
    #   df$TEI_delta[which(df$TEI_delta > 0.04)] <- NA
    
    df$occur <- 0
    
    non.na.row <- which(apply(df,1,function(r) all(!is.na(r))))
    
    df$pred <- NA
    df$pred[non.na.row] <- PredictGLMER(model = m_full$model,data = df,se.fit = FALSE)$y
    
    df$pred <- 1/(1+exp(-(df$pred)))
    
    df$pred <- df$pred * df$pres
    
    values(new_ras) <- df$pred * values(natural_2070)
    
    
    df$LandUse <- factor("Human",levels=levels(m_full$data$LandUse))
    df$TEI_BL <- values(bl)
    df$TEI_delta <- values(delta)
    # 
    #   df$TEI_BL[which(df$TEI_BL < 0.5)] <- NA
    #   df$TEI_BL[which(df$TEI_BL > 0.8)] <- NA
    #   df$TEI_delta[which(df$TEI_delta < -0.01)] <- NA
    #   df$TEI_delta[which(df$TEI_delta > 0.04)] <- NA
    
    df[is.na(df)] <- NA
    
    df$occur <- 0
    
    non.na.row <- which(apply(df,1,function(r) all(!is.na(r))))
    
    df$pred <- NA
    df$pred[non.na.row] <- PredictGLMER(model = m_full$model,data = df,se.fit = FALSE)$y
    
    df$pred <- 1/(1+exp(-(df$pred)))
    
    df$pred <- df$pred * df$pres
    
    values(new_ras) <- values(new_ras) + (df$pred * values(human_2070))
    
    return(new_ras)
    
  },tei_bl@layers,tei_delta@layers))
  
  pred_2070_propn <- sum(pred_2070,na.rm=TRUE)/sum(pred_bl,na.rm=TRUE)
  
  values(pred_2070_propn) <- values(pred_2070_propn) * values(mask)
  
  brks <- c(0,0.85,0.9,0.95,0.975,1.025,1.05,1.1,1.15,9e99)
  brks[length(brks)] <- max(1.151,max(values(pred_2070_propn),na.rm=TRUE))
  brks[1] <- min(0.849,min(values(pred_2070_propn),na.rm=TRUE))
  cols <- c(rev(brewer.pal(n = floor((length(brks)-2)/2),name = "Reds")),
            "#dddddd",
            brewer.pal(n = floor((length(brks)-2)/2),name = "Blues"))
  
  png(filename = paste(outDir,"2070MapLandUseAndClimateScenario",scenario,".png",sep=""),
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
  
  saveRDS(object = pred_2070_propn,file = paste(outDir,"2070MapLandUseAndClimateScenario",
                                                scenario,".rds",sep=""))
  
},scenarios,scenario.labels))








