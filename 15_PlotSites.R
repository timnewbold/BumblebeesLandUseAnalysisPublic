suppressMessages(suppressWarnings(library(rgdal)))

dataDir <- "0_data"
divDir <- "2_PrepareDiversityData/"

outDir <- "15_PlotSites/"

load(paste0(divDir,"diversity_data.rd"))

sample_years <- as.integer(sub("-[0-9]{2}-[0-9]{2}","",diversity$Sample_end_latest))
cat(paste0("Records collected between ",min(sample_years)," and ",max(sample_years)))

sites <- diversity[,c('SSBS','LandUse','Longitude','Latitude')]

sites <- unique(sites)

cat(paste0('Total site number: ',nrow(sites)))
print(table(sites$LandUse))

un_sub <- readOGR(dsn = dataDir,layer = "UN_subregion",verbose = FALSE)
study_area <- un_sub[un_sub$SUBREGION %in% c(21,39,154,155),]

tiff(filename = paste0(outDir,"MapOfSites.tif"),
     width = 17.5,height = 9,units = "cm",
     compression = "lzw",res = 300)

par(mar=c(0,0,0,0))
par(ps=10)

plot(study_area,xlim=c(-171,31),ylim=c(25,84),border=NA,col="#cccccc")

sites$SiteCol <- factor(x = sites$LandUse,
                        levels = c("Natural","Human"),
                        labels = c("#66A61E99","#d95f0299"))

points(sites$Longitude,sites$Latitude,pch=16,col=paste(sites$SiteCol))

legend(-175,40,c("Natural habitat","Human land use"),
       pch=16,col=c("#66A61E","#d95f02"),bty="n")

invisible(dev.off())
