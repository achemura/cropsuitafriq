
gc()

library(geodata)
library(Recocrop)
library(raster)
library(rgdal)
library(terra)
library(rasterVis)
library(dplyr)
library(rgbif)

library(docstring)
library(ggplot2)

setwd("~/DATABANK/Diversity/")

#SHP
admin <- readOGR("SHP/Africa.shp")
hollow <- readOGR("SHP/Afrihollow.shp")
grids <- readOGR("SHP/graticules_30.shp")
water <- readOGR("~/DATABANK/EastAfrica/Africa/africa_water_bodies.shp")
africa <- readOGR("SHP/Africa_conti.shp")

#SHP
admin <- readOGR("SHP/Africa.shp")
hollow <- readOGR("SHP/Afrihollow.shp")
grids <- readOGR("SHP/graticules_30.shp")
water <- readOGR("~/DATABANK/EastAfrica/Africa/africa_water_bodies.shp")

soilph  <- rast("Layers/Alt/soilph_is3.tif")
crs(soilph) <- "+proj=longlat +datum=WGS84 +no_defs"

prec <- terra::rast(stack(list.files(path="Layers/W5E5/Prec/",pattern=".tif$",full.names=T)))
crs(prec) <- "+proj=longlat +datum=WGS84 +no_defs"

tavg <- terra::rast(stack(list.files(path="Layers/W5E5/Tavg/",pattern=".tif$",full.names=T)))
crs(tavg) <- "+proj=longlat +datum=WGS84 +no_defs"

tmin <- terra::rast(stack(list.files(path="Layers/W5E5/Tmin/",pattern=".tif$",full.names=T)))
crs(tmin) <- "+proj=longlat +datum=WGS84 +no_defs"

#Create an ecocrop model for maize
maize <- ecocropPars("Maize")
maize <- Recocrop::ecocrop(maize)
control(maize, get_max=TRUE)
maize.suit <- predict(maize, ktmp=tmin, ktmp=tmin, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="Maize"))
maize.ras <- raster(maize.suit)

#Plot
cols2 <- colorRampPalette(c("grey90", "salmon3", "palegreen2", "forestgreen"))

levelplot(maize.ras, col.regions=cols2, main="Maize", ylab="Lattitude",
          colorkey=list(labels=list(cex=0.9), space="bottom",width= 0.9,height = 0.5), axes = FALSE, margin = F) +
  latticeExtra::layer(sp.lines(grids, col='grey15', lty=2, alpha=0.6, lwd= 1.2)) +
  latticeExtra::layer(sp.polygons(admin, lwd= 0.2, fill='transparent')) +
  latticeExtra::layer(sp.polygons(hollow, lwd = 0.01,  fill = "white"))

#Pearl millet
pmillet <- ecocropPars("Pearl millet")
pmillet <- Recocrop::ecocrop(pmillet)
control(pmillet, get_max=TRUE)
pmillet.suit <- predict(pmillet, ktmp=tmin, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="Pearl millet"))
pmillet.ras <- raster(pmillet.suit)

#Finger millet
fmillet <- ecocropPars("Finger millet")
fmillet$parameters[,4] <- c(20, 40, 2000, 2400)
fmillet <- Recocrop::ecocrop(fmillet)
control(fmillet, get_max=TRUE)
fmillet.suit <- predict(fmillet, ktmp=tmin, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="Finger millet"))
fmillet.ras <- raster(fmillet.suit)

#Sorghum
sorghum <- ecocropPars("Sorghum (low altitude)")
sorghum <- Recocrop::ecocrop(sorghum)
control(sorghum, get_max=TRUE)
sorghum.suit <- predict(sorghum, ktmp=tmin, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="Sorghum"))
sorghum.ras <- raster(sorghum.suit)

#Sorghum
price <- ecocropPars("Rice, paddy (Indica)")
price$parameters[,4] <- c(120, 160, 480, 1091)
price <- Recocrop::ecocrop(price)
control(price, get_max=TRUE)
price.suit <- predict(price, ktmp=tmin, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="Rice"))
price.ras <- raster(price.suit)
plot(price.ras)
#uprice-Upland
uprice <- ecocropPars("Rice upland (Indica)")
uprice <- Recocrop::ecocrop(uprice)
control(uprice, get_max=TRUE)
uprice.suit <- predict(uprice, ktmp=tmin, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="uprice"))
uprice.ras <- raster(uprice.suit)

#Combiner rice to one
rice.ras <- max(price.ras, uprice.ras)

#wheat
cwheat <- ecocropPars("Wheat, common")
cwheat <- Recocrop::ecocrop(cwheat)
control(cwheat, get_max=TRUE)
cwheat.suit <- predict(cwheat, ktmp=tmin, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="cwheat"))
cwheat.ras <- raster(cwheat.suit)

#dwheat
dwheat <- ecocropPars("Wheat, durum")
dwheat$parameters[,4] <- c(40, 80, 200, 400)
dwheat <- Recocrop::ecocrop(dwheat)
control(dwheat, get_max=TRUE)
dwheat.suit <- predict(dwheat, ktmp=tmin, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="dwheat"))
dwheat.ras <- raster(dwheat.suit)
wheat.ras <- max(cwheat.ras, dwheat.ras)

#Teff
teff <- ecocropPars("Teff")
teff <- Recocrop::ecocrop(teff)
control(teff, get_max=TRUE)
teff.suit <- predict(teff, ktmp=tmin, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="teff"))
teff.ras <- raster(teff.suit)

#fonio
fonio <- ecocropPars("Hungry rice")
#fonio$parameters[,3] <- c(18, 22, 32, 33)
fonio$parameters[,4] <- c(46, 86, 505, 1050)
fonio <- Recocrop::ecocrop(fonio)
control(fonio, get_max=TRUE)
fonio.suit <- predict(fonio, ktmp=tmin, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="fonio"))
fonio.ras <- raster(fonio.suit)


#Stack all suitabilities
crops <- stack(maize.ras, pmillet.ras, fmillet.ras, sorghum.ras, 
               rice.ras, wheat.ras, teff.ras, fonio.ras)
#
writeRaster(maize.ras, paste0(paste0(getwd(),"/Output/Cereals/Suit/Current/"),"maize_ras.tif"), overwrite=TRUE)
writeRaster(pmillet.ras, paste0(paste0(getwd(),"/Output/Cereals/Suit/Current/"),"pmillet_ras.tif"), overwrite=TRUE)
writeRaster(fmillet.ras, paste0(paste0(getwd(),"/Output/Cereals/Suit/Current/"),"fmillet_ras.tif"), overwrite=TRUE)
writeRaster(sorghum.ras, paste0(paste0(getwd(),"/Output/Cereals/Suit/Current/"),"sorghum_ras.tif"), overwrite=TRUE)
writeRaster(rice.ras, paste0(paste0(getwd(),"/Output/Cereals/Suit/Current/"),"rice_ras.tif"), overwrite=TRUE)
writeRaster(wheat.ras, paste0(paste0(getwd(),"/Output/Cereals/Suit/Current/"),"wheat_ras.tif"), overwrite=TRUE)
writeRaster(teff.ras, paste0(paste0(getwd(),"/Output/Cereals/Suit/Current/"),"teff_ras.tif"), overwrite=TRUE)
writeRaster(fonio.ras, paste0(paste0(getwd(),"/Output/Cereals/Suit/Current/"),"fonio_ras.tif"), overwrite=TRUE)

#Classification of suitabilitz based on threshold
class.names <- c("Suit", "Unsuit") 

# #Save rasters

cer_names <- c("maize, pmillet, fmillet, sorghum, rice, wheat, teff, fonio")
scenarios <- c("ssp126", "ssp370", "ssp585")
gcms <- c("gfdl", "cane", "cnrc", "cnrm", "ipsl", "mpie", "mrie", "miro", "erth", "uksm")

#####______________________________________________________________________________________________________________________________________________________________
#list directories in the Layers
f = base::list.dirs("~/DATABANK/Layers",recursive = F)

# making twice a loop to list directories in the subdirectories to avoid main directories in the list
df = list()
for (i in f){
  f1 =base::list.dirs(i,recursive = F)
  df = as.character(append(df,f1))
}

df1 = list()
for (i in df){
  f1 =base::list.dirs(i,recursive = F)
  df1 = as.character(append(df1,f1))
}

path = "~/DATABANK/Ethiopia/Coffee/outputs/outs/"
path1 = "~/DATABANK/Ethiopia/Coffee/outputs/outs_ratify/"
for (i in df1){
  #getting the file names from folder using substr 
  nms = substr(i,60,nchar(i))
  #replacing "/" to avoid unnecssary path creation to "_"
  nms1 = paste0(gsub("/","_",nms),".tif")
  #list ascii files in the folder 
  gcms = list.files(i,pattern = ".asc$", full.names=TRUE)
  #stck all ascii files
  gcm_stack = stack(gcms)
  # #subset only required files from the above stack
  gcm_stack_subset <- subset(gcm_stack, all.layers, drop=T)
  print(paste0("Estimating for the ", nms))
  out <- ensemble(sdm.model, newdata = gcm_stack_subset, filename = paste0(path,nms1),overwrite=TRUE,
                  setting=list(method='weighted', stat='AUC'))
  out.cl <- reclassify(out, class_vec)
  out.cl.rat <- ratify(out.cl)
  writeRaster(out.cl.rat,paste0(path1,nms1),overwrite=T)
  print(paste0("Ratify writing completed for ",nms1))
}


###### _______________________________________________________________________________________________________________

all.crop.suit.cl <- stack(cereals.allnow, cereals.all26, cereals.all70, cereals.all85)
#
# #Area calcuations
names.admin <- as.data.frame(africa@data)

tot.px <- as.data.frame(table(getValues(rasterize(africa, maize.ras))))
tot.px
#
# #Area calculation
proj4string(all.crop.suit.cl) = proj4string(africa)
all.crop.suit.r = rasterToPoints(all.crop.suit.cl, spatial = T)
all.crop.suit.r.pts = sf::st_as_sf(all.crop.suit.r)
all.crop.sf.shpa = sf::st_as_sf(africa)
all.crop.join = sf::st_join(all.crop.suit.r.pts,all.crop.sf.shpa)
all.crop.join = as.data.frame(all.crop.join)
head(all.crop.join)
all.crop.df1 = all.crop.join[ , -which(names(all.crop.join) %in% c("Id","geometry"))]
head(all.crop.df1)
# #
# all.crop.df1.sum <- as.data.frame(colSums(all.crop.df1))
# colnames(all.crop.df1.sum) <- "Suit_px"
# all.crop.df1.sum$Per <-(all.crop.df1.sum$Suit_px/tot.px$Freq) * 100
source(sprintf("%sHelper_area_calc_afriq.R", "~/DATABANK/RScripts/Bioclim/Ecocrop/Diversity/"))
#
all.crop.df1.sum<- ras2area(all.crop.suit.cl)
head(all.crop.df1.sum)
all.crop.df1.sum$Period <- c(rep("Curr", 8), rep("S126", 8), rep("S370", 8), rep("S585", 8))
all.crop.df1.sum$Crop <- rep(c("Maize", "Pmillet", "Fmillet", "Sorghum", "Rice", "Wheat", "Teff", "Fonio"), 4)
head(all.crop.df1.sum)
#
# #Plot
ggplot(all.crop.df1.sum, aes(x = Crop, y = Per, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") + ylim(0, 60) +
  #scale_fill_manual(values = c("palegreen", "salmon")) +
  labs(x="Crop and Scenario", y = "Suitable area [%]")
#
# #Extract GCm results for S126
#Maize S126
maize126.gcm.sum <- ras2area(maize126.gcm)
colnames(maize126.gcm.sum)[2] <- c("maize126")
sorghum126.gcm.sum <- ras2area(sorghum126.gcm)
colnames(sorghum126.gcm.sum)[2] <- c("sorghum126")
pmillet126.gcm.sum <- ras2area(pmillet126.gcm)
colnames(pmillet126.gcm.sum)[2] <- c("pmillet126")
fmillet126.gcm.sum <- ras2area(fmillet126.gcm)
colnames(fmillet126.gcm.sum)[2] <- c("fmillet126")
rice126.gcm.sum <- ras2area(rice126.gcm)
colnames(rice126.gcm.sum)[2] <- c("rice126")
wheat126.gcm.sum <- ras2area(wheat126.gcm)
colnames(wheat126.gcm.sum)[2] <- c("wheat126")
teff126.gcm.sum <- ras2area(teff126.gcm)
colnames(teff126.gcm.sum)[2] <- c("teff126")
fonio126.gcm.sum <- ras2area(fonio126.gcm)
colnames(fonio126.gcm.sum)[2] <- c("fonio126")
#
maize370.gcm.sum <- ras2area(maize370.gcm)
colnames(maize370.gcm.sum)[2] <- c("maize370")
sorghum370.gcm.sum <- ras2area(sorghum370.gcm)
colnames(sorghum370.gcm.sum)[2] <- c("sorghum370")
pmillet370.gcm.sum <- ras2area(pmillet370.gcm)
colnames(pmillet370.gcm.sum)[2] <- c("pmillet370")
fmillet370.gcm.sum <- ras2area(fmillet370.gcm)
colnames(fmillet370.gcm.sum)[2] <- c("fmillet370")
rice370.gcm.sum <- ras2area(rice370.gcm)
colnames(rice370.gcm.sum)[2] <- c("rice370")
wheat370.gcm.sum <- ras2area(wheat370.gcm)
colnames(wheat370.gcm.sum)[2] <- c("wheat370")
teff370.gcm.sum <- ras2area(teff370.gcm)
colnames(teff370.gcm.sum)[2] <- c("teff370")
fonio370.gcm.sum <- ras2area(fonio370.gcm)
# colnames(fonio370.gcm.sum)[2] <- c("fonio370")
#
maize585.gcm.sum <- ras2area(maize585.gcm)
colnames(maize585.gcm.sum)[2] <- c("maize585")
sorghum585.gcm.sum <- ras2area(sorghum585.gcm)
colnames(sorghum585.gcm.sum)[2] <- c("sorghum585")
pmillet585.gcm.sum <- ras2area(pmillet585.gcm)
colnames(pmillet585.gcm.sum)[2] <- c("pmillet585")
fmillet585.gcm.sum <- ras2area(fmillet585.gcm)
colnames(fmillet585.gcm.sum)[2] <- c("fmillet585")
rice585.gcm.sum <- ras2area(rice585.gcm)
colnames(rice585.gcm.sum)[2] <- c("rice585")
wheat585.gcm.sum <- ras2area(wheat585.gcm)
colnames(wheat585.gcm.sum)[2] <- c("wheat585")
teff585.gcm.sum <- ras2area(teff585.gcm)
colnames(teff585.gcm.sum)[2] <- c("teff585")
fonio585.gcm.sum <- ras2area(fonio585.gcm)
colnames(fonio585.gcm.sum)[2] <- c("fonio585")
#
# #Combined
maize.curr.gcm  <- all.crop.df1.sum[1,2]
maize.curr.gcm <- as.data.frame(rep(maize.curr.gcm, 10))
pmillet.curr.gcm  <- all.crop.df1.sum[2,2]
pmillet.curr.gcm <- as.data.frame(rep(pmillet.curr.gcm, 10))
fmillet.curr.gcm  <- all.crop.df1.sum[3,2]
fmillet.curr.gcm <- as.data.frame(rep(fmillet.curr.gcm, 10))
sorghum.curr.gcm  <- all.crop.df1.sum[4,2]
sorghum.curr.gcm <- as.data.frame(rep(sorghum.curr.gcm, 10))
rice.curr.gcm  <- all.crop.df1.sum[5,2]
rice.curr.gcm <- as.data.frame(rep(rice.curr.gcm, 10))
wheat.curr.gcm  <- all.crop.df1.sum[6,2]
wheat.curr.gcm <- as.data.frame(rep(wheat.curr.gcm, 10))
teff.curr.gcm  <- all.crop.df1.sum[7,2]
teff.curr.gcm <- as.data.frame(rep(teff.curr.gcm, 10))
fonio.curr.gcm  <- all.crop.df1.sum[8,2]
fonio.curr.gcm <- as.data.frame(rep(fonio.curr.gcm, 10))
# ##
cer.combined.gcm <- cbind(maize.curr.gcm, maize126.gcm.sum$maize126, maize370.gcm.sum$maize370, maize585.gcm.sum$maize585,
                          pmillet.curr.gcm, pmillet126.gcm.sum$pmillet126, pmillet370.gcm.sum$pmillet370, pmillet585.gcm.sum$pmillet585,
                          fmillet.curr.gcm, fmillet126.gcm.sum$fmillet126, fmillet370.gcm.sum$fmillet370, fmillet585.gcm.sum$fmillet585,
                          sorghum.curr.gcm, sorghum126.gcm.sum$sorghum126, sorghum370.gcm.sum$sorghum370, sorghum585.gcm.sum$sorghum585,
                          rice.curr.gcm, rice126.gcm.sum$rice126, rice370.gcm.sum$rice370, rice585.gcm.sum$rice585,
                          wheat.curr.gcm, wheat126.gcm.sum$wheat126, wheat370.gcm.sum$wheat370, wheat585.gcm.sum$wheat585,
                          teff.curr.gcm, teff126.gcm.sum$teff126, teff370.gcm.sum$teff370, teff585.gcm.sum$teff585,
                          fonio.curr.gcm, fonio126.gcm.sum$fonio126, fonio370.gcm.sum$fonio370, fonio585.gcm.sum$fonio585)
head(cer.combined.gcm)
#
colnames(cer.combined.gcm) <- c("maize.curr", "maize126", "maize370", "maize585",
"pmillet.curr", "pmillet126", "pmillet370", "pmillet585",
"fmillet.curr", "fmillet126", "fmillet370", "fmillet585",
"sorghum.curr", "sorghum126", "sorghum370", "sorghum585",
"rice.curr", "rice126.gcm", "rice370", "rice585",
"wheat.curr", "wheat126", "wheat370", "wheat585",
"teff.curr", "teff126", "teff370", "teff585.gcm",
"fonio.curr", "fonio126", "fonio370", "fonio585")
#
library(tibble)
head(cer.combined.gcm)
cer.combined.gcm.blk <- cer.combined.gcm %>%  add_column(Blank1 = 0, .after=4)
cer.combined.gcm.blk <- cer.combined.gcm.blk %>%  add_column(Blank2 = 0, .after=9)
cer.combined.gcm.blk <- cer.combined.gcm.blk %>%  add_column(Blank3 = 0, .after=14)
cer.combined.gcm.blk <- cer.combined.gcm.blk %>%  add_column(Blank4 = 0, .after=19)
cer.combined.gcm.blk <- cer.combined.gcm.blk %>%  add_column(Blank5 = 0, .after=24)
head(cer.combined.gcm.blk)

#write.csv(cer.combined.gcm.blk, "cerr_combined_gcm.csv")


###-----------------------------------------------------------------------------------------------------
print("Inini Ndinonzi Inini")

