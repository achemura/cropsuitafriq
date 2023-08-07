rm(list=ls()) # Cleaning the shit off
gc()

library(geodata)
library(Recocrop)
library(raster)
library(rgdal)
library(terra)
library(rasterVis)
#library(BiodiversityR)

library(ggplot2)
library(tibble)

setwd("~/DATABANK/Diversity/")

#SHP
admin <- readOGR("SHP/Africa.shp")
hollow <- readOGR("SHP/Afrihollow.shp")
grids <- readOGR("SHP/graticules_30.shp")
water <- readOGR("~/DATABANK/EastAfrica/Africa/africa_water_bodies.shp")
africa <- readOGR("SHP/Africa_conti.shp")

soilph  <- rast("Layers/Alt/soilph_is3.tif")
crs(soilph) <- "+proj=longlat +datum=WGS84 +no_defs"

# CUrrent climate data
#prec.list<- list.files(path="Layers/Current/Prec/",pattern=".tif$",full.names=T)
#tavg.list<- list.files(path="Layers/Current/Tavg/",pattern=".tif$",full.names=T)

# Environmental variables
prec <- terra::rast(stack(list.files(path="Layers/ISIMIP3/Current/Prec/",pattern=".tif$",full.names=T)))
crs(prec) <- "+proj=longlat +datum=WGS84 +no_defs"

tavg <- terra::rast(stack(list.files(path="Layers/ISIMIP3/Current/Tavg/",pattern=".tif$",full.names=T)))
crs(tavg) <- "+proj=longlat +datum=WGS84 +no_defs"

#Create an ecocrop model for cassava
cassava <- ecocropPars("cassava")
cassava$parameters[,3] <- c(15, 22, 32, 45)
cassava$parameters[,4] <- c(20, 40, 2000, 2400)
cassava <- ecocrop(cassava)
control(cassava, get_max=TRUE)
cassava.suit <- predict(cassava, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="cassava"))
cassava.ras <- raster(cassava.suit)

#Plot
cols2 <- colorRampPalette(c("grey90", "salmon3", "palegreen2", "forestgreen"))

# levelplot(cassava.ras, col.regions=cols2, main="cassava_PH", ylab="Lattitude",
#           colorkey=list(labels=list(cex=0.9), space="bottom",width= 0.9,height = 0.5), axes = FALSE, margin = F) + 
#   layer(sp.lines(grids, col='white', alpha=0.6, lwd= 1.2))+
#   latticeExtra::layer(sp.polygons(admin, lwd= 0.2, fill='transparent')) +
#   latticeExtra::layer(sp.polygons(hollow, lwd = 0.01,  fill = "white"))

#"Groundnuts"
cocoyam <- ecocropPars("Cocoyam")
cocoyam$parameters[,4] <- c(60, 111, 360, 800)
cocoyam <- ecocrop(cocoyam)
control(cocoyam, get_max=TRUE)
cocoyam.suit <- predict(cocoyam, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="Cocoyam"))
cocoyam.ras <- raster(cocoyam.suit)

#Bambara nut
potato <- ecocropPars("potato")
potato <- ecocrop(potato)
control(potato, get_max=TRUE)
potato.suit <- predict(potato, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="potato"))
potato.ras <- raster(potato.suit)

#sweetpotato
sweetpotato <- ecocropPars("Sweet potato")
sweetpotato <- ecocrop(sweetpotato)
control(sweetpotato, get_max=TRUE)
sweetpotato.suit <- predict(sweetpotato, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="Sweet potato"))
sweetpotato.ras <- raster(sweetpotato.suit)

#sweetpotato
tannia <- ecocropPars("tannia")
tannia <- ecocrop(tannia)
control(tannia, get_max=TRUE)
tannia.suit <- predict(tannia, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="tannia"))
tannia.ras <- raster(tannia.suit)

#tannia
whiteyam <- ecocropPars("white yam")
whiteyam$parameters[,3] <- c(15, 21, 32, 38)
whiteyam$parameters[,4] <- c(30, 60, 400, 650)
whiteyam <- ecocrop(whiteyam)
control(whiteyam, get_max=TRUE)
whiteyam.suit <- predict(whiteyam, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="white yam"))
whiteyam.ras <- raster(whiteyam.suit)

#Stack all suitabilities
crops.pulse <- stack(cassava.ras, cocoyam.ras, potato.ras, 
                     sweetpotato.ras, tannia.ras, whiteyam.ras)
#

#Classification of suitabilitz based on threshold
class.names <- c("Suit", "Unsuit") 

#Reclassify to suit and unsuit
cassava.suit.cl <- reclassify(cassava.ras, class.vec)  
cocoyam.suit.cl <- reclassify(cocoyam.ras, class.vec)  
sweetpotato.suit.cl <- reclassify(sweetpotato.ras, class.vec)  
whiteyam.suit.cl <- reclassify(whiteyam.ras, class.vec)  
potato.suit.cl <- reclassify(potato.ras, class.vec)  
tannia.suit.cl <- reclassify(tannia.ras, class.vec)  


roots.allnow <- stack(cassava.suit.cl, cocoyam.suit.cl, sweetpotato.suit.cl, 
                             whiteyam.suit.cl, potato.suit.cl, tannia.suit.cl)
plot(roots.allnow)
plot(admin, add=T)

#Save rasters
writeRaster(cassava.suit.cl, paste0(paste0(getwd(),"/Output/Roots/Suit_cl/Current/"),"cassava_cl_curr.tif"), overwrite=TRUE)
writeRaster(cocoyam.suit.cl, paste0(paste0(getwd(),"/Output/Roots/Suit_cl/Current/"),"cocoyam_cl_curr.tif"), overwrite=TRUE)
writeRaster(sweetpotato.suit.cl, paste0(paste0(getwd(),"/Output/Roots/Suit_cl/Current/"),"sweetpotato_cl_curr.tif"), overwrite=TRUE)
writeRaster(whiteyam.suit.cl, paste0(paste0(getwd(),"/Output/Roots/Suit_cl/Current/"),"whiteyam_cl_curr.tif"), overwrite=TRUE)
writeRaster(potato.suit.cl, paste0(paste0(getwd(),"/Output/Roots/Suit_cl/Current/"),"potato_cl_curr.tif"), overwrite=TRUE)
writeRaster(tannia.suit.cl, paste0(paste0(getwd(),"/Output/Roots/Suit_cl/Current/"),"tannia_cl_curr.tif"), overwrite=TRUE)

#Adjusted for current crops


#RCP26-2050 Asessement 
# #Save rasters

cer_names <- c("cassava, cocoyam, sweetpotato, whiteyam, potato, tannia")
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


##___________________________________________________________________________________
all.crop.suit.cl <- stack(roots.allnow, roots.all26, roots.all70, roots.all85)

#Area calcuations 
names.admin <- as.data.frame(africa@data)
# 
tot.px <- as.data.frame(table(getValues(rasterize(africa, cassava.ras))))
tot.px

#Area calculation
# proj4string(all.crop.suit.cl) = proj4string(africa)
# all.crop.suit.r = rasterToPoints(all.crop.suit.cl, spatial = T)
# all.crop.suit.r.pts = sf::st_as_sf(all.crop.suit.r)
# all.crop.sf.shpa = sf::st_as_sf(africa)
# all.crop.join = sf::st_join(all.crop.suit.r.pts,all.crop.sf.shpa)
# all.crop.join = as.data.frame(all.crop.join)
# head(all.crop.join)
# all.crop.df1 = all.crop.join[ , -which(names(all.crop.join) %in% c("Id","geometry"))]
# head(all.crop.df1)
#
# all.crop.df1.sum <- as.data.frame(colSums(all.crop.df1))
# colnames(all.crop.df1.sum) <- "Suit_px"
# all.crop.df1.sum$Per <-(all.crop.df1.sum$Suit_px/tot.px$Freq) * 100
source(sprintf("%sHelper_area_calc_afriq.R", "~/DATABANK/RScripts/Bioclim/Ecocrop/Diversity/"))

all.crop.df1.sum<- ras2area(all.crop.suit.cl)
head(all.crop.df1.sum)
all.crop.df1.sum$Period <- c(rep("Curr", 6), rep("S126", 6), rep("S370", 6), rep("S585", 6))
all.crop.df1.sum$Crop <- rep(c("cassava",  "cocoyam", "sweet.potato", "White.yam", "potato","tannia"), 4)
head(all.crop.df1.sum)

#Plot
ggplot(all.crop.df1.sum, aes(x = Crop, y = Per, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") + ylim(0, 60) +
  #scale_fill_manual(values = c("palegreen", "salmon")) +
  labs(x="Crop and Scenario", y = "Suitable area [%]")


#"cassava"      "Cocoyam"      "sweet.potato" "whiteyam"    "potato"       "tannia"

#Extract GCm results for S126
#cassava S126
cassava126.gcm.sum <- ras2area(cassava126.gcm)
colnames(cassava126.gcm.sum)[2] <- c("cassava126")
cocoyam126.gcm.sum <- ras2area(cocoyam126.gcm)
colnames(cocoyam126.gcm.sum)[2] <- c("cocoyam126")
sweetpotato126.gcm.sum <- ras2area(sweetpotato126.gcm)
colnames(sweetpotato126.gcm.sum)[2] <- c("sweetpotato126")
whiteyam126.gcm.sum <- ras2area(whiteyam126.gcm)
colnames(whiteyam126.gcm.sum)[2] <- c("whiteyam126")
potato126.gcm.sum <- ras2area(potato126.gcm)
colnames(potato126.gcm.sum)[2] <- c("potato126")
tannia126.gcm.sum <- ras2area(tannia126.gcm)
colnames(tannia126.gcm.sum)[2] <- c("tannia126")

cassava370.gcm.sum <- ras2area(cassava370.gcm)
colnames(cassava370.gcm.sum)[2] <- c("cassava370")
cocoyam370.gcm.sum <- ras2area(cocoyam370.gcm)
colnames(cocoyam370.gcm.sum)[2] <- c("cocoyam370")
sweetpotato370.gcm.sum <- ras2area(sweetpotato370.gcm)
colnames(sweetpotato370.gcm.sum)[2] <- c("sweetpotato370")
whiteyam370.gcm.sum <- ras2area(whiteyam370.gcm)
colnames(whiteyam370.gcm.sum)[2] <- c("whiteyam370")
potato370.gcm.sum <- ras2area(potato370.gcm)
colnames(potato370.gcm.sum)[2] <- c("potato370")
tannia370.gcm.sum <- ras2area(tannia370.gcm)
colnames(tannia370.gcm.sum)[2] <- c("tannia370")

cassava585.gcm.sum <- ras2area(cassava585.gcm)
colnames(cassava585.gcm.sum)[2] <- c("cassava585")
cocoyam585.gcm.sum <- ras2area(cocoyam585.gcm)
colnames(cocoyam585.gcm.sum)[2] <- c("cocoyam585")
sweetpotato585.gcm.sum <- ras2area(sweetpotato585.gcm)
colnames(sweetpotato585.gcm.sum)[2] <- c("sweetpotato585")
whiteyam585.gcm.sum <- ras2area(whiteyam585.gcm)
colnames(whiteyam585.gcm.sum)[2] <- c("whiteyam585")
potato585.gcm.sum <- ras2area(potato585.gcm)
colnames(potato585.gcm.sum)[2] <- c("potato585")
tannia585.gcm.sum <- ras2area(tannia585.gcm)
colnames(tannia585.gcm.sum)[2] <- c("tannia585")


#Combined
cassava.curr.gcm  <- all.crop.df1.sum[1,2]
cassava.curr.gcm <- as.data.frame(rep(cassava.curr.gcm, 10))
cocoyam.curr.gcm  <- all.crop.df1.sum[2,2]
cocoyam.curr.gcm <- as.data.frame(rep(cocoyam.curr.gcm, 10))
sweetpotato.curr.gcm  <- all.crop.df1.sum[3,2]
sweetpotato.curr.gcm <- as.data.frame(rep(sweetpotato.curr.gcm, 10))
whiteyam.curr.gcm  <- all.crop.df1.sum[4,2]
whiteyam.curr.gcm <- as.data.frame(rep(whiteyam.curr.gcm, 10))
potato.curr.gcm  <- all.crop.df1.sum[5,2]
potato.curr.gcm <- as.data.frame(rep(potato.curr.gcm, 10))
tannia.curr.gcm  <- all.crop.df1.sum[6,2]
tannia.curr.gcm <- as.data.frame(rep(tannia.curr.gcm, 10))

roots.combined.gcm <- cbind(cassava.curr.gcm, cassava126.gcm.sum$cassava126, cassava370.gcm.sum$cassava370, cassava585.gcm.sum$cassava585,
                             cocoyam.curr.gcm, cocoyam126.gcm.sum$cocoyam126, cocoyam370.gcm.sum$cocoyam370, cocoyam585.gcm.sum$cocoyam585,
                             sweetpotato.curr.gcm, sweetpotato126.gcm.sum$sweetpotato126, sweetpotato370.gcm.sum$sweetpotato370, sweetpotato585.gcm.sum$sweetpotato585,
                             whiteyam.curr.gcm, whiteyam126.gcm.sum$whiteyam126, whiteyam370.gcm.sum$whiteyam370, whiteyam585.gcm.sum$whiteyam585,
                             potato.curr.gcm, potato126.gcm.sum$potato126, potato370.gcm.sum$potato370, potato585.gcm.sum$potato585,
                             tannia.curr.gcm, tannia126.gcm.sum$tannia126, tannia370.gcm.sum$tannia370, tannia585.gcm.sum$tannia585)
head(roots.combined.gcm)

colnames(roots.combined.gcm) <- c("cassava.curr", "cassava126", "cassava370", "cassava585",
                                   "cocoyam.curr", "cocoyam126", "cocoyam370", "cocoyam585",
                                   "sweetpotato.curr", "sweetpotato126", "sweetpotato370", "sweetpotato585",
                                   "whiteyam.curr", "whiteyam126.gcm", "whiteyam370", "whiteyam585",
                                   "potato.curr", "potato126", "potato370", "potato585",
                                  "tannia.curr", "tannia126", "tannia370", "tannia585")

head(roots.combined.gcm)
library(tibble)
roots.combined.gcm.blk <- roots.combined.gcm %>%  add_column(Blank1 = 0, .after=4)
roots.combined.gcm.blk <- roots.combined.gcm.blk %>%  add_column(Blank2 = 0, .after=9)
roots.combined.gcm.blk <- roots.combined.gcm.blk %>%  add_column(Blank3 = 0, .after=14)
roots.combined.gcm.blk <- roots.combined.gcm.blk %>%  add_column(Blank4 = 0, .after=19)
roots.combined.gcm.blk <- roots.combined.gcm.blk %>%  add_column(Blank5 = 0, .after=24)
head(roots.combined.gcm.blk)

# 
# write.csv(roots.combined.gcm.blk, "roots_combined_gcm.csv")
###-----------------------------------------------------------------------------------------------------
print("Inini Ndinonzi Inini")

