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
#prec.list<- list.files(path="W5E5/Prec/",pattern=".tif$",full.names=T)
#tavg.list<- list.files(path="W5E5/Tavg/",pattern=".tif$",full.names=T)

# Environmental variables
# Environmental variables
prec <- terra::rast(stack(list.files(path="Layers/W5E5/Prec/",pattern=".tif$",full.names=T)))
crs(prec) <- "+proj=longlat +datum=WGS84 +no_defs"

tavg <- terra::rast(stack(list.files(path="Layers/W5E5/Tavg/",pattern=".tif$",full.names=T)))
crs(tavg) <- "+proj=longlat +datum=WGS84 +no_defs"

tmin <- terra::rast(stack(list.files(path="Layers/W5E5/Tmin/",pattern=".tif$",full.names=T)))
crs(tmin) <- "+proj=longlat +datum=WGS84 +no_defs"

#Create an ecocrop model for plantain
plantain <- ecocropPars("Plantain bananas")
plantain$parameters[,1] <- c(260, NA, 220, 365)
plantain$parameters[,3] <- c(10, 18, 34, 42)
plantain$parameters[,4] <- c(20, 60, 1500, 2800)
plantain$parameters[,5] <- c(3.5, 4.0, 7.5, 8)
plantain <- ecocrop(plantain)
control(plantain, get_max=TRUE)
plantain.suit <- predict(plantain, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="Plantain"))
plantain.ras <- raster(plantain.suit)
# 
plot(plantain.ras)
# #Plot
cols2 <- colorRampPalette(c("grey90", "salmon3", "palegreen2", "forestgreen"))
# 
levelplot(plantain.ras, col.regions=cols2, main="plantain_PH", ylab="Lattitude",
          colorkey=list(labels=list(cex=0.9), space="bottom",width= 0.9,height = 0.5), axes = FALSE, margin = F) +
  layer(sp.lines(grids, col='white', alpha=0.6, lwd= 1.2))+
  latticeExtra::layer(sp.polygons(admin, lwd= 0.2, fill='transparent')) +
  latticeExtra::layer(sp.polygons(hollow, lwd = 0.01,  fill = "white"))

#"banana"
banana1 <- ecocropPars("Musa acuminata Colla")
banana1 <- ecocrop(banana1)
control(banana1, get_max=TRUE)
banana1.suit <- predict(banana1, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="banana"))
banana1.ras <- raster(banana1.suit)

banana2 <- ecocropPars("Musa halabanensis Meijer")
banana2$parameters[,1] <- c(200, NA, 160, 365)
banana2$parameters[,4] <- c(20, 60, 1500, 2800)
banana2 <- ecocrop(banana2)
control(banana2, get_max=TRUE)
banana2.suit <- predict(banana2, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="banana"))
banana2.ras <- raster(banana2.suit)

banana3 <- ecocropPars("Musa salaccensis Zoll.")
banana3 <- ecocrop(banana3)
control(banana3, get_max=TRUE)
banana3.suit <- predict(banana3, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="banana"))
banana3.ras <- raster(banana3.suit)

banana.ras <- max(banana1.ras, banana2.ras, banana3.ras)

#enset
enset<- ecocropPars("Enset")
enset$parameters[,1] <- c(180, NA, 160, 365)
enset$parameters[,3] <- c(12, 16, 28, 32)
enset$parameters[,4] <- c(60, 100, 1400, 2800)

enset<- ecocrop(enset)
control(enset, get_max=TRUE)
enset.suit <- predict(enset, prec=prec, tavg=tavg, ph=soilph, wopt=list(names="enset"))
enset.ras <- raster(enset.suit)
plot(enset.ras)
plot(enset.pnts.cln, add=T)
#Stack all suitabilities
crops.hombo <- stack(plantain.ras, banana.ras, enset.ras)
#

#Classification of suitabilitz based on threshold
class.names <- c("Suit", "Unsuit") 

#Reclassify to suit and unsuit
plantain.suit.cl <- reclassify(plantain.ras, class.vec)  
banana.suit.cl <- reclassify(banana.ras, class.vec)  
enset.suit.cl <- reclassify(enset.ras, class.vec)  

hombo.allnow <- stack(plantain.suit.cl, banana.suit.cl, enset.suit.cl)
plot(hombo.allnow)
plot(admin, add=T)

#Save rasters
writeRaster(plantain.suit.cl, paste0(paste0(getwd(),"/Output/Banana/Suit_cl/Current/"),"plantain_cl_curr.tif"), overwrite=TRUE)
writeRaster(banana.suit.cl, paste0(paste0(getwd(),"/Output/Banana/Suit_cl/Current/"),"banana_cl_curr.tif"), overwrite=TRUE)
writeRaster(enset.suit.cl, paste0(paste0(getwd(),"/Output/Banana/Suit_cl/Current/"),"enset_cl_curr.tif"), overwrite=TRUE)

#Adjusted for current crops
# crop.num  <- raster("Layers/Alt/crop_num_3.tif")
# plot(crop.num)
# plot(admin, add=T)

# hombo.multi.now.adj  <- overlay(hombo.multi.now, crop.num, fun=function(a,b) a/b*1)
# plot(hombo.multi.now.adj)
# plot(admin, add=T)

#RCP26-2050 Asessement 
#####______________________________________________________________________________________________________________________________________________________________
#RCP26-2050 Asessement 
# #Save rasters

cer_names <- c("plantain, banana, enset")
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


# #Area calcuations 
# names.admin <- as.data.frame(admin@data)
# names.admin$CODE <- c(1:55)
# 
# tot.px <- as.data.frame(table(getValues(rasterize(africa, plantain.ras))))
# 
# source(sprintf("%sHelper_area_calc_afriq.R", "~/DATABANK/RScripts/Bioclim/Ecocrop/Diversity/"))
# 
# all.crop.df1.sum<- ras2area(all.crop.suit.cl)
# head(all.crop.df1.sum)
# all.crop.df1.sum$Period <- c(rep("Curr", 3), rep("S126", 3), rep("S370", 3), rep("S585", 3))
# all.crop.df1.sum$Crop <- rep(c("plantain",  "banana", "enset"), 4)
# head(all.crop.df1.sum)
# 
# #Plot
# ggplot(all.crop.df1.sum, aes(x = Crop, y = Per, fill = Period)) +
#   geom_bar(stat = "identity", position = "dodge") + ylim(0, 60) +
#   #scale_fill_manual(values = c("palegreen", "salmon")) +
#   labs(x="Crop and Scenario", y = "Suitable area [%]")
# 
# #"plantain"      "banana"      "enset" "whiteyam"    "potato"       "tannia"
# 
# #Extract GCm results for S126
# #plantain S126
# plantain126.gcm.sum <- ras2area(plantain126.gcm)
# colnames(plantain126.gcm.sum)[2] <- c("plantain126")
# banana126.gcm.sum <- ras2area(banana126.gcm)
# colnames(banana126.gcm.sum)[2] <- c("banana126")
# enset126.gcm.sum <- ras2area(enset126.gcm)
# colnames(enset126.gcm.sum)[2] <- c("enset126")
# 
# plantain370.gcm.sum <- ras2area(plantain370.gcm)
# colnames(plantain370.gcm.sum)[2] <- c("plantain370")
# banana370.gcm.sum <- ras2area(banana370.gcm)
# colnames(banana370.gcm.sum)[2] <- c("banana370")
# enset370.gcm.sum <- ras2area(enset370.gcm)
# colnames(enset370.gcm.sum)[2] <- c("enset370")
# 
# plantain585.gcm.sum <- ras2area(plantain585.gcm)
# colnames(plantain585.gcm.sum)[2] <- c("plantain585")
# banana585.gcm.sum <- ras2area(banana585.gcm)
# colnames(banana585.gcm.sum)[2] <- c("banana585")
# enset585.gcm.sum <- ras2area(enset585.gcm)
# colnames(enset585.gcm.sum)[2] <- c("enset585")
# 
# #Combined
# plantain.curr.gcm  <- all.crop.df1.sum[1,2]
# plantain.curr.gcm <- as.data.frame(rep(plantain.curr.gcm, 10))
# banana.curr.gcm  <- all.crop.df1.sum[2,2]
# banana.curr.gcm <- as.data.frame(rep(banana.curr.gcm, 10))
# enset.curr.gcm  <- all.crop.df1.sum[3,2]
# enset.curr.gcm <- as.data.frame(rep(enset.curr.gcm, 10))
# 
# hombo.combined.gcm <- cbind(plantain.curr.gcm, plantain126.gcm.sum$plantain126, plantain370.gcm.sum$plantain370, plantain585.gcm.sum$plantain585,
#                             banana.curr.gcm, banana126.gcm.sum$banana126, banana370.gcm.sum$banana370, banana585.gcm.sum$banana585,
#                             enset.curr.gcm, enset126.gcm.sum$enset126, enset370.gcm.sum$enset370, enset585.gcm.sum$enset585)
# head(hombo.combined.gcm)
# 
# colnames(hombo.combined.gcm) <- c("plantain.curr", "plantain126", "plantain370", "plantain585",
#                                   "banana.curr", "banana126", "banana370", "banana585",
#                                   "enset.curr", "enset126", "enset370", "enset585")
# 
# head(hombo.combined.gcm)
# library(tibble)
# hombo.combined.gcm.blk <- hombo.combined.gcm %>%  add_column(Blank1 = 0, .after=4)
# hombo.combined.gcm.blk <- hombo.combined.gcm.blk %>%  add_column(Blank2 = 0, .after=9)
# head(hombo.combined.gcm.blk)
# 
# write.csv(hombo.combined.gcm.blk, "hombo_combined_gcm.csv")
# 
# write.csv(all.crop.df1.sum, "hombo_all_gcm.csv")
###-----------------------------------------------------------------------------------------------------
print("Inini Ndinonzi Inini")



