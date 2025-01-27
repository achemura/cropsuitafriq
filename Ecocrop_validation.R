arm(list=ls()) # Cleaning 
gc()

library(geodata)
library(Recocrop)
library(raster)
library(rgdal)
library(terra)
library(dplyr)
library(rgbif)
library(dismo)
library(tidyverse)  
library(openxlsx)  

setwd("/Users/chemura/CLUSTER/Ecocrop")
#source("Ecocrop_functions.R") 

####################################################
#########         Crop information         #########
####################################################

crop <- 'Maize'
SPAM_name <- 'MAIZ'
FAO_name <- '?'

#### get an idea of parameters to decide range
model <- ecocropPars(crop)
pr_parameters <- model$parameters[,4]

####################################################
#########    Geo, Soil and Climate data.   #########  
####################################################

### read shapes
admin <- readOGR("/DATABANK/Africa.shp")
admin$ADM0_NAME[11] <- "Côte d'Ivoire"

### load constant soil ph info
soilph  <- rast("Soil/soilph_is3.tif")
crs(soilph) <- "+proj=longlat +datum=WGS84 +no_defs"

#### load annual Environmental variables
prec <- rast("Climate/pr_W5E5v2.0_1979-2019monmean.nc")
tavg <- rast("Climate/tas_W5E5v2.0_1979-2019monmean.nc")
tmin <- rast("Climate/tasmin_W5E5v2.0_1979-2019monmean.nc")
raster_dims <- dim(prec)

####################################################
######### SPAM harvested area per country  #########  
####################################################

spam17_data <- rast(paste0("DATABANK",SPAM_name,"_R.tif"))
spam17_data <- raster(crop(spam17_data,prec))

#Area calculation
spam17_data = rasterToPoints(spam17_data, spatial = T) # convert to SpatiaPoints dataframe
spam17_data = sf::st_as_sf(spam17_data) # converts to sf object
admin_shape = sf::st_as_sf(admin)
spam17_admin = as.data.frame(sf::st_join(spam17_data, admin_shape))
spam17_admin = spam17_admin[ , -which(names(spam17_admin) %in% c("ISO3", "FAOSTAT", "Totarea","geometry"))]
colnames(spam17_admin) <- c("Hektar", "State")

spam17_state <- spam17_admin %>%
  group_by(State) %>%
  summarise(spam_area = sum(Hektar))

spam17_state <- as.data.frame(spam17_state)

####################################################
######### FAO harvested area per country  #########  
####################################################

fao_data <- read.csv("/DATABANK/Production_Crops_Livestock_E_Africa_NOFLAG.csv")  
fao_data <- reshape(fao_data, 
                         direction = "long",
                         varying = list(names(fao_data)[8:66]),
                         v.names = "Value",
                         idvar = c("Area.Code", "Area","Item.Code", "Item", "Element.Code", "Element", "Unit"),
                         timevar = "Year",
                         times = 1961:2019)
fao_state <- fao_data %>%                       
  filter(Element == "Area harvested" & Year >= 1979 & Item == "Maize") %>%  # Only use Area data from 1979 to 2019 for evaluation
  group_by(Area, Item) %>%                                # Group by country and crop
  summarize(fao_area = mean(Value)) #%>%       # Calculate mean area
fao_state = fao_state[ , -which(names(fao_state) %in% c("Item"))]
colnames(fao_state) <- c("State","fao_area")

####################################################
####  Ecocrop model with different parameters  #####  
####################################################

corr_max=0
x1_max=0
x2_max=0
x3_max=0
x4_max=0
thresh_max=0

halfrange = 10
step = 5

mini_bad <- seq(pr_parameters[1]-halfrange,pr_parameters[1]+halfrange,step)
mini_opt <- seq(pr_parameters[2]-halfrange,pr_parameters[2]+halfrange,step)
maxi_opt <- seq(pr_parameters[3]+halfrange,pr_parameters[3]-halfrange,-step)
maxi_bad <- seq(pr_parameters[4]+halfrange,pr_parameters[4]-halfrange,-step)
print(paste0('TRYING  ',length(mini_bad)," parameter combinations"))

for (p_count in seq(1,length(mini_bad))){

print(paste0('-----  p_count ',p_count))
print(paste0('trying combination of parameters ',mini_bad[p_count],',',mini_opt[p_count],',',maxi_opt[p_count],',',maxi_bad[p_count]))
# set up Ecocrop
crop_model <- ecocropPars(crop)
crop_model$parameters[,4] <- c(mini_bad[p_count],mini_opt[p_count],maxi_opt[p_count],maxi_bad[p_count])
crop_model <- Recocrop::ecocrop(crop_model)
control(crop_model, get_max=TRUE)
crop_suit <- predict(crop_model, ktmp=tmin, ktmp=tmin, prec=prec, tavg=tavg, ph=soilph, wopt=list(names=crop))

# Classification of suitability
for (threshold in seq(0.4,0.6,0.05)){
  print(paste0('  with threshold ',threshold))
  
  suit_bin <- reclassify(raster(crop_suit), c(0, threshold, 0, threshold, 1, 1))
  
  # turn suitable pixels to area in hektar
  raster_states <- rasterize(admin, suit_bin)
  suit_df <- as.data.frame(crosstab(raster_states, suit_bin))
  colnames(suit_df) <- c("countrycode", "occurence", "freq")
  suit_df$State <- admin@data$ADM0_NAME
  suit_state <- subset(suit_df,occurence==1)
  suit_state$suit_area <- suit_state$freq * 3025 * 100
  suit_state = suit_state[ , -which(names(suit_state) %in% c("countrycode","occurence","freq"))]
  
  # Combine SPAM, FAO and Suitability areas
  
  crop_areas <- merge(suit_state, spam17_state, by="State")
  crop_areas <- merge(crop_areas, fao_state, by="State")
  crop_areas_na <- na.omit(crop_areas)
  cor_spam_suit <- cor(crop_areas_na$spam_area, crop_areas_na$suit_area)
  cor_fao_suit <- cor(crop_areas_na$fao_area, crop_areas_na$suit_area)
  
  if ( cor_spam_suit > corr_max ){
  corr_max=cor_spam_suit
  x1_max=mini_bad[p_count]
  x2_max=mini_opt[p_count]
  x3_max=maxi_opt[p_count]
  x4_max=maxi_bad[p_count]
  thresh_max=threshold
  }
  
} # end of threshold
} # end of p_count


