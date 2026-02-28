## fire_metrics_hydrosheds.R
## S. Haire
## 28 Feb 2026 updated with fire perims from the data release
## https://www.sciencebase.gov/catalog/item/6920ef6dd4be025bc609c70e
## 
## Purpose: Use the fire perimeter polygon data to 
## summarize fire attributes in hydrosheds 
##### level 8; https://www.hydrosheds.org/products/hydrobasins

## Process one ecoregion at a time and then combine.
## Examples of plotting the data are included at the end.

## set up ###################
library(sf)
library(terra)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(units)
library(vegan)
# Neighbourhood richness fnc, unique fire years in buffer
richness <- function(x, ...) {
  length(unique(na.omit(x)))
}

## paths
hypath<-"./data/hydrobasins/" # hydrobasins for the study region
firepath<-"./data/Dataset 1 Fire Perimeters/" # fire perims
ecopath<-"./data/studyarea/" # three eco's study area

### data ###################
## level 8 three ecos 414 obs
hydro8<-read_sf(paste(hypath, "hybas_na_lev08_v1cSWCASC.shp", sep=""))
## join with nsmo polys
nsmopol<-read_sf(paste(firepath, "SMO_Largefires_WGS84_zone12_Final_v3.shp", sep=""))
hydro8tr <- st_transform(hydro8, crs=st_crs(nsmopol))
hydro_nsmo<-st_join(hydro8tr, nsmopol) #2033 features

## calc fire metrics
## must use dplyr:: or the mutate fnc doesn't do what you want
xhy<-hydro_nsmo %>% 
  dplyr::mutate(Year.num=as.numeric(Year), .after=HYBAS_ID) %>% 
  group_by(HYBAS_ID) %>%
  dplyr::mutate(n = n(), shannon_yr=diversity(Year.num, "shannon"), 
         simpson_yr=diversity(Year.num, "simpson"),
         div_yr=richness(Year.num), max_yr=max(Year.num), min_yr=min(Year.num), 
         richness_yr=richness(Year.num), meansize=mean(Hectares), maxsize=max(Hectares),
         totalarea=sum(Hectares),.after=HYBAS_ID)
xhy.nsmo<-xhy %>% distinct(HYBAS_ID, .keep_all=TRUE) # 414
nsmo_hyfdat <- xhy.nsmo %>% dplyr::select(c("HYBAS_ID", "n", "shannon_yr",
              "simpson_yr", "div_yr", "max_yr", "min_yr", "richness_yr",
              "meansize", "maxsize", "totalarea", 
              "NEXT_DOWN",   "NEXT_SINK",   "MAIN_BAS",    "DIST_SINK",  
              "DIST_MAIN",   "SUB_AREA",    "UP_AREA",     "PFAF_ID",    
               "ENDO" ,       "COAST" ,      "ORDER_",      "SORT",  
              "Ecoregion", "geometry"))
plot(nsmo_hyfdat["simpson_yr"])

## sky islands
skypol<-read_sf(paste(firepath, "Sky_Island_Largefires_1985_to_2022_Final_v3.shp", sep=""))
hydro8tr <- st_transform(hydro8, crs=st_crs(skypol))
hydro_sky<-st_join(hydro8tr, skypol) # 1,141 records, 27 fields

## calc fire metrics
xhy<-hydro_sky %>% dplyr::mutate(Year.num=as.numeric(Year), .after=HYBAS_ID) %>% 
  group_by(HYBAS_ID) %>%
  dplyr::mutate(n=n(),shannon_yr=diversity(Year.num, "shannon"), simpson_yr=diversity(Year.num, "simpson"),
         div_yr=richness(Year.num), max_yr=max(Year.num), min_yr=min(Year.num), 
         richness_yr=richness(Year.num), meansize=mean(Hectares), maxsize=max(Hectares), 
          totalarea=sum(Hectares),.after=HYBAS_ID)
xhy.sky<-xhy %>% distinct(HYBAS_ID, .keep_all=TRUE) 
sky_hyfdat <- xhy.sky %>% dplyr::select(c("HYBAS_ID", "n", "shannon_yr",
                              "simpson_yr", "div_yr", "max_yr", "min_yr", "richness_yr",
                              "meansize", "maxsize", "totalarea", 
                              "NEXT_DOWN",   "NEXT_SINK",   "MAIN_BAS",    "DIST_SINK",  
                              "DIST_MAIN",   "SUB_AREA",    "UP_AREA",     "PFAF_ID",    
                              "ENDO" ,       "COAST" ,      "ORDER_",      "SORT",  
                              "Ecoregion", "geometry"))
plot(sky_hyfdat["simpson_yr"])

## plateau
plapol<-read_sf(paste(firepath, "Plateau_Largefires_WGS84_zone12_Final_v3.shp", sep=""))
hydro8tr <- st_transform(hydro8, crs=st_crs(plapol))
hydro_pla<-st_join(hydro8tr, plapol)

## calc fire metrics
xhy<-hydro_pla %>% dplyr::mutate(Year.num=as.numeric(Year), .after=HYBAS_ID) %>% 
  group_by(HYBAS_ID) %>%
  dplyr::mutate(n=n(),shannon_yr=diversity(Year.num, "shannon"), simpson_yr=diversity(Year.num, "simpson"),
         div_yr=richness(Year.num), max_yr=max(Year.num), min_yr=min(Year.num), 
         richness_yr=richness(Year.num), meansize=mean(Hectares), maxsize=max(Hectares), 
         totalarea=sum(Hectares),.after=HYBAS_ID)
xhy.pla<-xhy %>% distinct(HYBAS_ID, .keep_all=TRUE) 
pla_hyfdat <- xhy.pla %>% dplyr::select(c("HYBAS_ID", "n", "shannon_yr",
                                          "simpson_yr", "div_yr", "max_yr", "min_yr", "richness_yr",
                                          "meansize", "maxsize", "totalarea", 
                                          "NEXT_DOWN",   "NEXT_SINK",   "MAIN_BAS",    "DIST_SINK",  
                                          "DIST_MAIN",   "SUB_AREA",    "UP_AREA",     "PFAF_ID",    
                                          "ENDO" ,       "COAST" ,      "ORDER_",      "SORT",  
                                          "Ecoregion", "geometry"))
plot(pla_hyfdat["simpson_yr"])

## fix names and combine the three sf's
pla_hyfdat$region_id="PLATEAU"
sky_hyfdat$region_id="SKY"
nsmo_hyfdat$region_id="NSMO"
all_hyf<-bind_rows(pla_hyfdat, sky_hyfdat, nsmo_hyfdat) # 1242
## records w fire
hyfire<-all_hyf %>% dplyr::filter(!is.na(totalarea)) # 357

## Interset w ecoregions
eco3<-read_sf(paste(ecopath, "Final_CASC_studyarea.shp", sep="")) %>% st_zm(drop=TRUE)
eco3utm<-st_transform(eco3, crs=st_crs(all_hyf))
hyfire_eco<-st_intersection(hyfire, eco3utm) #519
hyfire_eco2<-st_transform(hyfire_eco, crs="EPSG:4326") ## wgs 84
## select columns to keep, save the data, 
## then read back in and assign var names
save_hyfire_eco2 <- hyfire_eco2 %>% dplyr::select(
  "HYBAS_ID", "Ecoregion", "Complex_Na", "n", "shannon_yr","simpson_yr", 
  "div_yr", "max_yr", "min_yr", "richness_yr",
 "meansize","maxsize", "totalarea", "NEXT_DOWN",  
  "NEXT_SINK",   "MAIN_BAS",   "DIST_SINK",   "DIST_MAIN",  
  "SUB_AREA",   "UP_AREA",     "PFAF_ID",     "ENDO",       
  "COAST",       "ORDER_",     "SORT", "geometry"       
)
## write error due to esri :(
st_write(save_hyfire_eco2, paste(hypath, "hydro8_fire_metrics_finalperims.shp", sep=""))
## read back in and rename to use for plots
x<-read_sf(paste(hypath, "hydro8_fire_metrics_finalperims.shp", sep=""))
names(x)<-c( "HYBAS_ID", "Ecoregion", "Complex_Na", "n", "shannon_yr","simpson_yr", 
               "div_yr", "max_yr", "min_yr", "richness_yr",
               "meansize","maxsize", "totalarea", "NEXT_DOWN",  
               "NEXT_SINK",   "MAIN_BAS",   "DIST_SINK",   "DIST_MAIN",  
               "SUB_AREA",   "UP_AREA",     "PFAF_ID",     "ENDO",       
               "COAST",       "ORDER_",     "SORT", "geometry")
## sf plots
plot(x["maxsize"])
plot(x["shannon_yr"])
## ggplot
x %>% # dplyr::filter(Complex_Na == "Sierra Madre Norte") %>%
  ggplot() +  
  geom_sf(aes(fill=shannon_yr)) + scale_fill_viridis_c(option="plasma") + 
  labs(fill = "") + #theme_dark()
theme_linedraw()

