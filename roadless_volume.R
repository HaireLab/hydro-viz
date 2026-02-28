## roadless_volume.R
## 1. summarize the roadless volume raster by extracting
## the values in each hydrobasin polygon and returning the mean
## 2. intersect with the study region polys
## S. Haire
## 28 Feb 2026 updated with roadless volume from the data release
## https://www.sciencebase.gov/catalog/item/6920ef6dd4be025bc609c70e
## data source for hydrobasins: https://www.hydrosheds.org/products/hydrobasins

## set up
library(sf)
library(terra)
library(dplyr)
library(tidyterra)
library(ggplot2)

## paths
hypath<-"./data/hydrobasins/" # hydrobasins for the study region
rvpath<-"./data/Dataset 3 Fire Regimes and Land Use Variable/" # path to roadless vol
ecopath<-"./data/studyarea/" # three eco's study area

########## data ###########
## study area polys
eco3<-read_sf(paste(ecopath, "Final_CASC_studyarea.shp", sep="")) %>%
  st_zm(drop=TRUE)

## roadless volume in utm
rv<-rast(paste(rvpath, "RV_100.tif", sep=""))

## hydro8 polys WGS84
hy8<-read_sf(paste(hypath, "hybas_na_lev08_v1cSWCASC.shp", sep=""))
ID<-paste("ID", hy8$HYBAS_ID, sep="_")
hy8polys<-hy8 %>% mutate(ID=ID, .after=HYBAS_ID)
hy8rv<-st_transform(hy8polys, crs=st_crs(rv))

## extract values by hy8 poly; calc mean
ex.rv<-terra::extract(rv, hy8rv, fun=mean, na.rm=TRUE, ID=TRUE, touches=TRUE)
rv_df<-bind_cols(hy8rv$ID, ex.rv[,2])
names(rv_df)<-c("ID", "rv_km3")

## merge with hy8 then intersect
hy8_rv<-right_join(hy8rv, rv_df, by="ID")
eco3_utm<-st_transform(eco3, crs=st_crs(hy8_rv))
hyrv_eco<-st_intersection(hy8_rv, eco3utm) #519
hyrv_eco2<-st_transform(hyrv_eco, crs="EPSG:4326")
plot(hyrv_eco2["rv_km3"])
## change to your output directory
st_write(hy8_rv, "./data/xvar_outputs/hy8_RV.shp")

## plot
## 
ggplot(hyrv_eco2) +  
  geom_sf(aes(fill=rv_km3)) + 
 # scale_fill_distiller(palette="YlOrRd") +  
  scale_fill_whitebox_c(palette="arid") +
  labs(title = "Roadless volume", fill=expression(paste("km"^"3"))) +  
  theme(legend.position = "right") +
  theme_linedraw()

