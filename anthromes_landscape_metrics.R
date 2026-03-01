## anthromes_landscape_metrics.R
## follow the guidelines in the manual and compute
## landscape metrics for anthrome classes within the subbasins
## see: https://r-spatialecology.github.io/landscapemetrics/articles/irregular_areas.html
## re-ran using published anthrome data
## 28 Feb 2026

## set up
library(landscapemetrics)
library(sf)
library(terra)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggsci)
library(readr)

## paths
hypath<-"./data/hydrobasins/" # hydrobasins for the study region
antpath<-"./data/Dataset 3 Fire Regimes and Land Use Variable/" # path to anthromes raster
ecopath<-"./data/studyarea/" # three eco's study area

############ data prep
## more utils: https://r-spatialecology.github.io/landscapemetrics/articles/utility.html
### anthromes raster in utm (sci base v is lon/lat...projection done in arcgis)
## insert your path after projecting
ant<-rast("./data/Anthromes/Anthromes_Rehfeldt_1km_prj.tif")
check_landscape(ant)

## eco polys UTM
eco3<-read_sf(paste(ecopath, "Final_CASC_studyarea.shp", sep="")) %>% st_zm()

## hydro8 polys WGS84
hy8<-read_sf(paste(hypath, "hybas_na_lev08_v1cSWCASC.shp", sep=""))
ID<-paste("ID", hy8$HYBAS_ID, sep="_")
hy8pol<-hy8 %>% mutate(ID=ID, .after=HYBAS_ID)
hy8pol<-st_transform(hy8pol, crs=st_crs(ant))

############ calc metrics for each of the hydrobasins ####################
hy8_metrics = sample_lsm(ant, hy8pol,
            what = c("lsm_l_pd", "lsm_l_pladj",
            "lsm_l_mesh", "lsm_l_lpi", "lsm_l_ed",
             "lsm_l_joinent", "lsm_l_shdi",
             "lsm_l_pr", "lsm_l_para_cv")) 
## take a look and copy to res
hy8_metrics
res<-hy8_metrics
length(unique(res$plot_id)) # 414 subbasins

## identify dups
## there are none
res |>
  dplyr::summarise(n = dplyr::n(), .by = c(plot_id, metric)) |>
  dplyr::filter(n > 1L) 
#res_nodup<-res %>% distinct(plot_id, metric, .keep_all = TRUE)

# connect values to polygons 
res_w = pivot_wider(res, id_cols = plot_id,
                          names_from = metric,
                          values_from = value)
res_hy8pols = cbind(hy8pol, res_w)
hyant_eco<-st_intersection(res_hy8pols, eco3)
hyant_eco2<-st_transform(hyant_eco, crs="EPSG:4326") # save in lon/lat
hyantmets<-hyant_eco2 %>% dplyr::select(c(1:24,51))
st_write(hyantmets, paste(hypath, "hydro8_anthrome_metrics.shp", sep=""), append=FALSE)

## plot
p1<-ggplot(hyant_eco2) +  
  geom_sf(aes(fill=lpi)) +
  #scale_fill_distiller(palette = "YlOrBr") +
  labs(fill = "Largest Patch Index (LPI)") +
  scale_fill_gradientn(colours = terrain.colors(3)) +
  theme_linedraw()
p1

p2<-ggplot(hyant_eco2) +  
  geom_sf(aes(fill=pd)) +
  scale_fill_distiller(palette = "Set3") +
  labs(fill = "Patch Density (PD)") +
  #scale_fill_gradientn(colours = terrain.colors(30)) +
  theme_linedraw()
p2


