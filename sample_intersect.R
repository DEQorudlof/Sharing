library(sp)
library(sf)
library(tidycensus)
library(tidyverse)
library(dplyr)
library(leaflet)
library(mapview)
library(data.table)
library(janitor)



race_vars <- c(White = "B03002_003", Black = "B03002_004", Native = "B03002_005", 
               Asian = "B03002_006", HIPI = "B03002_007", Hispanic = "B03002_012")

or_pop <- get_acs(geography = "tract", 
                   state = "OR",
                   variables = "B03002_001", 
                   geometry = TRUE)

or_popsf <- st_as_sf(or_pop) %>% st_transform(crs= 3488)
or_popsf$area_sqkm<- as.numeric(st_area(or_popsf)/1000000)
#View(ord1)
#ord1$pop_div<-(ord1$pop_tot-ord1$pop_white)

or_popsf$totdens<-(or_popsf$estimate/or_popsf$area_sqkm)
mapview(or_popsf)

asheds <- st_read("C:/Users/orudlof/OneDrive - Oregon/Documents/R/EPA Grant/Sharing/oha_airsheds", "oregon_airshed_complete_coverage4", stringsAsFactors = FALSE)
asheds$row_num <- seq.int(nrow(asheds))
asheds2 <- st_collection_extract(asheds, "POLYGON")

st_write(asheds2, "asheds2.shp")

asheds2sf<- st_as_sf(asheds) %>% st_transform(3488)
mapview(st_zm(asheds2sf), legend = TRUE)
mapview(ord1)

int <- st_intersection(asheds2sf, or_popsf)
mapview(st_zm(int))
int$intarea_sqkm<- as.numeric(st_area(int)/1000000)
int$pop<-(int$intarea_sqkm*int$totdens)
#int2 <- int[-c(1:5,7:12)]
int3<- data.frame(int)
#int4 <- int3[-c(4)]
#View(int4)
int5 <- data.table(int3)
int6<-int5[,list(airshed_areakm2=sum(intarea_sqkm), airshed_pop=sum(pop)), by=row_num]
int7<-left_join(asheds2sf, int6,by="row_num")
mapview(st_zm(int7))

st_write(st_zm(int7), "airshed_population.shp")
