##EMB
##2023-03-28


library(tidyverse)
library(raster)
library(mapdata)
library(marmap) #getNOAA.bathy()

chl_raster= raster("data/AQUA_MODIS.20020701_20220731.L3m.MC.CHL.chlor_a.9km.nc")
chl_raster
names(chl_raster)="chl_a"
names(chl_raster)

#change to data frame

chl_pts= rasterToPoints(chl_raster, spatial=TRUE)
head(chl_pts)
chl_df= data.frame(chl_pts)
head(chl_df)
hist(chl_df$chl_a)
max(chl_df$chl_a)
hist(log10(chl_df$chl_a))

#colors?
cols= rainbow(7, rev=TRUE)[-1] #codes for colors on a rainbow, flipped the order and dropping first one (purple)

global_chl_map=ggplot()+
  geom_raster(data=chl_df, aes(x=x, y=y,fill=log10(chl_a)))+
  scale_fill_gradientn(colors=cols, limits=c(-1.5, 0.75), name="log_10(chla")+
  theme_classic() #set limits of range the data crosses, dark blue set at -1.5, bright red set at 0.75
ggsave(global_chl_map, filename="figures/global_chl_July2002-July2022.pdf", device="pdf",
       height=5, width=9)

##section off gulf of maine and look at chla
#choose lat and long boundaries

lon_bound=c(-72, -62)
lat_bound=c(39,47)

#crop
c(lon_bound, lat_bound)
chl_GOM_raster= raster::crop(chl_raster, extent(c(lon_bound, lat_bound)))

chl_GOM_df=data.frame(rasterToPoints(chl_GOM_raster, spatial=TRUE))
head(chl_GOM_df)

world_map=map_data("worldHires")
head(world_map)

GOM_chl_map= ggplot()+
  geom_raster(data=chl_GOM_df, aes(x=x, y=y, fill=log10(chl_a)))+
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill="darkgrey")+
  scale_fill_gradientn(colors=cols, limits=c(-1, 1.75), )+
  theme_bw()+
  coord_fixed(1.3, xlim=lon_bound, ylim=lat_bound, expand=FALSE) #1.3 is scale

#bathymetry
#Gulf of Main
lon_bound=c(-72, -62)
lat_bound=c(39,47)

bath_m_raw= marmap::getNOAA.bathy(lon1=lon_bound[1],
                      lon2=lon_bound[2],
                      lat1=lat_bound[1],
                      lat2=lat_bound[2],
                      resolution=4) #in arcminutes
class(bath_m_raw)

