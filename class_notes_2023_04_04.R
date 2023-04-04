#2023-04-04
#EMB-AIS


library(tidyverse)
library(sf)
library(mapdata)
library(marmap)
library(lubridate)

lat_bounds=c(25, 34)
lon_bounds=c(-82, -76)


ais_day= read.csv("data/processed_ais/ais_2017-01-25.csv")
head(ais_day)

USA_crit_hab= st_read("data/North_Atlantic_Right_Whale_Critical_Habitat/",
                      "North_Atlantic_Right_Whale_Critical_Habitat")
USA_crit_hab

world_map=map_data("worldHires", xlim=lon_bounds, ylim=lat_bounds)
head(world_map)

ais_map_pts= ggplot()+
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group))+
  geom_sf(data=USA_crit_hab, fill="yellow", alpha=0.5)+
  geom_point(data=ais_day, aes(x=LON, y=LAT, color=CallSign), size=0.5)+
  coord_sf(1.3, xlim=lon_bounds, ylim=lat_bounds)+
  guides(color="none")+
  theme_classic()

head(ais_day)
USA_crit_hab

ships_RW_intersect=ais_day %>%
  st_as_sf(coords=c("LON", "LAT"), crs=4269)%>%
  st_intersection(USA_crit_hab%>% dplyr::select(geometry))

ships_RW_intersect

#collapse points into lines

law_breakers= ships_RW_intersect %>%
  filter(Length> 20)%>% #units in meters, law applies to boats > 65 feet in length
  filter(SOG>10)# speed over ground 

dim(law_breakers)

unique(law_breakers$CallSign)
unique(law_breakers$VesselName)

head(law_breakers)

illegal_paths= law_breakers %>%
  mutate(date_time= lubridate::ymd_hms(BaseDateTime))%>%
  arrange(date_time)%>%
  group_by(CallSign)%>%
  summarize(do_union=FALSE)%>% #created multipoint
  st_cast("LINESTRING")%>%
  st_make_valid()
  
illegal_path_lengths= illegal_paths %>%
  mutate(track_length_m= st_length(geometry))

glimpse(illegal_path_lengths)
str(illegal_path_lengths)
sum(illegal_path_lengths$track_length_m)








