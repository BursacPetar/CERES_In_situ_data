library(tidyverse)
library(dplyr)
library(magrittr)
library(ggplot2)
library(sf)
library(gridExtra)
library(readxl)
library(mapview)
library(ggspatial)

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Bolesti biljaka
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# data <- read.csv(file = "Data/bolesti.csv", 
#                  header = FALSE, 
#                  sep = " ", 
#                  stringsAsFactors = FALSE) %>% 
#   as.data.frame()
# 
# 
# data

# Podaci
Sys.setlocale(locale = 'Serbian (Latin)')
data <- readxl::read_excel(path = "Data/bolesti.xlsx", col_names = FALSE) %>%
  as.data.frame()

data %<>% dplyr::rename(Kultura = "...1", Datum = "...2", Opstina = "...3", Bolest = "...4")

# Opstine
sf_opstine <- sf::st_read("Data/opstine/gadm36_SRB_2.shp", options = "ENCODING=UTF-8")
sf_opstine %<>% dplyr::mutate(NAME_2 = case_when(NAME_1 == "Grad Beograd" & NAME_2 != "Mladenovac" ~ "Beograd", 
                                                 NAME_1 != "Grad Beograd" | NAME_2 == "Mladenovac" ~ NAME_2))
sf_opstine %<>% dplyr::select(NAME_2)


dat1 <- data %>% dplyr::filter(Opstina == "Svi regioni")
dat2 <- data %>% dplyr::filter(Opstina != "Svi regioni")

# Statistika po opstinama
dat_summ <- dat2 %>% 
  dplyr::group_by(Opstina) %>%
  dplyr::summarise( n_bolesti = n_distinct(Bolest), n_dates = n_distinct(Datum)) %>%
  as.data.frame()

all(dat_summ$Opstina %in% sf_opstine$NAME_2)
sf_opstine$n_bolesti <- dat_summ$n_bolesti[match(sf_opstine$NAME_2, dat_summ$Opstina)]
sf_opstine$n_dates <- dat_summ$n_dates[match(sf_opstine$NAME_2, dat_summ$Opstina)]
sf_opstine %<>% dplyr::mutate_all(~replace(., is.na(.), 0))

# Vizuelizacija
g1 <- ggplot()+
  geom_sf(data = sf_opstine, aes(fill = n_bolesti))+
  coord_sf(crs = 4326) +
  labs(title = "Karta opština", subtitle = "N bolesti", caption = "CRS: WGS84\n CERES 2020")+
  theme_bw()+
  theme(legend.position = "bottom")

g2 <- ggplot()+
  geom_sf(data = sf_opstine, aes(fill = n_dates))+
  scale_fill_continuous(type = "viridis")+
  coord_sf(crs = 4326) +
  labs(title = "Karta opština", subtitle = "N datuma", caption = "CRS: WGS84\n CERES 2020")+
  theme_bw()+
  theme(legend.position = "bottom")

grid.arrange(g1, g2, ncol = 2)


m1 <- mapview(sf_opstine, zcol = "n_bolesti")
m2 <- mapview(sf_opstine, zcol = "n_dates")
library(leafsync)
sync(m1, m2, ncol = 2)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Terenski rezultati
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# geonames
geonames <- sf::st_read(dsn = "Data/geonames/GeoNames_SRB.gpkg")

names(geonames)
unique(geonames$field_8)
mapview(geonames)

# LCTY	locality	a minor area or place of unspecified or mixed character and indefinite boundaries

#geonames_ostalo <- geonames %>% dplyr::filter(field_8 != "LCTY")
#geonames_loc <- geonames %>% dplyr::filter(field_8 == "LCTY")

# P city, village,...
#geonames_ppl <- geonames %>% dplyr::filter(., grepl("PPL", field_8, fixed = TRUE))

# Objedinjeno
geonames_names <- geonames %>% dplyr::filter(field_8 == "LCTY" | grepl("PPL", field_8, fixed = TRUE)) %>% 
  dplyr::filter(field_2 != "Rusulijske Ravnine")



# Voronoi tesselation for each element of the list
#voronoi_grids_lst <- lapply(points_lst, st_voronoi)

# Voronoi - Thiessen polygons
geonames_names_32634 <- st_transform(geonames_names, 32634)

srb_granica <- st_read(dsn = "Data/geonames/gadm36_SRB_0.shp") %>%
  st_transform(32634)

#voronoi <- st_voronoi(geonames_names_32634, srb_granica)
#mapview(voronoi)

# compute Voronoi polygons
voronoi_grid <- geonames_names_32634 %>% 
  st_geometry() %>% 
  do.call(c, .) %>% 
  st_voronoi() %>% 
  st_collection_extract() %>% 
  st_set_crs(32634)

plot(voronoi_grid)

# final 
v_poly <- st_cast(voronoi_grid) %>% 
  st_intersection(srb_granica) %>%
  st_sf() %>% 
  st_join(geonames_names_32634, join = st_contains) #za preklop sa tackama

v_poly %<>% dplyr::select(field_2, field_8) %>% dplyr::rename(Naziv = field_2, code = field_8)


ggplot()+
  geom_sf(data = v_poly, aes(fill = code))


mapview(v_poly)
# Žednik npr pronalzi kod Sombora a u podacima je Žednik opština Subotica
mapview(v_poly %>% dplyr::filter(Naziv == "Stari Žednik" | Naziv == "Novi Žednik")) + mapview(sf_opstine)



