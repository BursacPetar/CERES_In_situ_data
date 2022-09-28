library(tidyverse)
library(dplyr)
library(magrittr)
library(ggplot2)
library(sf)
library(gridExtra)
library(readxl)
library(mapview)
library(ggspatial)
library(lubridate)

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


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Psenica i jabuka
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Podaci
Sys.setlocale(locale = 'Serbian (Latin)')

data_psenica <- readxl::read_excel(path = "Data/bolesti/psenica.xlsx", col_names = FALSE) %>%
  as.data.frame()
head(data_psenica)
data_psenica %<>% dplyr::rename(Dan = "...1", Mesec = "...2", Godina = "...3", Lokacija = "...4", Bolest = "...5")
data_psenica %<>% dplyr::mutate(Bolest_lat = regmatches(Bolest, gregexpr("(?<=\\().*?(?=\\))", Bolest, perl=T)))
data_psenica %<>% dplyr::mutate(Datum = make_date(Godina, Mesec, Dan))


data_jabuka <- readxl::read_excel(path = "Data/bolesti/jabuka.xlsx", col_names = FALSE) %>%
  as.data.frame()
head(data_jabuka)
data_jabuka %<>% dplyr::rename(Dan = "...1", Mesec = "...2", Godina = "...3", Lokacija = "...4", Bolest = "...5")
data_jabuka %<>% dplyr::mutate(Bolest_lat = regmatches(Bolest, gregexpr("(?<=\\().*?(?=\\))", Bolest, perl=T)))
data_jabuka %<>% dplyr::mutate(Datum = make_date(Godina, Mesec, Dan))


unique(data_psenica$Lokacija)
unique(data_jabuka$Lokacija)

# Opstine
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_opstine <- sf::st_read("Data/opstine/gadm36_SRB_2.shp", options = "ENCODING=UTF-8")
sf_opstine %<>% dplyr::mutate(NAME_2 = case_when(NAME_1 == "Grad Beograd" & NAME_2 != "Mladenovac" ~ "Beograd", 
                                                 NAME_1 != "Grad Beograd" | NAME_2 == "Mladenovac" ~ NAME_2))
sf_opstine %<>% dplyr::select(NAME_2)
sf_opstine$NAME_2

sf_opstine_1 <- sf::st_read("Data/opstine/opstine.gpkg")
sf_opstine_1 %<>% dplyr::select(NAME_2)

sf_opstine <- sf_opstine_1

# Statistika po opstinama
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

dat_summ_p <- data_psenica %>% 
  dplyr::group_by(Lokacija) %>%
  dplyr::summarise(n_bolesti = n_distinct(Bolest_lat), 
                   n_dates = n_distinct(Datum),
                   n_pts = n()) %>%
  as.data.frame()

sum(dat_summ_p$n_pts)

dat_summ_j <- data_jabuka %>% 
  dplyr::group_by(Lokacija) %>%
  dplyr::summarise(n_bolesti = n_distinct(Bolest_lat), 
                   n_dates = n_distinct(Datum),
                   n_pts = n()) %>%
  as.data.frame()

sum(dat_summ_j$n_pts)

all(dat_summ_p$Lokacija %in% sf_opstine$NAME_2)
all(dat_summ_j$Lokacija %in% sf_opstine$NAME_2)

sf_opstine$n_bolesti_p <- dat_summ_p$n_bolesti[match(sf_opstine$NAME_2, dat_summ_p$Lokacija)]
sf_opstine$n_bolesti_j <- dat_summ_j$n_bolesti[match(sf_opstine$NAME_2, dat_summ_j$Lokacija)]

sf_opstine$n_dates_p <- dat_summ_p$n_dates[match(sf_opstine$NAME_2, dat_summ_p$Lokacija)]
sf_opstine$n_dates_j <- dat_summ_j$n_dates[match(sf_opstine$NAME_2, dat_summ_j$Lokacija)]

sf_opstine$n_pts_p <- dat_summ_p$n_pts[match(sf_opstine$NAME_2, dat_summ_p$Lokacija)]
sf_opstine$n_pts_j <- dat_summ_j$n_pts[match(sf_opstine$NAME_2, dat_summ_j$Lokacija)]

sf_opstine %<>% dplyr::mutate_all(~replace(., is.na(.), 0))

# Vizuelizacija

g1 <- ggplot()+
  geom_sf(data = sf_opstine, aes(fill = n_bolesti_p))+
  coord_sf(crs = 4326) +
  labs(# title = "Karta opština", 
       subtitle = "N bolesti psenica") +# , 
       #caption = "CRS: WGS84\n CERES 2020")+
  theme_bw()+
  theme(legend.position = "bottom")

g2 <- ggplot()+
  geom_sf(data = sf_opstine, aes(fill = n_dates_p))+
  scale_fill_continuous(type = "viridis")+
  coord_sf(crs = 4326) +
  labs(#title = "Karta opština", 
       subtitle = "N datuma psenica") +#, 
       #caption = "CRS: WGS84\n CERES 2020")+
  theme_bw()+
  theme(legend.position = "bottom")

g3 <- ggplot()+
  geom_sf(data = sf_opstine, aes(fill = n_bolesti_j))+
  coord_sf(crs = 4326) +
  labs(# title = "Karta opština", 
    subtitle = "N bolesti jabuka") +# , 
  #caption = "CRS: WGS84\n CERES 2020")+
  theme_bw()+
  theme(legend.position = "bottom")

g4 <- ggplot()+
  geom_sf(data = sf_opstine, aes(fill = n_dates_j))+
  scale_fill_continuous(type = "viridis")+
  coord_sf(crs = 4326) +
  labs(#title = "Karta opština", 
    subtitle = "N datuma jabuka") +#, 
  #caption = "CRS: WGS84\n CERES 2020")+
  theme_bw()+
  theme(legend.position = "bottom")


grid.arrange(g1, g2, g3, g4, ncol = 2)



library(classInt)

pal1 <- viridisLite::viridis(6, direction = 1)
pal2 <- viridisLite::cividis(6, direction = 1)
pal3 <- viridisLite::magma(6, direction = 1)
pal4 <- rev(RColorBrewer::brewer.pal(6, "Spectral"))
pal5 <- RColorBrewer::brewer.pal(6, "Pastel2")

library(classInt)
library(ggspatial)

my_theme <- function(base_size = 10, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 13),
      plot.title = element_text(face = "bold"),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#fffcfc"),
      strip.background = element_rect(fill = "#820000", color = "#820000", size =0.5),
      strip.text = element_text(face = "bold", size = 10, color = "white"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey30", fill = NA, size = 0.5),
      legend.title=element_text(size=11),
      legend.text=element_text(size=9)
    )
}

theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = NA, color = NA),
      panel.background = element_rect(fill = NA, color = NA),
      legend.background = element_rect(fill = NA, color = NA),
      panel.border = element_blank(),
      ...
    )
}

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

classes.n_bolesti_p <- classIntervals(sf_opstine$n_bolesti_p, n = 6, style = "fisher")
brks <- round(classes.n_bolesti_p$brks, 0)
classes.n_bolesti_p <- classIntervals(sf_opstine$n_bolesti_p, n = 6, style = "fixed", fixedBreaks = brks)

sf_opstine %<>%
  mutate(percent_class_n_bolesti_p = cut(n_bolesti_p, classes.n_bolesti_p$brks, include.lowest = T))

map1 <- ggplot() +
  geom_sf(data = sf_opstine,
          aes(fill = percent_class_n_bolesti_p)) +
  scale_fill_manual(values = pal1,
                     name = "Number of diseases") +
  labs(# title = "Map - spatial position of point samples [LUCAS database]",
    subtitle = "WHEAT") +
  # theme_map() +
  # theme(legend.position = "bottom") +
  # geom_sf(data = sf_opstine, color = "black", fill = NA) +
  # guides(fill = guide_legend(reverse = TRUE))
  #   #xlab = "Longitude [°]",
    #ylab = "Latitude [°]")+
  theme(panel.grid = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="bottom")+
  # geom_sf(data = cbound_c12, color = "red", fill = NA) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(crs = 4326)+
  my_theme() +
  guides(fill = guide_legend(reverse = TRUE))

map1

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

classes.n_dates_p <- classIntervals(sf_opstine$n_dates_p, n = 6, style = "fisher")
brks <- round(classes.n_dates_p$brks, 0)
classes.n_dates_p <- classIntervals(sf_opstine$n_dates_p, n = 6, style = "fixed", fixedBreaks = brks)

sf_opstine %<>%
  mutate(percent_class_n_dates_p = cut(n_dates_p, classes.n_dates_p$brks, include.lowest = T))

map2 <- ggplot() +
  geom_sf(data = sf_opstine,
          aes(fill = percent_class_n_dates_p)) +
  scale_fill_manual(values = pal2,
                    name = "Number of dates") +
  labs(subtitle = "WHEAT") +
  # theme_map() +
  # theme(legend.position = "bottom") +
  # geom_sf(data = sf_opstine, color = "black", fill = NA) +
  # guides(fill = guide_legend(reverse = TRUE))
  # 

  # labs(# title = "Map - spatial position of point samples [LUCAS database]",
  #   subtitle = "WHEAT",
  #   xlab = "Longitude [°]",
  #   ylab = "Latitude [°]")+
  theme(panel.grid = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="bottom")+
  # geom_sf(data = sf_opstine, color = "black", fill = NA) +
  # geom_sf(data = cbound_c12, color = "red", fill = NA) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(crs = 4326)+
  my_theme() +
  guides(fill = guide_legend(reverse = TRUE))

map2

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

classes.n_bolesti_j <- classIntervals(sf_opstine$n_bolesti_j, n = 6, style = "fisher")
brks <- round(classes.n_bolesti_j$brks, 0)
classes.n_bolesti_j <- classIntervals(sf_opstine$n_bolesti_j, n = 6, style = "fixed", fixedBreaks = brks)

sf_opstine %<>%
  mutate(percent_class_n_bolesti_j = cut(n_bolesti_j, classes.n_bolesti_j$brks, include.lowest = T))

map3 <- ggplot() +
  geom_sf(data = sf_opstine,
          aes(fill = percent_class_n_bolesti_j)) +
  scale_fill_manual(values = pal1,
                    name = "Number of diseases") +
  labs(# title = "Map - spatial position of point samples [LUCAS database]",
    subtitle = "APPLE") +
  # theme_map() +
  # theme(legend.position = "bottom") +
  # geom_sf(data = sf_opstine, color = "black", fill = NA) +
  # guides(fill = guide_legend(reverse = TRUE))
  #   #xlab = "Longitude [°]",
  #ylab = "Latitude [°]")+
  theme(panel.grid = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="bottom")+
  # geom_sf(data = cbound_c12, color = "red", fill = NA) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(crs = 4326)+
  my_theme() +
  guides(fill = guide_legend(reverse = TRUE))

map3

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

classes.n_dates_j <- classIntervals(sf_opstine$n_dates_j, n = 6, style = "fisher")
brks <- round(classes.n_dates_j$brks, 0)
classes.n_dates_j <- classIntervals(sf_opstine$n_dates_j, n = 6, style = "fixed", fixedBreaks = brks)

sf_opstine %<>%
  mutate(percent_class_n_dates_j = cut(n_dates_j, classes.n_dates_j$brks, include.lowest = T))

map4 <- ggplot() +
  geom_sf(data = sf_opstine,
          aes(fill = percent_class_n_dates_j)) +
  scale_fill_manual(values = pal2,
                    name = "Number of dates") +
  labs(subtitle = "APPLE") +
  # theme_map() +
  # theme(legend.position = "bottom") +
  # geom_sf(data = sf_opstine, color = "black", fill = NA) +
  # guides(fill = guide_legend(reverse = TRUE))
  # 
  
  # labs(# title = "Map - spatial position of point samples [LUCAS database]",
  #   subtitle = "WHEAT",
  #   xlab = "Longitude [°]",
  #   ylab = "Latitude [°]")+
  theme(panel.grid = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="bottom")+
  # geom_sf(data = sf_opstine, color = "black", fill = NA) +
  # geom_sf(data = cbound_c12, color = "red", fill = NA) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(crs = 4326)+
  my_theme() +
  guides(fill = guide_legend(reverse = TRUE))

map4


gg1 <- grid.arrange(map1, map2, ncol = 2)

gg2 <- grid.arrange(map3, map4, ncol = 2)




ggsave(plot = gg1,
       filename = "Data/bolesti/Maps/Summary_map_wheat_1.jpg",
       width = 25,
       height = 20,
       units = "cm",
       device = "jpeg",
       dpi = 700)

ggsave(plot = gg2,
       filename = "Data/bolesti/Maps/Summary_map_apple_1.jpg",
       width = 25,
       height = 20,
       units = "cm",
       device = "jpeg",
       dpi = 700)


# Sample points inside polygons
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# dat_summ_p %<>% dplyr::mutate(n_pts = n_dates)
# sum(dat_summ_p$n_dates)
# sf_opstine$n_pts_p <- dat_summ_p$n_pts[match(sf_opstine$NAME_2, dat_summ_p$Lokacija)]
# sf_opstine %<>% dplyr::mutate_all(~replace(., is.na(.), 0))
# dim(data_psenica)

sum(sf_opstine$n_pts_p)
sum(sf_opstine$n_pts_j)

pts_p <- st_sample(sf_opstine, size = sf_opstine$n_pts_p, by_polygon = TRUE, type = "random", exact = TRUE) %>%
  st_as_sf()

pts_j <- st_sample(sf_opstine, size = sf_opstine$n_pts_j, by_polygon = TRUE, type = "random", exact = TRUE) %>%
  st_as_sf()

# mapview(pts_p) + mapview(sf_opstine)

pts_p %<>% 
  st_join(., sf_opstine, join = st_within)

pts_j %<>% 
  st_join(., sf_opstine, join = st_within)


data_psenica %<>% arrange(Lokacija)
data_jabuka %<>% arrange(Lokacija)
pts_p %<>% arrange(NAME_2)
pts_j %<>% arrange(NAME_2)

pts_p %<>% dplyr::mutate(Bolest_lat = data_psenica$Bolest_lat, Godina = data_psenica$Godina)
pts_j %<>% dplyr::mutate(Bolest_lat = data_jabuka$Bolest_lat, Godina = data_jabuka$Godina)

pts_p %<>% dplyr::mutate(Bolest_lat = unlist(Bolest_lat)) 
pts_j %<>% dplyr::mutate(Bolest_lat = unlist(Bolest_lat)) 

# st_write(pts_p, "Data/bolesti/psenica_tac.gpkg")
# st_write(pts_j, "Data/bolesti/jabuka_tac.gpkg")


pts_p <- st_read("Data/bolesti/psenica_tac.gpkg")
pts_j <- st_read("Data/bolesti/jabuka_tac.gpkg")

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

pts_p %<>% dplyr::mutate(Bolest_lat = firstup(Bolest_lat))
pts_j %<>% dplyr::mutate(Bolest_lat = firstup(Bolest_lat))


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

pts_p_19 <- pts_p %>% dplyr::filter(Godina == 2019)
pts_p_20 <- pts_p %>% dplyr::filter(Godina == 2020)

paln <- viridisLite::viridis(length(unique(pts_p_19$Bolest_lat)), direction = 1)

map11 <- ggplot() +
  geom_sf(data = pts_p_19,
          aes(color = factor(Bolest_lat)), size = 3.5, show.legend = "point") +
  scale_color_manual(values = paln,
                    name = "Diseases: ", 
                    guide = guide_legend(override.aes = list(size = 3.5))) +
  labs(subtitle = "WHEAT - 2019 year") +
  geom_sf(data = sf_opstine, color = "black", fill = NA) +
  theme(panel.grid = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = "right")+
  # geom_sf(data = sf_opstine, color = "black", fill = NA) +
  # geom_sf(data = cbound_c12, color = "red", fill = NA) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(crs = 4326)+
  my_theme() +
  theme(legend.position = "right") +
  guides(size=FALSE, alpha = FALSE) +
  theme(plot.subtitle=element_text(size = 14, face = "bold", color = "black"))

map11

paln <- viridisLite::viridis(length(unique(pts_p_20$Bolest_lat)), direction = 1)

map12 <- ggplot() +
  geom_sf(data = pts_p_20,
          aes(color = factor(Bolest_lat)), size = 3.5, show.legend = "point") +
  scale_color_manual(values = paln,
                     name = "Diseases: ", 
                     guide = guide_legend(override.aes = list(size = 3.5))) +
  labs(subtitle = "WHEAT - 2020 year") +
  geom_sf(data = sf_opstine, color = "black", fill = NA) +
  theme(panel.grid = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = "right")+
  # geom_sf(data = sf_opstine, color = "black", fill = NA) +
  # geom_sf(data = cbound_c12, color = "red", fill = NA) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(crs = 4326)+
  my_theme() +
  theme(legend.position = "right") +
  guides(size=FALSE, alpha = FALSE) +
  theme(plot.subtitle=element_text(size = 14, face = "bold", color = "black"))

map12

ggg1 <- grid.arrange(map11, map12, ncol = 2)

ggsave(plot = ggg1,
       filename = "Data/bolesti/Maps/Diseases_map_wheat.jpg",
       width = 45,
       height = 30,
       units = "cm",
       device = "jpeg",
       dpi = 700)

ggsave(plot = map11,
       filename = "Data/bolesti/Maps/Diseases_map_wheat_2019.jpg",
       width = 45,
       height = 30,
       units = "cm",
       device = "jpeg",
       dpi = 700)
ggsave(plot = map12,
       filename = "Data/bolesti/Maps/Diseases_map_wheat_2020.jpg",
       width = 45,
       height = 30,
       units = "cm",
       device = "jpeg",
       dpi = 700)
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

pts_j_19 <- pts_j %>% dplyr::filter(Godina == 2019)
pts_j_20 <- pts_j %>% dplyr::filter(Godina == 2020)

paln <- viridisLite::turbo(length(unique(pts_j_19$Bolest_lat)), direction = 1)

map11 <- ggplot() +
  geom_sf(data = pts_j_19,
          aes(color = factor(Bolest_lat)), size = 3.5, show.legend = "point") +
  scale_color_manual(values = paln,
                     name = "Diseases: ", 
                     guide = guide_legend(override.aes = list(size = 3.5))) +
  labs(subtitle = "APPLE - 2019 year") +
  geom_sf(data = sf_opstine, color = "black", fill = NA) +
  theme(panel.grid = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = "right")+
  # geom_sf(data = sf_opstine, color = "black", fill = NA) +
  # geom_sf(data = cbound_c12, color = "red", fill = NA) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(crs = 4326)+
  my_theme() +
  theme(legend.position = "right") +
  guides(size=FALSE, alpha = FALSE) +
  theme(plot.subtitle=element_text(size = 14, face = "bold", color = "black"))

map11

paln <- viridisLite::turbo(length(unique(pts_j_20$Bolest_lat)), direction = 1)

map12 <- ggplot() +
  geom_sf(data = pts_j_20,
          aes(color = factor(Bolest_lat)), size = 3.5, show.legend = "point") +
  scale_color_manual(values = paln,
                     name = "Diseases: ", 
                     guide = guide_legend(override.aes = list(size = 3.5))) +
  labs(subtitle = "APPLE - 2020 year") +
  geom_sf(data = sf_opstine, color = "black", fill = NA) +
  theme(panel.grid = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = "right")+
  # geom_sf(data = sf_opstine, color = "black", fill = NA) +
  # geom_sf(data = cbound_c12, color = "red", fill = NA) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(crs = 4326)+
  my_theme() +
  theme(legend.position = "right") +
  guides(size=FALSE, alpha = FALSE) +
  theme(plot.subtitle=element_text(size = 14, face = "bold", color = "black"))

map12

ggg2 <- grid.arrange(map11, map12, ncol = 2)

ggsave(plot = map11,
       filename = "Data/bolesti/Maps/Diseases_map_apple_2019.jpg",
       width = 45,
       height = 30,
       units = "cm",
       device = "jpeg",
       dpi = 700)
ggsave(plot = map12,
       filename = "Data/bolesti/Maps/Diseases_map_apple_2020.jpg",
       width = 45,
       height = 30,
       units = "cm",
       device = "jpeg",
       dpi = 700)


# Grad - karta
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

grad_data <- readxl::read_xlsx(path = "Data/grad/grad.xlsx", col_names = FALSE) %>% 
  as.data.frame()

grad_data %<>% dplyr::rename(Datum = `...1`, Lokacija = "...2")

grad_data %<>% dplyr::mutate(Godina = str_sub(Datum, -5, -2))


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

geonames <- sf::st_read(dsn = "Data/geonames/GeoNames_SRB.gpkg")
geonames_names <- geonames %>% dplyr::filter(field_8 == "LCTY" | grepl("PPL", field_8, fixed = TRUE)) %>% 
  dplyr::filter(field_2 != "Rusulijske Ravnine")

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

all(grad_data$Lokacija %in% geonames_names$field_2)
grad_data %>% dplyr::filter(!(Lokacija %in% geonames_names$field_2))


grad_data$Lokacija[grad_data$Lokacija == "Raški okrug"] <- "Raška"
grad_data$Lokacija[grad_data$Lokacija == "Beograd"] <- "Stari Grad"

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# field_5 = lat  field_6 = lat
grad_data$lat <- geonames_names$field_5[match(grad_data$Lokacija, geonames_names$field_2)]
grad_data$lon <- geonames_names$field_6[match(grad_data$Lokacija, geonames_names$field_2)]

grad_data_dup <- grad_data %>% 
  add_count(Lokacija) %>% #add new column that has the frequency counts. Non duplicate will have n=1
  filter(n>1) %>%  # remove non duplicates
  select(-n)

runif(length(grad_data_dup$Lokacija), min = 0.00800, 0.00900)

grad_data_dup %<>% dplyr::mutate(lat = lat + runif(length(grad_data_dup$Lokacija),  min = 0.1, max = 0.5),
                                 lon = lon + runif(length(grad_data_dup$Lokacija),  min = 0.1, max = 0.5))


grad_data_non_dup <- grad_data %>% 
  add_count(Lokacija) %>% #add new column that has the frequency counts. Non duplicate will have n=1
  filter(n==1) %>%  # remove non duplicates
  select(-n)

grad_data_full <- rbind(grad_data_dup, grad_data_non_dup)


grad_sf <- st_as_sf(grad_data_full, coords = c("lon", "lat"), crs = 4326)
mapview(grad_sf)



paln <- viridisLite::turbo(length(unique(grad_sf$Godina)), direction = 1)

map12 <- ggplot() +
  geom_sf(data = sf_opstine, color = "black", fill = NA) +
  geom_sf(data = grad_sf,
          aes(color = factor(Godina)), size = 3.5, show.legend = "point") +
  scale_color_manual(values = paln,
                     name = "Year of hail occurence: ", 
                     guide = guide_legend(override.aes = list(size = 3.5))) +
  labs(subtitle = "HAIL occurence") +
  
  theme(panel.grid = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = "right")+
  # geom_sf(data = sf_opstine, color = "black", fill = NA) +
  # geom_sf(data = cbound_c12, color = "red", fill = NA) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(crs = 4326)+
  my_theme() +
  theme(legend.position = "right") +
  guides(size=FALSE, alpha = FALSE) +
  theme(plot.subtitle=element_text(size = 14, face = "bold", color = "black"))

map12


ggsave(plot = map12,
       filename = "Data/grad/Maps/Hail_occurence.jpg",
       width = 45,
       height = 30,
       units = "cm",
       device = "jpeg",
       dpi = 700)
