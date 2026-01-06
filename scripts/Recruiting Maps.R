library(cfbfastR)
library(tidyverse)

info <- cfbd_team_info()


p12 <- map_df(c("Boise State", "Colorado State", "Utah State", "Fresno State", "San Diego State"), ~ cfbd_recruiting_player(year = 2026, team = .x)) %>% 
  rename(team = committed_to) %>% 
  left_join(info %>% select(school, logo), by = c("team" = "school")) %>%
  mutate(conference = "P12")

mw <- map_df(c("Air Force", "Hawai'i", "Nevada", "New Mexico", "San Jose State", "UNLV", "Wyoming", "Northern Illinois", "UTEP"), ~ cfbd_recruiting_player(year = 2026, team = .x)) %>% 
  rename(team = committed_to) %>% 
  left_join(info %>% select(school, logo), by = c("team" = "school")) %>%
  mutate(conference = "MW")


combined <- bind_rows(p12, mw)

combined2 <- combined %>%
  filter(
    !is.na(hometown_info_longitude),
    !is.na(hometown_info_latitude)
  ) %>%
  st_as_sf(
    coords = c("hometown_info_longitude", "hometown_info_latitude"),
    crs = 4326
  ) %>% 
  shift_geometry() %>% 
  st_transform(crs = 4326)


# Map Files ---------------------------------------------------------------

library(sf)
library(tigris)

states <- states(cb =T, resolution = "20m") %>% 
  shift_geometry() %>% 
  filter(STUSPS != "PR") %>% 
  st_transform(crs = 4326)

counties <- counties(cb = T, resolution = "20m") %>% 
  shift_geometry() %>% 
  filter(STUSPS != "PR") %>% 
  st_transform(crs = 4326)


# Make Map? ---------------------------------------------------------------

library(leaflet)
library(leaflet.extras)


# Reprojection
epsg2163 <- leaflet::leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "ESRI:102003",
  proj4def = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
  resolutions = 2^(16:7)
)

# The leaflet map
m <- leaflet::leaflet(options = leaflet::leafletOptions(crs = epsg2163,
                                                        zoomControl = TRUE,
                                                        zoomSnap = 0.25,
                                                        zoomDelta = 1),
                      height = 4000,
                      width = 6400) |>
  leaflet::setView(lng = -98.64580,
                   lat = 38.05909,
                   zoom = 6) |>
  # leaflet::addPolylines(data = counties,
  #                       color = "grey",
  #                       weight = 0.25,
  #                       smoothFactor = 0,
  #                       opacity = 0.75)  |>
  leaflet::addPolylines(data = states,
                        color = "grey",
                        weight = 2,
                        smoothFactor = 0,
                        opacity = 1) |>
  addMarkers(
    data = combined2,
    icon = ~makeIcon(
      iconUrl = logo,
      iconWidth = 150,
      iconHeight = 150
    )
  ) |>
  setMapWidgetStyle(list(background = "white")) 

output_file <- "pacmw.png"

htmlwidgets::saveWidget(m, "temp.html", selfcontained = F)

webshot2::webshot(url = "temp.html",
                  file = output_file,
                  vwidth = 6400,
                  vheight = 4000)  
  
  

  