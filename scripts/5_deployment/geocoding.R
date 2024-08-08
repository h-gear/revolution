# 0. GOAL ----
# Mapping the geospatial distribution of the letters in the dataset

# 1. LIBRARIES ----
library(tidyverse)
library(sf)
library(tidygeocoder)
library(mapview)
library(tigris) # https://rdrr.io/cran/tigris/man/states.html
                # https://github.com/walkerke/tigris

# 2. DATA ----
# read Founders Online data with information on coordinates
founders <- readRDS(file = "data/processed/founders/founders_geo_ref.rds")

# read ecco data with information on coordinates
ecco <- readRDS(file = "data/processed/ecco/ecco_geo_ref.rds") %>%
        rename(city = place_of_publication)

# read evans data with information on coordinates
#evans <- readRDS(file = "data/processed/evans/evans_geo_ref.rds")

# combine city, state and country information together to obtain the coordinates
# via tidygeocoder
founders <- founders %>%
  mutate(city2 = case_when(!is.na(state) ~ str_c(city, ", ", state),
                           TRUE ~ city)) %>%
  mutate(city3 = case_when(!is.na(country) ~ str_c(city2, ", ", country),
                           TRUE ~ city2))

ecco <- ecco %>%
  mutate(city2 = case_when(!is.na(state) ~ str_c(city, ", ", state),
                           TRUE ~ city)) %>%
  mutate(city3 = case_when(!is.na(country) ~ str_c(city2, ", ", country),
                           TRUE ~ city2))

# 3. GEOCODING ----
# obtain the coordinates via tidygeocoder (in addition to the coordinates we
# already had)
latlon_founders <- founders %>%
geocode(
    address = city3,  # contains city name, state and country, if available
    method = "arcgis" # ArcGIS single address geocoder
)

latlon_ecco <- ecco %>%
  geocode(
    address = city3,  # contains city name, state and country, if available
    method = "arcgis" # ArcGIS single address geocoder
  )

# update coordinates in case tidygeocoder did not find any coordinates but we
# had them in the dataset already
latlon_founders <- latlon_founders %>%
  mutate(lat = if_else(is.na(lat) & !is.na(latitude), latitude, lat),
         long = if_else(is.na(long) & !is.na(longitude), longitude, long)) %>%
  select(-latitude, -longitude)

latlon_ecco <- latlon_ecco %>%
  mutate(lat = if_else(is.na(lat) & !is.na(latitude), latitude, lat),
         long = if_else(is.na(long) & !is.na(longitude), longitude, long)) %>%
  select(-latitude, -longitude)

# remove cases where there is no information on longitude and latitude
latlon_founders <- latlon_founders %>% filter(!is.na(lat) & !is.na(long))
latlon_ecco     <- latlon_ecco %>% filter(!is.na(lat) & !is.na(long))

# simple features, convert coordinates to an sf object
latlon_founders <- latlon_founders %>%
    st_as_sf(
        coords = c("long","lat"),
        crs    =     4326)

latlon_ecco <- latlon_ecco %>%
  st_as_sf(
    coords = c("long","lat"),
    crs    =     4326)

# group by city3 and count the number of letters per city to get an idea of the
# distribution of the letters and the degree of being a hub
latlon_founders <- latlon_founders %>%
  group_by(city3) %>%
    summarize(n = n()) %>%
  ungroup()

latlon_ecco <- latlon_ecco %>%
  group_by(city3) %>%
    summarize(n = n()) %>%
  ungroup()

# 4. VISUALIZING ----
## 4.1 View spatial objects interactively on a world map ----
mapview(latlon_founders,
    cex = "n",
    col.regions = "yellow",
    alpha.regions = 0.5,
    legend = T) +

 # add mapview with information on ecco
 mapview(latlon_ecco,
    cex = "n",
    col.regions = "blue",
    alpha.regions = 0.5,
    legend = T)

## 4.2 Specify visualization for us state ----
# pull in shape files, work with poylgons
us_states_sf <- tigris::states() %>% st_set_crs(4326)

# plot points in us-states to see the amount of letters per us-state ----
plt <- latlon_founders %>%

    # spatial join
    st_join(us_states_sf %>% select(GEOID)) %>%

    # convert to tibble & summarize by GEOOID
    as_tibble() %>%
    group_by(GEOID) %>%
      summarise(total_letters = n()
    ) %>%
    ungroup() %>%

    # Join by GEOID and convert back to sf
    right_join(us_states_sf, by = "GEOID") %>%
    select(GEOID:total_letters,NAME, geometry) %>%
    st_as_sf(crs = 4326)

# plot the amount of letters sent per us-state
plt %>%
    mapview(
        zcol       = "total_letters",
        color      = "white",
        map.types  = "CartoDB.DarkMatter",
        layer.name = "Amount of Letters"
    )

## 4.3 Visualization with K-means spatial clustering ----
set.seed(123)

# for the moment, we assume two clusters representing the United States and Europe
kmeans_obj <- latlon_founders %>%
    #st_distance() %>%
    st_coordinates() %>%
    as_tibble() %>%
    kmeans(centers = 8, nstart = 20)

kmeans_obj$cluster

# visualize the clusters
latlon_founders %>%
    mutate(cluster = kmeans_obj$cluster %>% factor()) %>%
    mapview(
        zcol       = "cluster",
        color      = "white",
        cex        = "n",
        map.types  = "CartoDB.DarkMatter",
        layer.name = "Geospatial segments"
    )

