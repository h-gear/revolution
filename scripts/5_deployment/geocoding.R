library(sf)
library(tidygeocoder)
library(mapview)
library(tigris)
library(tidyverse)

# See also:
# https://rdrr.io/cran/tigris/man/states.html
# https://github.com/walkerke/tigris

# read founders online data with information on coordinates
letter_ss <- readRDS(file = "data/processed/founders/letters_geo_ref.rds")
glimpse(letter_ss)

# TODO: insert coordinates of published books -> check whether there are hubs

# combine city, stat,e and country information together to obtain proper coordinates
letter_ss <- letter_ss %>%
  mutate(city2 = case_when(!is.na(state) ~ str_c(city, ", ", state),
                           TRUE ~ city)) %>%

  mutate(city3 = case_when(!is.na(country) ~ str_c(city2, ", ", country),
                           TRUE ~ city2))

latlon_tbl <- letter_ss %>%
geocode(
    address = city3,
    method = "arcgis"
)

# remove cases where thereis no information on longitude and lattitude
latlon_tbl2 <- latlon_tbl %>% filter(!is.na(lat) & !is.na(long))

# simple features
df_2 <- latlon_tbl2 %>%
    st_as_sf(
        coords = c("long","lat"),
        crs    =     4326
)

df_2 <- df_2 %>%
  group_by(city3) %>%
  summarize(n = n())

#df_2_yr %>% mapview(zcol = "start.year", legend = TRUE)
#mapview(df_2_yr, zcol = "district") + mapview(breweries, zcol = "founded")

mapview(df_2,
    cex = "n",
    col.regions = "yellow",
    alpha.regions = 0.5,
    legend = FALSE
  ) # +

 # add mapview with information on evans
 mapview(evans,
    cex = "n",
    col.regions = "blue",
    alpha.regions = 0.5,
    legend = FALSE,
    #zcol = "founded"
  )


# Specify visualization for us state and clusters ----

# pull in shape files, work with poylgons
us_states_sf <- tigris::states() %>% st_set_crs(4326)

# plot points in us states to see the amount of letters per us-state ----
plt <- df_2 %>%

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

plt %>%
    mapview(
        zcol       = "total_letters",
        color      = "white",
        map.types  = "CartoDB.DarkMatter",
        layer.name = "Amount of Letters"
    )

# K means spatial clustering and segments ----
set.seed(123)

# for the moment, we assume two clusters representing the United States and Europe
kmeans_obj <- df_2 %>%
    #st_distance() %>%
    st_coordinates() %>%
    as_tibble() %>%
    kmeans(centers = 2, nstart = 20)

kmeans_obj$cluster

df_2 %>%
    mutate(cluster = kmeans_obj$cluster %>% factor()) %>%
    mapview(
        zcol = "cluster",
        color = "white",
        map.types = "CartoDB.DarkMatter",
        layer.name = "Geospatial segments"
    )


