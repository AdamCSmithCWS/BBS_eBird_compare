## spatial summary of eBird trends
## 
## 
# https://ebird.github.io/ebirdst/articles/trends.html
#


library(ebirdst)
library(tidyverse)
library(bbsBayes2)
library(terra)
library(sf)


species <- "American Robin"

#species_ebird <- ebirdst::get_species(species)

regions <- bbsBayes2::load_map("prov_state")


path <- try(ebirdst_download_trends(species),
            silent = TRUE)

trends <- load_trends(species,
                      fold_estimates = TRUE) # fold estimates allow uncertainty
season <- unique(trends$season)

trends_sf <- st_as_sf(trends, 
                      coords = c("longitude", "latitude"), 
                      crs = 4326) %>% 
  sf::st_transform(.,st_crs(regions))



# National Summary --------------------------------------------------------


trends_regional_join <- trends_sf %>% 
  st_join(.,regions,
          left = FALSE) #left = FALSE drops trends outside of the regions map

trends_country <- trends_regional_join %>% 
  st_drop_geometry() %>% 
  group_by(country,fold) %>%
  summarise(weighted_abd_ppy = sum(abd * abd_ppy) / sum(abd),
            .groups = "drop") %>% 
  group_by(country) %>%
  summarise(abd_ppy_median = median(weighted_abd_ppy, na.rm = TRUE),
            abd_ppy_lower = quantile(weighted_abd_ppy, 0.10, na.rm = TRUE),
            abd_ppy_upper = quantile(weighted_abd_ppy, 0.90, na.rm = TRUE),
            .groups = "drop") %>% 
  arrange(abd_ppy_median)


trends_country



# 1-Degree grid cell summary and map --------------------------------------



latlong <- bbsBayes2::load_map("latlong")

trends_latlong_join <- trends_sf %>% 
  st_join(.,latlong,
          left = FALSE) #left = FALSE drops trends outside of the regions map

trends_latlong <- trends_latlong_join %>% 
  st_drop_geometry() %>% 
  group_by(strata_name,fold) %>%
  summarise(weighted_abd_ppy = sum(abd * abd_ppy) / sum(abd),
            .groups = "drop") %>% 
  group_by(strata_name) %>%
  summarise(abd_ppy_median = median(weighted_abd_ppy, na.rm = TRUE),
            abd_ppy_lower = quantile(weighted_abd_ppy, 0.10, na.rm = TRUE),
            abd_ppy_upper = quantile(weighted_abd_ppy, 0.90, na.rm = TRUE),
            .groups = "drop") %>% 
  arrange(abd_ppy_median)

head(trends_latlong)


# mapping -----------------------------------------------------------------


map_trends_latlong <- latlong %>% 
  inner_join(.,trends_latlong,
             by = "strata_name") %>% 
  mutate(trend = abd_ppy_median,
         survey = "eBird")

  map_ebird <- ggplot(map_trends_latlong) +
    geom_sf(aes(fill = abd_ppy_median)) +
    colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                                palette = "Blue-Red 2")+
    guides(fill = guide_colorbar(title.position = "top", barwidth = 15)) +
    labs(title = paste(species,season,"degree latlong breeding trends 2012-2022"),
         fill = "Relative abundance trend [% change / year]") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  map_ebird






