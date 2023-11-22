#Fetch_ebird_trends

library(ebirdst)
library(tidyverse)
library(bbsBayes2)
library(terra)
library(sf)

map_single_program <- FALSE

species <- "Wood Thrush"

species <- "Louisiana Waterthrush"

species <- "Eastern Bluebird"

species <- "Western/Eastern Cattle Egret"

species_1 <- c("Eastern Bluebird",
             "Cooper's Hawk",
             #"Mourning Dove",
             "Carolina Wren")

for(species in species_1){


path <- ebirdst_download_trends(species)

trends <- load_trends(species,
                      fold_estimates = TRUE)

trends_sf <- st_as_sf(trends, 
                      coords = c("longitude", "latitude"), 
                      crs = 4326)

latlong <- bbsBayes2::load_map("latlong") %>% 
  st_transform(.,st_crs(trends_sf))


trends_latlong_join <- trends_sf %>% 
  st_join(.,latlong,
          left = FALSE)

trends_latlong <- trends_latlong_join %>% 
  st_drop_geometry() %>% 
  group_by(strata_name) %>%
  summarise(abd_ppy_median = median(abd_ppy, na.rm = TRUE),
            abd_ppy_lower = quantile(abd_ppy, 0.10, na.rm = TRUE),
            abd_ppy_upper = quantile(abd_ppy, 0.90, na.rm = TRUE),
            .groups = "drop") %>% 
  arrange(abd_ppy_median)


map_trends_latlong <- latlong %>% 
  inner_join(.,trends_latlong,
             by = "strata_name") %>% 
  mutate(trend = abd_ppy_median,
         survey = "eBird")
if(map_single_program){
map_ebird <- ggplot(map_trends_latlong) +
  geom_sf(aes(fill = abd_ppy_median)) +
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 2")+
  guides(fill = guide_colorbar(title.position = "top", barwidth = 15)) +
  labs(title = paste(species,"degree latlong breeding trends 2012-2022"),
       fill = "Relative abundance trend [% change / year]") +
  theme_bw() +
  theme(legend.position = "bottom")

map_ebird
}

if(species == "Western/Eastern Cattle Egret"){
  species <- "Cattle Egret"
}
# load BBS results --------------------------------------------------------
aou <- as.integer(search_species(species)["aou"])
fit <- readRDS(paste0("output/fit_",aou,".rds"))

# indices <- generate_indices(fit,hpdi = TRUE)
#  saveRDS(indices,
#          file = paste0("output/inds_",aou,".rds"))

 indices <- readRDS(paste0("output/inds_",aou,".rds"))
 traj <- plot_indices(indices,
                      add_observed_means = TRUE)

 trends <- generate_trends(indices,
                          slope = TRUE,hpdi = TRUE)

map_trends_bbs <- latlong %>% 
  inner_join(.,trends$trends,
             by = c("strata_name" = "region")) %>% 
  mutate(survey = "BBS")
# 
if(map_single_program){
  
map_bbs <- ggplot(map_trends_bbs) +
  geom_sf(aes(fill = trend)) +
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 2")+
  guides(fill = guide_colorbar(title.position = "top", barwidth = 15)) +
  labs(title = paste(species,"degree latlong breeding trends 2012-2022"),
       fill = "Relative abundance trend [% change / year]") +
  theme_bw() +
  theme(legend.position = "bottom")

map_bbs
}


map_trends_both <- bind_rows(map_trends_latlong,map_trends_bbs)

map <- ggplot(map_trends_both) +
  geom_sf(aes(fill = trend)) +
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 2")+
  guides(fill = guide_colorbar(title.position = "top", barwidth = 15)) +
  labs(title = paste(species,"degree latlong breeding trends 2012-2022"),
       fill = "Relative abundance trend [% change / year]") +
  theme_bw() +
  theme(legend.position = "bottom")+
  facet_wrap(vars(survey))

pdf(file = paste0("Figures/trend_comparison_",species,".pdf"),
    width = 11,
    height = 8.5)
print(map)
dev.off()
}
