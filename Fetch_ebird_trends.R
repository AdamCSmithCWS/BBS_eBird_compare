#Fetch_ebird_trends

library(ebirdst)
library(tidyverse)
library(bbsBayes2)
library(terra)
library(sf)

map_single_program <- FALSE

sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE) %>% 
  mutate(sp_ebird <- ebirdst::get_species(english)) %>% 
  na.omit()

species_1 <- which(sp_list$vm < 6,"english")
calcu_indices <- FALSE

pdf(file = paste0("Figures/trend_comparisons.pdf"),
    width = 11,
    height = 8.5)

for(i in species_1){

  species <- as.character(sp_list[i,"english"])
  aou <- search_species(species)["aou"]
  aou <- as.integer(aou[1,])
  if(!file.exists(paste0("output/fit_",aou,".rds"))){next}
  
path <- try(ebirdst_download_trends(species),
            silent = TRUE)
if(class(path) == "try-error"){next}
trends <- load_trends(species,
                      fold_estimates = TRUE)
season <- unique(trends$season)

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
  labs(title = paste(species,season,"degree latlong breeding trends 2012-2022"),
       fill = "Relative abundance trend [% change / year]") +
  theme_bw() +
  theme(legend.position = "bottom")

map_ebird
}




# if(species == "Western/Eastern Cattle Egret"){
#   species <- "Cattle Egret"
# }
# load BBS results --------------------------------------------------------
## these results are produced using the "bbs_script.R" and the 
## package bbsBayes2. 
fit <- readRDS(paste0("output/fit_",aou,".rds"))

if(!file.exists(paste0("output/inds_",aou,".rds"))){
indices <- generate_indices(fit,hpdi = TRUE)
 saveRDS(indices,
         file = paste0("output/inds_",aou,".rds"))
}else{
 indices <- readRDS(paste0("output/inds_",aou,".rds"))
}
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

### apply consistent colour palette and break


breaks <- c(-7, -4, -2, -1, -0.5, 0.5, 1, 2, 4, 7)
labls <- c(paste0("< ", breaks[1]),
           paste0(breaks[-c(length(breaks))],":", breaks[-c(1)]),
           paste0("> ",breaks[length(breaks)]))
labls <- paste0(labls, " %")

map_trends_both$t_plot <- cut(map_trends_both$trend, breaks = c(-Inf, breaks, Inf),
                     labels = labls)

pal <- stats::setNames(
  c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf",
    "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"),
  levels(map_trends_both$t_plot))


map <- ggplot(map_trends_both) +
  geom_sf(aes(fill = t_plot)) +
  # colorspace::scale_fill_continuous_diverging(rev = TRUE,
  #                                             palette = "Blue-Red 2")+
  scale_fill_manual(values = pal)+
  #guides(fill = guide_colorbar(title.position = "top", barwidth = 15)) +
    guides(fill = guide_legend(title.position = "top")) +
    labs(title = paste(species,season,"degree latlong breeding trends 2012-2022"),
       fill = "Trend [%/year]") +
  theme_bw() +
  theme(legend.position = "right")+
  facet_wrap(vars(survey))


print(map)

}


dev.off()

