## testing bbsBayes2 parallel in HRE env

library(bbsBayes2)
library(tidyverse)
library(foreach)
library(doParallel)


setwd("C:/github/BBS_ebird_compare")


sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE) %>% 
  mutate(sp_ebird <- ebirdst::get_species(english)) %>% 
  na.omit()

#species <- which(sp_list$vm %in% c(4,5))
species <- 1:nrow(sp_list)


re_run <- FALSE # set to true to re-run any species that have results in output folder

# build cluster -----------------------------------------------------------
# allowing multiple species at once if > 8 cores are available to run
# 

n_species <- floor((detectCores()-1)/4) # requires 4 cores per species
  
cluster <- makeCluster(n_species, type = "PSOCK")
registerDoParallel(cluster)


test <- foreach(i = species, #nrow(sp_list),
                .packages = c("bbsBayes2",
                              "tidyverse",
                              "cmdstanr"),
                .errorhandling = "pass") %dopar%
  {
    
    
#for(sp in species[c(4,3)]){#
 
#i = which(sp_list[,"english"] == sp)
sp = sp_list[i,"english"]

aou <- as.integer(sp_list[i,"aou"])
 if(!file.exists(paste0("output/fit_",aou,".rds")) | re_run){

   fy <- 2012

   strat <- "latlong"

   s <- stratify(by = strat,
              species = sp,
              quiet = TRUE) %>%
  prepare_data(min_n_routes = 1,
               min_max_route_years = 1,
               quiet = TRUE,
               min_year = fy)

   ## bbsBayes2 models do not currently work unless n_strata > 1
   if(nrow(s$meta_strata) == 1){stop(paste("Only 1 stratum for",sp,"skipping to next species"))}

   if(nrow(s$meta_strata) > 2){ #spatial models are irrelevant with < 3 strata
  bbs_dat <- prepare_spatial(s,
                  strata_map = load_map(strat)) %>%
  prepare_model(.,
                model = "first_diff",
                model_variant = "spatial")

   }else{
     bbs_dat <- prepare_model(s,
                     model = "first_diff",
                     model_variant = "hier")
   }

   ## fit and save model output
fit <- run_model(model_data = bbs_dat,
                 refresh = 500,
                 #iter_warmup = 300,
                 #iter_sampling = 100,
                 output_dir = "output",
                 output_basename = paste0("fit_",aou))

}# end if file exists
  }

stopCluster(cluster)


