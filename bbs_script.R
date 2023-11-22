## testing bbsBayes2 parallel in HRE env

shhh <- suppressPackageStartupMessages # so I don't get a bunch of start up messages in the output file, a tip I encountered while searching through StackOverflow...
shhh(library(bbsBayes2))
shhh(library(tidyverse))

setwd("C:/github/BBS_ebird_compare")


sp_list <- readRDS("species_list.rds") %>%
  filter(model == TRUE)

species <- c("Eastern Bluebird",
             "Cooper's Hawk",
             "Mourning Dove",
             "Carolina Wren")

# build cluster -----------------------------------------------------------

   #for(i in 1:4){

for(sp in species[c(4,3)]){#
    #sp <- "Cattle Egret"
    
i = which(sp_list[,"english"] == sp)
aou <- as.integer(sp_list[i,"aou"])


# identifying first years for selected species ----------------------------
    fy <- 2012
    # if(aou %in% c(4661,4660)){ #Alder and Willow Flycatcher
    #   fy <- 1978 #5 years after the split
    # }
    # if(aou %in% c(10,11,22860)){ # Clark's and Western Grebe and EUCD
    #   fy <- 1990 #5 years after the split and first year EUCD observed on > 3 BBS routes
    # }
    # if(aou == 6121){ # CAve Swallow
    #   fy = 1985
    # }



   strat <- "latlong"

   s <- stratify(by = strat,
              species = sp,
              quiet = TRUE) %>%
  prepare_data(min_n_routes = 1,
               min_max_route_years = 2,
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

fit <- run_model(model_data = bbs_dat,
                 refresh = 200,
                 #iter_warmup = 300,
                 #iter_sampling = 100,
                 output_dir = "output",
                 output_basename = paste0("fit_",aou))

}
