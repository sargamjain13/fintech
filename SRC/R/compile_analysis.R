## The following Rscript is the parent file for entire analysis of
## the project.

rm(list = ls())
options(scipen = 99)

########################
## Libraries included ##
########################
library(stringr)
library(data.table)
library(gmapsdistance)
library(ggmap)
library(dplyr)
geocodeQueryCheck(userType = "free")

#####################
## Input variables ##
#####################
regexp <- "[[:digit:]]+"
data_path <- "../DATA/"
graph_path <- "../../DOC/GRAPHS/"
ifsc_file_path <- "../DATA/IFSC_CSV/"
                                        # Extreme lat and lon for India
                                        # Setting limits
ext_min_lat_india <- 4
ext_max_lat_india <- 40
ext_min_lon_india <- 65
ext_max_lon_india <- 100

###################
## Files sourced ##
###################

## The function in read_currency_chest_data.R will generate warnings()
## due to blank spaces converted to NA.
source("read_currency_chest_data.R")
source("read_pincodes_data.R")
## The functions in following Rscripts take a lot of time in processing
## and have constraints w.r.t Google API. Output for the functions in
## these scripts is already saved in the DATA/ repository. The
## functions will be executed only if there are no files in DATA.
source("extract_IFSC_code.R")
source("compute_lat_lons.R")
source("plot_india_map.R")

##################################################
## Create final data set for RBI currency chest ##
##################################################

if(file.exists(paste0(data_path, "complete_data_cc.rda"))){
    load(paste0(data_path, "complete_data_cc.rda"), verbose = TRUE)
} else {
    currency_chest_data <- currency_chest_data[ , c("cc_branch",
                                                    "district",
                                                    "state",
                                                    "pin_code")]
    currency_chest_data$lat <- NA
    currency_chest_data$lon <- NA
    cc_with_latlon <- lapply(1:nrow(cc_latlon), function(x){
      cat(x, "\n")
      pin <- cc_latlon$pincode[x]
      lat <- cc_latlon$lat[x]
      lon <- cc_latlon$lon[x]
      pdata <- currency_chest_data[
          currency_chest_data$pin_code %in% pin, ]
      pdata$lat <- lat
      pdata$lon <- lon
      return(pdata)
    })
    cc_with_latlon <- rbindlist(cc_with_latlon)
    save(cc_with_latlon, file = paste0(data_path,
                                       "complete_data_cc.rda"))
}

#################################################
## Create final data set for pincode directory ##
#################################################

if(file.exists(paste0(data_path, "complete_data_pin_dir.rda"))){
    load(paste0(data_path, "complete_data_pin_dir.rda"),
         verbose = TRUE)
} else {
    pincodes_data <- pincodes_data[ , c("pincode", "talukname",
                                        "districtname", "statename")]
    pincodes_data$lat <- NA
    pincodes_data$lon <- NA
    pin_dir_with_latlon <- lapply(1:nrow(pin_latlon), function(x){
        cat(x, "\n")
        pin <- as.integer(pin_latlon$pincode[x])
        lat <- pin_latlon$lat[x]
        lon <- pin_latlon$lon[x]
        pdata <- pincodes_data[pincodes_data$pincode %in% pin, ]
        pdata$lat <- lat
        pdata$lon <- lon
        return(pdata)
    })
    pin_dir_with_latlon <- rbindlist(pin_dir_with_latlon)
    save(pin_dir_with_latlon,
         file = paste0(data_path, "complete_data_pin_dir.rda"))
}
#################################################################
## Plot India map for currency chests and entire PIN directory ##
#################################################################

plot_india_map_cc()

plot_india_map_pin_dir()

