## The following Rscript picks the nearest currency chest for every
## bank branch listed in the IFSC database.

########################################
## Read shape file for 11000 pincodes ##
########################################

cat("Reading shape file for a subset of 11000 pincodes. \n\n")
shapefile_pincodes <- read.table(paste0(data_path,
                                        "pincodes-shape.txt"),
                                 header = TRUE,
                                 sep = ",")
shapefile_pincodes$key <- as.character(shapefile_pincodes$key)
shapefile_pincodes$PINCODE <- as.numeric(do.call(
    rbind, lapply(shapefile_pincodes$key, function(x){
        strsplit(x, "/")[[1]][2]
        })))
shapefile_pincodes <- shapefile_pincodes[ , c("PINCODE", "place_name",
                                              "latitude", "longitude")]
colnames(shapefile_pincodes) <- c("PINCODE", "PLACE_NAME",
                                  "LAT", "LON")
shapefile_pincodes <- shapefile_pincodes[!(
    is.na(shapefile_pincodes$LAT)) , ]

#######################################################################
## Function to generate latitudes and longitudes for currency chests ##
#######################################################################

generate_latlons_cc <- function(cc_data){
                                        # Taking unique currency chest
                                        # pin codes
    unique_cc_pins <- data.frame(
        "cc_pin" = paste0(unique(cc_data$pin_code), ", India"),
        stringsAsFactors = FALSE)
                                        # Generating latitudes and
                                        # longitudes
    cc_latlon <- geocode(sapply(unique_pin_codes$pin,
                                URLencode,
                                reserved = TRUE))
    cc_latlon$pincode  <- as.numeric(do.call(rbind,
                      lapply(cc_latlon$cc_pin,
                             function(x){
                                 strsplit(x, ",")[[1]][1]
                             })))
    cc_latlon_junk <- cc_latlon[cc_latlon$lon < 0, ]
    cc_latlon <- cc_latlon[!(cc_latlon$pincode %in%
                             cc_latlon_junk$pincode), ]
                                        # Picking possible lat and lon
                                        # for junk values from shape
                                        # files
    junk_in_shape <- cc_latlon_junk[(cc_latlon_junk$pincode %in%
                                     shapefile_pincodes$PINCODE), ]
    cc_latlon_junk <- cc_latlon_junk[!(cc_latlon_junk$pincode %in%
                                       junk_in_shape$pincode), ]
    cc_latlon_junk$lat <- NA
    cc_latlon_junk$lon <- NA
                                        # Hard coded the junk values
    cc_latlon_junk$lat <- c(25.777949, 25.786156, 25.530165, 25.471203,
                            29.596265, 30.403656, 28.922155, 29.526823,
                            28.630749, 31.322499, 34.434980, 25.651217,
                            25.585324, 29.259014, 25.559340, 24.102712,
                            26.793611, 28.626258, 27.774491, 27.229570,
                            25.351274, 24.280949, 27.231877, 22.742493,
                            22.769733, 22.220075, 22.450439, 23.966026,
                            22.695371, 22.738661, 21.274369, 21.394048,
                            23.131995, 23.621693, 26.501493, 23.332035,
                            25.455197, 24.117143, 18.854788, 17.675033,
                            19.299061, 21.855156, 21.763854, 15.178941,
                            16.305160, 15.819581, 18.573447, 17.918253,
                            18.866680, 17.367221, 17.996436, 15.143945,
                            12.880717, 17.568054, 13.339936, 12.513530,
                            9.699678, 10.284776, 11.116070, 11.237030,
                            21.694524, 21.541518, 21.626947, 22.599048,
                            22.637247)
    cc_latlon_junk$lon <- c(84.736949, 84.720290, 81.375676, 81.660109,
                            79.655113, 79.318763, 79.702118, 76.611685,
                            75.941923, 75.578832, 74.791065, 72.422132,
                            75.501268, 74.400901, 76.440618, 74.442844,
                            82.740055, 78.798296, 80.730190, 80.832598,
                            82.976612, 87.243624, 88.494836, 88.476329,
                            88.342174, 88.189776, 86.996629, 88.038667,
                            88.455917, 88.373067, 74.749239, 79.326167,
                            81.700960, 77.433029, 78.002960, 77.782197,
                            78.133418, 82.662621, 73.882847, 75.900594,
                            84.792432, 83.919816, 85.972835, 77.361453,
                            80.439842, 78.042476, 83.354893, 83.191336,
                            83.550996, 78.565793, 79.557452, 76.923475,
                            74.849713, 76.568809, 74.736681, 79.886779,
                            78.451648, 79.198846, 77.370220, 78.864290,
                            71.516569, 71.578170, 69.982465, 71.802296,
                            71.458817)
    junk_in_shape$lat <- NA
    junk_in_shape$lon <- NA
    for(i in 1:nrow(junk_in_shape)){
        lat <- shapefile_pincodes$LAT[(
            shapefile_pincodes$PINCODE %in% junk_in_shape$pincode[i])]
        lon <- shapefile_pincodes$LON[(
            shapefile_pincodes$PINCODE %in% junk_in_shape$pincode[i])]
        junk_in_shape$lat[i] <- lat
        junk_in_shape$lon[i] <- lon
    }
    cc_latlon <- rbind(cc_latlon, cc_latlon_junk, junk_in_shape)
    save(cc_latlon,
         file = paste0(data_path, "latlon_currency_chests.rda"))
    return(NULL)
}
    

######################################################################
## Functions to generate latitudes and longitudes for bank branches ##
######################################################################
generate_latlons_pin_directory <- function(pin_directory){
                                        # Pick possible lat and lons
                                        # for from shape files
    unique_pin_codes <- data.frame(
        "pin" = unique(pin_directory$pincode),
        stringsAsFactors = FALSE)
    unique_pin_codes$pin <- as.character(unique_pin_codes$pin)
    pin_latlon <- geocode(sapply(unique_pin_codes$pin,
                                 URLencode,
                                 reserved = TRUE))
    save(pin_latlon,
         file = paste0(data_path, "latlon_pin_directory.rda"))
    return(NULL)
}

clean_latlons_pin <- function(){
    pin_latlon <- rbind(pin_latlon_1_2500,
                        pin_latlon_2501_5000,
                        pin_latlon_5001_7500,
                        pin_latlon_7500_10000,
                        pin_latlon_10001_12500,
                        pin_latlon_12501_15000,
                        pin_latlon_15001_17500,
                        pin_latlon_17501_19100)
    colnames(pin_latlon) <- c("pincode", "lon", "lat")
    ## Subsetting for lats and lons with NA
    pin_latlon_NA <- pin_latlon[is.na(pin_latlon$lon), ]
    na_in_shape <- pin_latlon_NA[pin_latlon_NA$pincode %in%
                                 shapefile_pincodes$PINCODE, ]
    for(i in 1:nrow(na_in_shape)){
        lat <- shapefile_pincodes$LAT[(
            shapefile_pincodes$PINCODE %in% na_in_shape$pincode[i])]
        lon <- shapefile_pincodes$LON[(
            shapefile_pincodes$PINCODE %in% na_in_shape$pincode[i])]
        na_in_shape$lat[i] <- lat
        na_in_shape$lon[i] <- lon
    }
    remaining_NA <- pin_latlon_NA[!(pin_latlon_NA$pincode %in%
                                    na_in_shape$pincode), ]
    ## Subsetting for lats and lons with junk values
    pin_latlon_junk <- subset(pin_latlon,
                              pin_latlon$lat < ext_min_lat_india |
                              pin_latlon$lon < ext_min_lon_india |
                              pin_latlon$lat > ext_max_lat_india |
                              pin_latlon$lon > ext_max_lon_india)
                                        # Junk values found in Shape
                                        # files
    junk_in_shape <- pin_latlon_junk[pin_latlon_junk$pincode %in%
                                     shapefile_pincodes$PINCODE, ]
    junk_in_shape$lat <- NA
    junk_in_shape$lon <- NA
    for(i in 1:nrow(junk_in_shape)){
        lat <- shapefile_pincodes$LAT[(
            shapefile_pincodes$PINCODE %in% junk_in_shape$pincode[i])]
        lon <- shapefile_pincodes$LON[(
            shapefile_pincodes$PINCODE %in% junk_in_shape$pincode[i])]
        junk_in_shape$lat[i] <- lat
        junk_in_shape$lon[i] <- lon
    }
    remaining_junk <- pin_latlon_junk[!(pin_latlon_junk$pincode %in%
                                       junk_in_shape$pincode), ]
                                        # Clean set
    pin_latlon <- pin_latlon[!(pin_latlon$pincode %in%
                               pin_latlon_NA$pincode), ]
    pin_latlon <- pin_latlon[!(pin_latlon$pincode %in%
                               pin_latlon_junk$pincode), ]
    pin_latlon <- rbind(pin_latlon, junk_in_shape, na_in_shape)
    return(pin_latlon)
}
    
####################
## Function calls ##
####################

                                        # CURRENCY CHESTS
if(file.exists(paste0(data_path, "latlon_currency_chests.rda"))){
    cat("Reading the file with latitudes and longitudes of all currency chests.\n")
    load(paste0(data_path, "latlon_currency_chests.rda"),
         verbose = T)
} else {
    cat("Running the function to generate latitudes and longitudes for currency chests. \n\n")
    generate_latlons_cc(cc_data = currency_chest_data)
}

                                        # ENTIRE PINCODE DIRECTORY
if(file.exists(paste0(data_path, "latlon_pin_directory.rda"))){
    cat("\nReading the file with latitudes and longitudes of entire pin codes directory.\n")
    load(paste0(data_path, "latlon_pin_directory.rda"),
         verbose = T)
    pin_latlon <- clean_latlons_pin()
} else {
    cat("Running the function to generate latitudes and longitudes for entire pin codes directory. \n\n")
    generate_latlons_pin_directory(pin_directory = pincodes_data)
    load(paste0(data_path, "latlon_pin_directory.rda"),
         verbose = T)
    pin_latlon <- clean_latlons_pin()
}
