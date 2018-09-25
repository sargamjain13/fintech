## The following Rscript reads IFSC file for each bank and extracts
## pincodes for every bank branch address. 

#############################
## List IFSC files in DATA ##
#############################
ifsc_files <- list.files(ifsc_file_path)

cat("Running the function to read IFSC files.\n\n")
read_ifsc_files <- lapply(ifsc_files, function(x){
    tmp <- read.csv(paste0(ifsc_file_path, x), 
                    header = TRUE)
                                        # Hard coding to pick columns
                                        # with non-junk values
    tmp <- tmp[ , 1:9]
    colnames(tmp) <- c("BANK", "IFSC", "MICR", "BRANCH", "ADDRESS",
                       "CONTACT", "CITY", "DISTRICT", "STATE")
    tmp <- tmp[ , c("BANK", "IFSC", "BRANCH", "ADDRESS",
                    "CITY", "DISTRICT", "STATE")]
    tmp$BANK <- as.character(tmp$BANK)
    tmp$IFSC <- as.character(tmp$IFSC)
    tmp$BRANCH <- as.character(tmp$BRANCH)
    tmp$ADDRESS <- as.character(tmp$ADDRESS)
    tmp$CITY <- as.character(tmp$CITY)
    tmp$DISTRICT <- as.character(tmp$DISTRICT)
    tmp$STATE <- as.character(tmp$STATE)
    return(tmp)
})

ifsc_files <- do.call(rbind, read_ifsc_files)

###################################
## Function to generate pincodes ##
###################################

generate_pincodes <- function(mdata){
    address <- gsub(" ", "", mdata$ADDRESS)
    tmp <- lapply(1:length(address), function(x){
       print(x)
       ans <- address[x] %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
        if(any(nchar(as.character(ans)) %in% 6)){
            ans <- ans[nchar(as.character(ans)) == 6][1]
            ## cat(paste0("Pincode extracted.", ans, "\n"))
        } else {
            ## cat("PIN code not available. \n")
            ans <- NA
        }
       return(ans)
   })
    tmp <- do.call(c, (tmp))
    mdata$PINCODE <- as.character(tmp)
    return(mdata)
}

####################
## Function calls ##
####################

if(file.exists(paste0(data_path, "bankbranch_ifsc_pincodes.rda"))){

    cat("Reading the file with IFSC codes and pin codes for bank branches.\n")
    load(paste0(data_path, "bankbranch_ifsc_pincodes.rda"), verbose = T)

} else {

    cat("Running the function to extract PIN codes for bank branches.\n\n")
    pincodes_ifsc <- generate_pincodes(mdata = ifsc_data)
    save(pincodes_ifsc, file = paste0(
                            data_path,
                            "bankbranch_ifsc_pincodes.rda"))
}
