## The following Rscript reads data for currency chests and extracts
## cleaned pin codes for all currecy chests under jurisdiction of 18
## regional offices.

###################
## Files sourced ##
###################
source("clean_address_pincode.R")

###############
## Read data ##
###############
base_data <- read.csv(file = paste0(data_path,
                          "../DATA/currency_chest_rbi.csv"),
                      header = TRUE)
base_data <- base_data[ , c("S.No", "Name.of.the.State",
                            "Name.of.the.District", "Bank.Name",
                            "Name.of.the.CC.Branch",
                            "Complete.Address.of.Currency.Chest",
                            "Pin.code", "RBI.Office")]
colnames(base_data) <- c("sno", "state", "district", "bank_name",
                         "cc_branch", "complete_address", "pin_code",
                         "reg_office")

################
## Clean data ##
################

## Function call to pick PIN CODES for each currency chest
                                        # The function will generate
                                        # warnings() due to blank spaces
                                        # converted to NA.
currency_chest_data <- pick_pincode(mdata = base_data)











