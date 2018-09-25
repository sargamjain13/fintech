## The following Rscript extracts all pincodes in India.
## Indian administrative hierachy is as follows:
## Office name --> Taluk --> District --> Division --> Region --> State

###############
## Read data ##
###############
cat("\nReading the complete pin codes directory of India.\n\n")
pincodes_data <- read.csv(file = paste0(data_path,
                              "allindia_POlist.csv"),
                          header = TRUE)
pincodes_data <- pincodes_data[ , c("officename", "pincode",
                                    "divisionname", "regionname",
                                    "circlename", "Taluk",
                                    "Districtname", "statename")]
colnames(pincodes_data) <- c("officename", "pincode", "divisionname",
                             "regionname", "circlename", "talukname",
                             "districtname", "statename")
