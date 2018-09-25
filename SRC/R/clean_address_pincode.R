## The following Rscript consists of function to extract
## cleaned-up pincodes for each regional office.

########################################################
## Function to pick pincodes for all regional offices ##
########################################################

pick_pincode <- function(mdata){
    mdata$pin_code <- as.numeric(as.character(mdata$pin_code))
    ## Reg office: New Delhi
    cat("1. RBI currency chests under New Delhi office. \n")
    tmp_DL <- mdata[mdata$reg_office == "New Delhi", ]
    tmp_DL$pin_code <- do.call(rbind, lapply(tmp_DL$complete_address,
                                             function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    cat("No hard coding required for New Delhi office. \n\n")
    
    ## Reg office: Dehradun/Kanpur
    cat("2. RBI currency chests under Kanpur office. \n")
    tmp_DN_KNP <- mdata[mdata$reg_office == "Dehradun/Kanpur", ]
    tmp_DN_KNP$pin_code <- do.call(rbind,
                                   lapply(tmp_DN_KNP$complete_address,
                                          function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    cat("No hard coding required for Kanpur office. \n\n")
    
    ## Reg office: Shimla/Chandigarh
    cat("3. RBI currency chests under Chandigarh office. \n")
    tmp_SH_CH <- mdata[mdata$reg_office == "Shimla/Chandigarh", ]
    tmp_SH_CH$pin_code <- as.numeric(gsub(" ", "", tmp_SH_CH$pin_code))
    cat("No hard coding required for Chandigarh office. \n\n")

    ## Reg office: Srinagar/Jammu
    cat("4. RBI currency chests under Jammu office. \n")
    tmp_SN_JM <- mdata[mdata$reg_office == "Sri Nagar/Jammu", ]
    tmp_SN_JM$pin_code <- as.numeric(gsub(" ", "", tmp_SN_JM$pin_code))
    if(any(is.na(tmp_SN_JM$pin_code)) == "TRUE"){
        ## Fill in the NA value (hard coding)
        tmp_SN_JM$pin_code[is.na(tmp_SN_JM$pin_code)] <- 180001
        cat("1 pin code value for Sri Nagar/Jammu was NA. ")
        cat("Hard coded. \n\n")
    }
    
    ## Reg office: Jaipur
    cat("5. RBI currency chests under Jaipur office. \n")
    tmp_JP <- mdata[mdata$reg_office == "Jaipur", ]
    tmp_JP$pin_code <- do.call(rbind, lapply(tmp_JP$complete_address,
                                             function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    if(any(is.na(tmp_JP$pin_code)) == "TRUE"){
        ## Fill in the NA value (hard coding)
        tmp_JP$pin_code[is.na(tmp_JP$pin_code)] <- c(305001, 301001,
                                                     301001, 321203,
                                                     334001, 328001,
                                                     302015, 302016,
                                                     326023, 324001)
        cat("10 pin code values for Jaipur are NA. ")
        cat("Hard coded. \n\n")
    }
    
    ## Reg office: Lucknow
    cat("6. RBI currency chests under Lucknow office. \n")
    tmp_LN <- mdata[mdata$reg_office == "Lucknow", ]
    tmp_LN$pin_code <- do.call(rbind,
                               lapply(tmp_LN$complete_address,
                                      function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    cat("No hard coding required for Lucknow office. \n\n")
    
    ## Reg office: Guwahati/Agartala, Aizawl/Gangtok/Imphal/Shillong
    cat("7. RBI currency chests under Guwahati office. \n")
    tmp_NE <- mdata[mdata$reg_office == "Guwahati/Agartala/Aizawl/Gangtok/Imphal/Shillong", ]
    tmp_NE$pin_code <- do.call(rbind,
                           lapply(tmp_NE$complete_address,
                                  function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    cat("No hard coding required for Gangtok office. \n\n")
    
    ## Reg office: Patna/Ranchi
    cat("8. RBI currency chests under Ranchi office. \n")
    tmp_PT_RA <- mdata[mdata$reg_office == "Patna/Ranchi", ]
    tmp_PT_RA$pin_code <- do.call(rbind,
                                  lapply(tmp_PT_RA$complete_address,
                                         function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    if(any(is.na(tmp_PT_RA$pin_code)) == "TRUE"){
        ## Fill in the NA value (hard coding)
        tmp_PT_RA$pin_code[is.na(tmp_PT_RA$pin_code)] <- 823001
        cat("1 pin code value for Ranchi is NA. ")
        cat("Hard coded. \n\n")
    }

    ## Reg office: Kolkata
    cat("9. RBI currency chests under Kolkata office. \n")
    tmp_CAL <- mdata[mdata$reg_office == "Kolkata", ]
    tmp_CAL$pin_code <- do.call(rbind,
                                  lapply(tmp_CAL$complete_address,
                                         function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    if(any(is.na(tmp_CAL$pin_code)) == "TRUE"){
        ## Fill in the NA value (hard coding)
        tmp_CAL$pin_code[is.na(tmp_CAL$pin_code)] <- c(700016, 700017,
                                                       700062, 700072,
                                                       700091)
        cat("5 pin code values for Kolkata are NA. ")
        cat("Hard coded. \n\n")
    }
    
    ## Reg office: Raipur/Nagpur
    cat("10. RBI currency chests under Nagpur office. \n")
    tmp_RP_NP <- mdata[mdata$reg_office == "Raipur/Nagpur", ]
    tmp_RP_NP$pin_code <- do.call(rbind,
                                  lapply(tmp_RP_NP$complete_address,
                                         function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    if(any(is.na(tmp_RP_NP$pin_code)) == "TRUE"){
        ## Fill in the NA value (hard coding)
        tmp_RP_NP$pin_code[is.na(tmp_RP_NP$pin_code)] <- c(445206,
                                                           495006,
                                                           491335)
        cat("3 pin code values for Nagpur are NA. ")
        cat("Hard coded. \n\n")
    }

    ## Reg office: Bhopal
    cat("11. RBI currency chests under Bhopal office. \n")
    tmp_BHP <- mdata[mdata$reg_office == "Bhopal", ]
    tmp_BHP$pin_code <- do.call(rbind,
                                lapply(tmp_BHP$complete_address,
                                       function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    if(any(is.na(tmp_BHP$pin_code)) == "TRUE"){
        ## Fill in the NA value (hard coding)
        tmp_BHP$pin_code[is.na(tmp_BHP$pin_code)] <- c(452016,
                                                       452016,
                                                       470004)
        cat("3 pin code values for Bhopal are NA. ")
        cat("Hard coded. \n\n")
    }
    
    ## Reg office: Mumbai
    cat("12. RBI currency chests under Mumbai office. \n")
    tmp_MUM <- mdata[mdata$reg_office == "Mumbai", ]
    tmp_MUM$pin_code <- do.call(rbind,
                                lapply(tmp_MUM$complete_address,
                                       function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
   }))
    cat("No hard coding required for Mumbai office. \n\n")

    ## Reg office: Belapur
    cat("13. RBI currency chests under Belapur office. \n")
    tmp_BEL <- mdata[mdata$reg_office == "Belapur", ]
    tmp_BEL$pin_code <- do.call(rbind,
                                lapply(tmp_BEL$complete_address,
                                       function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    
    if(any(is.na(tmp_BEL$pin_code)) == "TRUE"){
        ## Fill in the NA value (hard coding)
        tmp_BEL$pin_code[is.na(tmp_BEL$pin_code)|nchar(tmp_BEL$pin_code) != 6] <- c(411001, 422306, 413304, 411034)
        cat("4 pin code value for Belapur are NAs. ")
        cat("Hard coded. \n\n")
    }

    ## Reg office: Bhubaneswar
    cat("14. RBI currency chests under Bhubaneswar office. \n")
    tmp_BHB <- mdata[mdata$reg_office == "Bhubaneswar", ]
    tmp_BHB$pin_code <- do.call(rbind,
                                lapply(tmp_BHB$complete_address,
                                       function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    cat("No hard coding required for Bhubaneswar office. \n\n")
    
    ## Reg office: Hyderabad
    cat("15. RBI currency chests under Hyderabad office. \n")
    tmp_HYD <- mdata[mdata$reg_office == "Hyderabad", ]
    tmp_HYD$pin_code <- do.call(rbind,
                                lapply(tmp_HYD$complete_address,
                                       function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    cat("No hard coding required for Hyderabad office. \n\n")
 
    ## Reg office: Bangalore
    cat("16. RBI currency chests under Bangalore office. \n")
    tmp_BLR <- mdata[mdata$reg_office == "Bangalore", ]
    tmp_BLR$pin_code <- do.call(rbind,
                                lapply(tmp_BLR$complete_address,
                                       function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    if(any(is.na(tmp_BLR$pin_code))){
        ## Fill in the NA value (hard coding)
        tmp_BLR$pin_code[is.na(tmp_BLR$pin_code)] <- 575008
        cat("1 pin code value for Bangalore office is NA. ")
        cat("Hard coded. \n\n")
    }
    cat("No hard coding required for Bangalore office. \n\n")
    
    ## Reg office: Chennai
    cat("17. RBI currency chests under Chennai office. \n")
    tmp_CHN <- mdata[mdata$reg_office == "Chennai", ]
    tmp_CHN$pin_code <- do.call(rbind,
                                lapply(tmp_CHN$complete_address,
                                       function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    cat("No hard coding required for Chennai office. \n\n")
  
    ## Reg office: Ahmedabad
    cat("18. RBI currency chests under Ahmedabad office. \n")
    tmp_AHM <- mdata[mdata$reg_office == "Ahmedabad", ]
    tmp_AHM$pin_code <- do.call(rbind,
                                lapply(tmp_AHM$complete_address,
                                       function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    cat("No hard coding required for Ahmedabad office. \n\n")
       
    ## Reg office: Thiruvananthapuram
    cat("19. RBI currency chests under Thiruvananthapuram office. \n")
    tmp_THM <- mdata[mdata$reg_office == "Thiruvananthapuram", ]
    tmp_THM$pin_code <- do.call(rbind,
                                lapply(tmp_THM$complete_address,
                                       function(x){
       ans <- gsub(" ", "", x)
       ans <- ans %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
       ans <- ans[nchar(as.character(ans)) == 6][1]
       ans <- as.numeric(ans)
       return(ans)
   }))
    if(any(is.na(tmp_THM$pin_code)) == "TRUE"){
        ## Fill in the NA value (hard coding)
        tmp_THM$pin_code[is.na(tmp_THM$pin_code)] <- c(682304,
                                                       rep(682555,
                                                           9))
        cat("10 pin code values for Thiruvanathapuram are NA. ")
        cat("Hard coded. \n\n")
    }
    
    tmp <- rbind(tmp_DL, tmp_DN_KNP, tmp_SH_CH, tmp_SN_JM,
                 tmp_JP, tmp_LN, tmp_NE, tmp_PT_RA, tmp_CAL,
                 tmp_RP_NP, tmp_BHP, tmp_MUM, tmp_BEL, tmp_BHB,
                 tmp_HYD, tmp_BLR, tmp_CHN, tmp_AHM, tmp_THM)
    return(tmp)
}
