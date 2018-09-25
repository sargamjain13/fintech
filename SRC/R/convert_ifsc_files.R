## The following Rscript converts IFSC files in data repository from
## xls format to csv format.
rm(list = ls())
options(scipen = 999)

ifsc_files <- list.files("IFSC/")

tmp <- lapply(ifsc_files, function(x){
          cat(x, ".", "\n")        
          csvfile <- paste0("ssconvert",
                            paste0(data_path, "IFSC/"),
                            x,
                            paste0(data_path, "IFSC_CSV/"),
                            strsplit(x, ".", fixed = TRUE)[[1]][1],
                            ".csv")
          system(csvfile)
      })

csvfile <- list.files(paste0(data_path, "IFSC_CSV/"))
length(csvfile)

