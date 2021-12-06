library(data.table)
library(rapport)
library(dplyr)

data_folder <- "F:/EDY DATA/raw/"

paths <- list.files(path = data_folder, full.names = T)
names <- list.files(path = data_folder, full.names = F)
temp_dir <- "D:/temp2"
patt <- "biomet.data"
nn <- nchar(patt)

s_names <- substr(x = names, start = 1, stop = nchar(names)-15)
times <- gsub(x = s_names, pattern = "T", replacement = " ")
posix <- as.POSIXct(times, format = "%Y-%m-%d %H%M")

dir.create(path = temp_dir)

dta_list <- list()
#for (i in 1:length(paths)){
for (i in 5010:length(paths)){
  
  unzip(zipfile = paths[i], exdir = temp_dir)
  path <- list.files(temp_dir, pattern = patt, full.names = T)
  if (length(path) != 0){
    
    dta <- read.table(file = path, header = T, sep = "\t", skip = 5)
    file.remove(list.files(path = temp_dir, full.names = T))
    dta_list[[i]] <- dta
    print(i)
    
  } else {
    
    print(i)
  }
  
  
}

dta <- rbindlist(dta_list)

dta <- dta %>% select(DATE, TIME, P_RAIN_1_1_1.mm., TA_1_1_1.C.)
dta[, TIME2:= substr(x = as.character(TIME), start = 1, stop = 5)]
dta[ ,DTM:= as.POSIXct(paste0(DATE, " ", TIME2), format = "%Y-%m-%d %H:%M")]

dta <- dta %>% select(DTM, P = P_RAIN_1_1_1.mm., T = TA_1_1_1.C.)

dta1 <- dta
