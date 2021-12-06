library(readxl)
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)

setwd("C:/Users/vmoravec/Downloads/Cleverfarm/")

a <- list.files(path = "./", full.names = T, pattern = "txt")
b <- list.files(path = "./", full.names = F, pattern = "txt")

b_n <- substr(x = b, start = 1, stop = nchar(b)-4)

list_dta <- list()
for (i in 1:length(a)){
  
  n <- read.table(a[i], header = T, sep = "\t", colClasses = c("character", 
                                                               "character", "character", "numeric", "numeric", 
                                                               "numeric", "numeric", "character", "numeric", 
                                                               "numeric"))
  n$povodi <- b_n[i]
  list_dta[[i]] <- n
  
}

dta <- rbindlist(list_dta)  

dta[,DTM:= as.POSIXct(paste0(Date, " ", Time), format = "%d-%m-%Y %H:%M") ]

dta$Date <- dta$Time <- dta$Note <- NULL

melt_dta_wd <- melt(dta %>% select(Wind.direction, povodi, DTM), id.vars = c("DTM","povodi"))
melt_dta_nwd <- melt(dta %>% select(-Wind.direction, povodi, DTM), id.vars = c("DTM","povodi"))

melt_dta_nwd[variable == "Temperature" & value < -20, value:= NA]

month_dta <- melt_dta_nwd[, {MEAN = mean(value, na.rm = T); SUM = sum(value, na.rm = T); list(MEAN = MEAN, SUM = SUM) }, by = .(month(DTM), year(DTM), povodi, variable)]

month_dta[, agg:= ifelse(test = variable == "Rainfall", yes = SUM, no = MEAN)]

ggplot(melt_dta_nwd)+
  geom_line(aes(x = DTM, y = value, colour = povodi))+
  facet_grid(povodi~variable, scales = "free_y")

ggplot(melt_dta_nwd)+
  geom_bar(aes())