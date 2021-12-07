library(data.table)
library(foreign)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)

setwd("C:/Users/Moravec/Downloads/Vojta_a_Martin/")

hru_KL <- readRDS("./dHRUMInputs_KL_Drainage_FG.rds")
hru_BP <- readRDS("./dHRUMInputs_BP_Drainage_FG.rds")

colnames(hru_KL)[2] <- "OBJECTID"
colnames(hru_BP)[2] <- "OBJECTID"

hru_KL$POV <- "Karluv luh" 
hru_BP$POV <- "Brejlsky potok" 

kl_dbf <- read.dbf("./shp/KL_Drainage_FG/KL_Drainage_FG.dbf")
bp_dbf <- read.dbf("./shp/BP_Drainage_FG/BP_D_FG.dbf")


KL <- merge(x = hru_KL, y = kl_dbf %>% select(OBJECTID,Land_Use), by = "OBJECTID")
BP <- merge(x = hru_BP, y = bp_dbf %>% select(OBJECTID,Land_Use), by = "OBJECTID")

dta_all <- rbind(KL, BP)


unique(dta_all$Land_Use)

dta_all <- dta_all[!Land_Use %in% c("Building", "Channel")]

dta_all[Land_Use == "Forest" , CAST:= "Lesni cast"]
dta_all[Land_Use != "Forest" , CAST:= "Zemedelska cast"]

dta_all[month(DTM) %in% c(12,1,2), SEZONA:= "zimni"]
dta_all[month(DTM) %in% c(3,4,5), SEZONA:= "jarni"]
dta_all[month(DTM) %in% c(6,7,8), SEZONA:= "letni"]
dta_all[month(DTM) %in% c(9,10,11), SEZONA:= "podzimni"]

#################################### Mesicni #################################

dta_pov_def <- dta_all[,{P = mean(P); T = mean(T); list(P=P, T=T)}, by = .(year(DTM), month(DTM), day(DTM), POV)]
dta_cast_def <- dta_all[,{P = mean(P); T = mean(T); list(P=P, T=T)}, by = .(year(DTM), month(DTM), day(DTM), CAST)]
dta_cely_def <- dta_all[,{P = mean(P); T = mean(T); list(P=P, T=T)}, by = .(year(DTM), month(DTM), day(DTM))]

dta_pov_m <- dta_pov_def[,{Suma_P = sum(P); Prumer_T = mean(T); Min_T = min(T); Max_T = max(T); Max_P = max(P); list(Suma_P=Suma_P, Prumer_T=Prumer_T, Min_T=Min_T, Max_T=Max_T, Max_P=Max_P)}, by = .(year, month, POV)]
dta_cast_m <- dta_cast_def[,{Suma_P = sum(P); Prumer_T = mean(T); Min_T = min(T); Max_T = max(T); Max_P = max(P); list(Suma_P=Suma_P, Prumer_T=Prumer_T, Min_T=Min_T, Max_T=Max_T, Max_P=Max_P)}, by = .(year, month, CAST)]
dta_cely_m <- dta_cely_def[,{Suma_P = sum(P); Prumer_T = mean(T); Min_T = min(T); Max_T = max(T); Max_P = max(P); list(Suma_P=Suma_P, Prumer_T=Prumer_T, Min_T=Min_T, Max_T=Max_T, Max_P=Max_P)}, by = .(year, month)]

dta_pov_m <- melt(dta_pov_m, id.vars = c("year", "month", "POV"))
dta_cast_m <- melt(dta_cast_m, id.vars = c("year", "month", "CAST"))
dta_cely_m <- melt(dta_cely_m, id.vars = c("year", "month"))

dta_pov_m[, c('METHOD', 'VAR') := tstrsplit(variable, '_', keep = c(1,2))]
dta_cast_m[, c('METHOD', 'VAR') := tstrsplit(variable, '_', keep = c(1,2))]
dta_cely_m[, c('METHOD', 'VAR') := tstrsplit(variable, '_', keep = c(1,2))]

#################################### Sezonni #################################

dta_pov_def[month %in% c(12,1,2), SEZONA:= "zimni"]
dta_pov_def[month %in% c(3,4,5), SEZONA:= "jarni"]
dta_pov_def[month %in% c(6,7,8), SEZONA:= "letni"]
dta_pov_def[month %in% c(9,10,11), SEZONA:= "podzimni"]

dta_pov_s <- dta_pov_def[,{Suma_P = sum(P); Prumer_T = mean(T); Min_T = min(T); Max_T = max(T); Max_P = max(P); list(Suma_P=Suma_P, Prumer_T=Prumer_T, Min_T=Min_T, Max_T=Max_T, Max_P=Max_P)}, by = .(year, SEZONA, POV)]
dta_cast_s <- dta_pov_def[,{Suma_P = sum(P); Prumer_T = mean(T); Min_T = min(T); Max_T = max(T); Max_P = max(P); list(Suma_P=Suma_P, Prumer_T=Prumer_T, Min_T=Min_T, Max_T=Max_T, Max_P=Max_P)}, by = .(year, SEZONA, CAST)]
dta_cely_s <- dta_all[,{Suma_P = sum(P); Prumer_T = mean(T); Min_T = min(T); Max_T = max(T); Max_P = max(P); list(Suma_P=Suma_P, Prumer_T=Prumer_T, Min_T=Min_T, Max_T=Max_T, Max_P=Max_P)}, by = .(year, SEZONA)]

#################################### Rocni #################################

dta_pov_r <- dta_all[,{Suma_P = sum(P); Prumer_T = mean(T); Min_T = min(T); Max_T = max(T); Max_P = max(P); list(Suma_P=Suma_P, Prumer_T=Prumer_T, Min_T=Min_T, Max_T=Max_T, Max_P=Max_P)}, by = .(year, POV)]
dta_cast_r <- dta_all[,{Suma_P = sum(P); Prumer_T = mean(T); Min_T = min(T); Max_T = max(T); Max_P = max(P); list(Suma_P=Suma_P, Prumer_T=Prumer_T, Min_T=Min_T, Max_T=Max_T, Max_P=Max_P)}, by = .(year, CAST)]
dta_cely_r <- dta_all[,{Suma_P = sum(P); Prumer_T = mean(T); Min_T = min(T); Max_T = max(T); Max_P = max(P); list(Suma_P=Suma_P, Prumer_T=Prumer_T, Min_T=Min_T, Max_T=Max_T, Max_P=Max_P)}, by = .(year)]

################################## EDDY MESICNI #################################

eddy <- data.table(readRDS("C:/Users/Moravec/Desktop/eddy_data.rds"))

eddy <- eddy[!is.na(P) | !is.na(T)]

eddy_m <- eddy[,{Suma_P = sum(P); Prumer_T = mean(T); Min_T = min(T); Max_T = max(T); Max_P = max(P); list(Suma_P=Suma_P, Prumer_T=Prumer_T, Min_T=Min_T, Max_T=Max_T, Max_P=Max_P)}, by = .(year(DTM), month(DTM))]
eddy_m <- eddy_m[!is.na(year)]

eddy_m <- melt(eddy_m, id.vars = c("year", "month"))
eddy_m[, c('METHOD', 'VAR') := tstrsplit(variable, '_', keep = c(1,2))]

################################## EDDY MESICNI #################################


############ Mesicni plot #######################


ggplot()+
  geom_boxplot(data = dta_pov_m, aes(x=as.factor(month), y = value, group = interaction(month, POV), fill = POV),position=position_dodge(1))+
  geom_point(data = eddy_m, aes(as.factor(month), y = value, group = month), color = "#377eb8", shape = 4, size =3)+
  geom_point(data = eddy_m, aes(as.factor(month), y = value, group = month), color = "#377eb8", shape = 16)+
  scale_fill_manual(values = c("#e41a1c", "#4daf4a"))+
  #scale_x_discrete(breaks = c(1:12), labels = c(1:12))+
  facet_wrap(VAR~METHOD, scales = "free_y", ncol = 1, strip.position = "right")+
  theme_bw()+
  theme(axis.title.y = element_blank(), legend.position = "bottom")+
  labs(fill = "", x = "Mesic")

ggplot()+
  geom_boxplot(data = dta_cast_m, aes(x=as.factor(month), y = value, group = interaction(month, CAST), fill = CAST))+
  geom_point(data = eddy_m, aes(as.factor(month), y = value, group = month), color = "#377eb8", shape = 4, size =3)+
  geom_point(data = eddy_m, aes(as.factor(month), y = value, group = month), color = "#377eb8", shape = 16)+
  scale_fill_manual(values = c("#e41a1c", "#4daf4a"))+
  #scale_x_discrete(breaks = c(1:12), labels = c(1:12))+
  facet_wrap(VAR~METHOD, scales = "free_y", ncol = 1, strip.position = "right")+
  theme_bw()+
  theme(axis.title.y = element_blank(), legend.position = "bottom")+
  labs(fill = "", x = "Mesic")

ggplot()+
  geom_boxplot(data = dta_cely_m, aes(x=as.factor(month), y = value, group = month))+
  geom_point(data = eddy_m, aes(as.factor(month), y = value, group = month), color = "#377eb8", shape = 4, size =3)+
  geom_point(data = eddy_m, aes(as.factor(month), y = value, group = month), color = "#377eb8", shape = 16)+
  scale_fill_manual(values = c("#e41a1c", "#4daf4a"))+
  #scale_x_discrete(breaks = c(1:12), labels = c(1:12))+
  facet_wrap(VAR~METHOD, scales = "free_y", ncol = 1, strip.position = "right")+
  theme_bw()+
  theme(axis.title.y = element_blank(), legend.position = "bottom")+
  labs(fill = "", x = "Mesic")
