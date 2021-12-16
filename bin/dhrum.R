library(data.table)
library(foreign)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Moravec/Downloads/Vojta_a_Martin/")

bp_dbf <- data.table(read.dbf("./shp/BP_Drainage_FG/BP_D_FG.dbf"))
kl_dbf <- data.table(read.dbf("./shp/KL_Drainage_FG/KL_Drainage_FG.dbf"))

bp_dist <- readRDS("C:/Users/Moravec/Downloads/BP_dist.rds")
kl_dist <- readRDS("C:/Users/Moravec/Downloads/KL_dist.rds")

bp_area <- bp_dbf[Land_Use %in% c("Forest", "Arable land", "Meadow") , sum(Area)]
kl_area <- kl_dbf[Land_Use %in% c("Forest", "Arable land", "Meadow") , sum(Area)]

colnames(bp_dist)[24] <- "OBJECTID"
colnames(kl_dist)[24] <- "OBJECTID"
bp_dist$OBJECTID <- as.numeric(bp_dist$OBJECTID)
kl_dist$OBJECTID <- as.numeric(kl_dist$OBJECTID)

BP_DIST <- merge(x = bp_dist, y = bp_dbf %>% select(OBJECTID,Land_Use, Area), by = "OBJECTID")
KL_DIST <- merge(x = kl_dist, y = kl_dbf %>% select(OBJECTID,Land_Use, Area), by = "OBJECTID")

BP_DIST <- BP_DIST %>% select(DTM, LU = Land_Use, Area, PREC, TEMP, TOTR, BASF, SURS, SOIS, INTS, DIRR, PERC, GROS, AET, EVAC, EVAS, EVBS)
KL_DIST <- KL_DIST %>% select(DTM, LU = Land_Use, Area, PREC, TEMP, TOTR, BASF, SURS, SOIS, INTS, DIRR, PERC, GROS, AET, EVAC, EVAS, EVBS)

unique(BP_DIST$LU)
unique(KL_DIST$LU)

BP_DIST <- BP_DIST[LU %in% c("Forest", "Arable land", "Meadow")]
KL_DIST <- KL_DIST[LU %in% c("Forest", "Arable land", "Meadow")]

BP_DIST[LU == "Meadow", LU:= "Arable land"]
KL_DIST[LU == "Meadow", LU:= "Arable land"]

BP_DIST[, POV:= "Brejlsky potok"]
KL_DIST[, POV:= "Karluv luh"]

dta <- rbind(BP_DIST, KL_DIST)

dta_m <- melt(dta, id.vars = c("DTM", "LU", "POV", "Area"))

dta_m[ , value_m3:= ifelse(test = variable %in% c("TEMP"), yes = value, no = value / 1000 * Area)]

dta_m <- dta_m[, {mean=mean(value_m3); sum=sum(value_m3); list(mean=mean, sum=sum)}, by = .(DTM, LU, POV, variable)]

dta_m[, val:= ifelse(test = variable %in% c("TEMP"), yes = mean, no = sum)]

dta_m$mean <- dta_m$sum <- NULL

dta_m <- dta_m[,{mean=mean(val); sum=sum(val); list(mean=mean, sum=sum)}, by = .(year(DTM), month(DTM), LU, POV, variable)]

dta_m[, val:= ifelse(test = variable %in% c("SOIS", "GROS", "SURS", "TEMP"), yes = mean, no = sum)]

dta_m$mean <- dta_m$sum <- NULL

dta <- data.table(dcast(dta_m, formula = year+month+LU+POV~variable, value.var = "val"))

dta[,AETT:= AET+EVAC+EVAS+EVBS]

dta$AET <- dta$EVAC <- dta$EVAS <- dta$EVBS <- NULL

dta_m <- melt(dta, id.vars = c("year", "month", "LU", "POV"))

dta_m[month %in% c(12,1,2), SEZONA:= "zimni"]
dta_m[month %in% c(3,4,5), SEZONA:= "jarni"]
dta_m[month %in% c(6,7,8), SEZONA:= "letni"]
dta_m[month %in% c(9,10,11), SEZONA:= "podzimni"]

dta_m[year %in% c(1961:1980), PER:= "1961-1980"]
dta_m[year %in% c(1981:2000), PER:= "1981-2000"]
dta_m[year %in% c(2001:2020), PER:= "2001-2020"]

dta_m[variable == "PREC", variable:= "Srazky"]
dta_m[variable == "TEMP", variable:= "Teplota"]
dta_m[variable == "TOTR", variable:= "Celkovy odtok"]
dta_m[variable == "BASF", variable:= "Zakladni odtok"]
dta_m[variable == "SURS", variable:= "Povrchova retence"]
dta_m[variable == "SOIS", variable:= "Zasoba vody v pude"]
dta_m[variable == "INTS", variable:= "Intercepce"]
dta_m[variable == "DIRR", variable:= "Primy odtok"]
dta_m[variable == "PERC", variable:= "Perkolace"]
dta_m[variable == "GROS", variable:= "Zasoba podzemni vody"]
dta_m[variable == "AETT", variable:= "Celkova aktualni evapotranspirace"]

dta_d <- data.table(dcast(dta_m, formula = year+month+variable+SEZONA+PER+POV~LU, value.var = "value"))

dta_d[POV == "Brejlsky potok", `Arable land [mm]`:= `Arable land` / bp_area * 1000]
dta_d[POV == "Karluv luh", `Arable land [mm]`:= `Arable land` / kl_area * 1000]

dta_d[POV == "Brejlsky potok", `Forest [mm]`:= `Forest` / bp_area * 1000]
dta_d[POV == "Karluv luh", `Forest [mm]`:= `Forest` / kl_area * 1000]

dta_d[, DIFF:= (Forest / `Arable land` - 1) * 100 ]
dta_d[DIFF == Inf, DIFF:= NA ]
dta_d[DIFF <= 0 ,fill:= "red"]
dta_d[DIFF > 0 ,fill:= "blue"]
dta_d2 <- dta_d[, mean(DIFF, na.rm = T), by = .(variable, SEZONA, PER, POV)]
dta_d2[V1 <= 0 ,fill:= "red"]
dta_d2[V1 > 0 ,fill:= "blue"]

dta_d[, DIFF:= (`Forest [mm]` - `Arable land [mm]`) ]
dta_d[, DTM:= as.Date(paste0(year, "-", month, "-01"), format = "%Y-%m-%d")]


dta_d_cum <- dta_d[, cumsum(DIFF), by = .(DTM, variable, POV)]

dta_d_m <- melt(dta_d, id.vars = c("DTM","year", "month", "variable", "SEZONA", "PER", "POV"))

############## SEZONNI ############

dta_d2 <- dta_d[, mean(DIFF, na.rm = T), by = .(variable, SEZONA, PER, POV)]
dta_d2[V1 <= 0 ,fill:= "red"]
dta_d2[V1 > 0 ,fill:= "blue"]

############### MESICNI ###############

dta_d2 <- dta_d[, mean(DIFF, na.rm = T), by = .(variable, month, PER, POV)]
dta_d2[POV == "Brejlsky potok", AREA:= bp_area]
dta_d2[POV == "Karluv luh", AREA:= kl_area]
dta_d2[,NORM:= V1 / AREA * 1000]

dta_d2[V1 <= 0 ,fill:= "red"]
dta_d2[V1 > 0 ,fill:= "blue"]

ggplot()+
  geom_boxplot(data = BP_DIST_m, aes(x=as.factor(month), y = value, group = interaction(month, LU), fill = LU))+
  #geom_point(data = eddy_m, aes(as.factor(month), y = value, group = month), color = "#377eb8", shape = 4, size =3)+
  #geom_point(data = eddy_m, aes(as.factor(month), y = value, group = month), color = "#377eb8", shape = 16)+
  scale_fill_manual(values = c("#e41a1c", "#4daf4a"))+
  #scale_x_discrete(breaks = c(1:12), labels = c(1:12))+
  facet_wrap(.~variable, scales = "free_y", ncol = 1, strip.position = "right")+
  theme_bw()+
  theme(axis.title.y = element_blank(), legend.position = "bottom")+
  labs(fill = "", x = "Mesic")

######### SEZONA ##############

ggplot()+
  geom_bar(data = dta_d2[!variable %in% c("Srazky", "Teplota")] %>% mutate(var_pov = paste0(variable, ' - ', POV)), aes(x = PER, y= V1, group = SEZONA, fill = fill), colour = "black",position = position_dodge(), stat = "identity")+
  #geom_text(data = BP_DIST_d2[!variable %in% c("PREC", "TEMP")], aes(x = PER, y= 0, group = SEZONA, label=SEZONA), vjust=1, position = position_dodge(width = 0.9))+
  #geom_point(data = eddy_m, aes(as.factor(month), y = value, group = month), color = "#377eb8", shape = 4, size =3)+
  #geom_point(data = eddy_m, aes(as.factor(month), y = value, group = month), color = "#377eb8", shape = 16)+
  #scale_colour_manual(values = c("#e41a1c", "#4daf4a", "#984ea3", "#377eb8"))+
  scale_fill_manual(values = c("#377eb8", "#e41a1c"))+
  #scale_x_discrete(breaks = c(1:12), labels = c(1:12))+
  #facet_wrap(POV~variable, scales = "free_y", ncol = 1, strip.position = "right")+
  facet_wrap(~var_pov, scales = "free_y", ncol = 2)+
  theme_bw()+
  theme(axis.title.y = element_blank(), legend.position = "none")+
  labs(fill = "", x = "")

######### MESIC ##############

ggplot()+
  geom_bar(data = dta_d2[!variable %in% c("Srazky", "Teplota")] %>% mutate(var_pov = paste0(variable, ' - ', POV)), aes(x = PER, y= V1, group = month, fill = fill), colour = "black",position = position_dodge(), stat = "identity")+
  #geom_text(data = BP_DIST_d2[!variable %in% c("PREC", "TEMP")], aes(x = PER, y= 0, group = SEZONA, label=SEZONA), vjust=1, position = position_dodge(width = 0.9))+
  #geom_point(data = eddy_m, aes(as.factor(month), y = value, group = month), color = "#377eb8", shape = 4, size =3)+
  #geom_point(data = eddy_m, aes(as.factor(month), y = value, group = month), color = "#377eb8", shape = 16)+
  #scale_colour_manual(values = c("#e41a1c", "#4daf4a", "#984ea3", "#377eb8"))+
  scale_fill_manual(values = c("#377eb8", "#e41a1c"))+
  #scale_x_discrete(breaks = c(1:12), labels = c(1:12))+
  #facet_wrap(POV~variable, scales = "free_y", ncol = 1, strip.position = "right")+
  facet_wrap(~var_pov, scales = "free_y", ncol = 2)+
  theme_bw()+
  theme(axis.title.y = element_blank(), legend.position = "none")+
  labs(fill = "", x = "")

ggplot(dta_d_m[variable.1 != "DIFF" & !variable %in% c("Srazky", "Teplota")])+
  geom_line(aes(x = DTM, y = value, colour = interaction(POV, variable.1)))+
  facet_wrap(~ variable, scales = "free_y", ncol = 1)

ggplot(dta_d_cum[!variable %in% c("Srazky", "Teplota")])+
  geom_line(aes(x = DTM, y = V1, colour = POV))+
  facet_wrap(~variable, scales = "free_y", ncol = 1)


dta_d[, sort:= sort(`Forest [mm]`, index.return = T)$ix, by = .(month, variable, POV)]

max(dta_d$sort)


a <- ggplot(dta_d[variable %in% c("Celkovy odtok") & POV == "Brejlsky potok"] )+
  geom_point(aes(x = `Arable land [mm]`, y = `Forest [mm]`, group= month))+
  geom_smooth(aes(x = `Arable land [mm]`, y = `Forest [mm]`), method = "lm")+
  geom_line(aes(x = seq(from = 0, to =max(`Forest [mm]`), length.out = 720), y = seq(from = 0, to =max(`Forest [mm]`), length.out = 720)))+
  facet_wrap(~month, nrow = 1)+
  #coord_fixed()+
  theme_bw()+
  theme(axis.title.y = element_blank(), legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank(), strip.text = element_blank())+
  labs(title = "Celkovy odtok", fill = "", x = "")

gg_list <- list()

names <- dta_d[!variable %in% c("Srazky", "Teplota", "Celkovy odtok"), sort(unique(variable))]

for (i in 1:length(names)){
  
  b <- ggplot(dta_d[variable %in% names[i] & POV == "Brejlsky potok"] )+
    geom_point(aes(x = `Arable land [mm]`, y = `Forest [mm]`, group= month))+
    geom_smooth(aes(x = `Arable land [mm]`, y = `Forest [mm]`), method = "lm")+
    geom_line(aes(x = seq(from = 0, to =max(`Forest [mm]`), length.out = 720), y = seq(from = 0, to =max(`Forest [mm]`), length.out = 720)))+
    facet_wrap(~month, nrow = 1)+
    #coord_fixed()+
    theme_bw()+
    theme(axis.title.y = element_blank(), legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank(), strip.text = element_blank())+
    labs(title = names[i], fill = "", x = "")
  
  gg_list[[i]] <- b
  
}

ggarrange(a, gg_list[[1]], gg_list[[2]], gg_list[[3]], gg_list[[4]], gg_list[[5]], gg_list[[6]], gg_list[[7]], gg_list[[8]], ncol = 1)
