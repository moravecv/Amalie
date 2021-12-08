setwd("C:/Users/Moravec/Downloads/Vojta_a_Martin/")


bp_dbf <- read.dbf("./shp/BP_Drainage_FG/BP_D_FG.dbf")


bp_dist <- readRDS("C:/Users/Moravec/Downloads/BP_dist.rds")

colnames(bp_dist)[24] <- "OBJECTID"
bp_dist$OBJECTID <- as.numeric(bp_dist$OBJECTID)

BP_DIST <- merge(x = bp_dist, y = bp_dbf %>% select(OBJECTID,Land_Use), by = "OBJECTID")

BP_DIST <- BP_DIST %>% select(DTM, LU = Land_Use, PREC, TEMP, TOTR, BASF, SURS, SOIS, INTS, DIRR, PERC, GROS, AET, EVAC, EVAS, EVBS)

unique(BP_DIST$LU)

BP_DIST <- BP_DIST[LU %in% c("Forest", "Arable land", "Meadow")]
BP_DIST[LU == "Meadow", LU:= "Arable land"]

BP_DIST_m <- melt(BP_DIST, id.vars = c("DTM", "LU"))

BP_DIST_m <- BP_DIST_m[, mean(value), by = .(DTM, LU, variable)]

BP_DIST_m <- BP_DIST_m[,{mean=mean(V1); sum=sum(V1); list(mean=mean, sum=sum)}, by = .(year(DTM), month(DTM), LU, variable)]

BP_DIST_m[, val:= ifelse(test = variable %in% c("SOIS", "GROS", "SURS", "TEMP"), yes = mean, no = sum)]

BP_DIST_m$mean <- BP_DIST_m$sum <- NULL

BP_DIST <- data.table(dcast(BP_DIST_m, formula = year+month+LU~variable, value.var = "val"))

BP_DIST[,AETT:= AET+EVAC+EVAS+EVBS]

BP_DIST$AET <- BP_DIST$EVAC <- BP_DIST$EVAS <- BP_DIST$EVBS <- NULL

BP_DIST_m <- melt(BP_DIST, id.vars = c("year", "month", "LU"))

BP_DIST_m[month %in% c(12,1,2), SEZONA:= "zimni"]
BP_DIST_m[month %in% c(3,4,5), SEZONA:= "jarni"]
BP_DIST_m[month %in% c(6,7,8), SEZONA:= "letni"]
BP_DIST_m[month %in% c(9,10,11), SEZONA:= "podzimni"]

BP_DIST_m[year %in% c(1961:1980), PER:= "1961-1980"]
BP_DIST_m[year %in% c(1981:2000), PER:= "1981-2000"]
BP_DIST_m[year %in% c(2001:2020), PER:= "2001-2020"]

BP_DIST_d <- data.table(dcast(BP_DIST_m, formula = year+month+variable+SEZONA+PER~LU, value.var = "value"))
BP_DIST_d[, DIFF:= (Forest / `Arable land` - 1) * 100 ]
BP_DIST_d[DIFF == Inf, DIFF:= NA ]
BP_DIST_d[DIFF <= 0 ,fill:= "red"]
BP_DIST_d[DIFF > 0 ,fill:= "blue"]
BP_DIST_d2 <- BP_DIST_d[, mean(DIFF, na.rm = T), by = .(variable, SEZONA, PER)]
BP_DIST_d2[V1 <= 0 ,fill:= "red"]
BP_DIST_d2[V1 > 0 ,fill:= "blue"]


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

ggplot()+
  geom_bar(data = BP_DIST_d2[!variable %in% c("PREC", "TEMP")], aes(x = PER, y= V1, group = SEZONA, colour = SEZONA, fill = fill), position = position_dodge(), stat = "identity")+
  #geom_text(data = BP_DIST_d2[!variable %in% c("PREC", "TEMP")], aes(x = PER, y= 0, group = SEZONA, label=SEZONA), vjust=1, position = position_dodge(width = 0.9))+
  #geom_point(data = eddy_m, aes(as.factor(month), y = value, group = month), color = "#377eb8", shape = 4, size =3)+
  #geom_point(data = eddy_m, aes(as.factor(month), y = value, group = month), color = "#377eb8", shape = 16)+
  scale_colour_manual(values = c("#e41a1c", "#4daf4a", "#984ea3", "#377eb8"))+
  scale_fill_manual(values = c("#377eb8", "#e41a1c"))+
  #scale_x_discrete(breaks = c(1:12), labels = c(1:12))+
  facet_wrap(.~variable, scales = "free_y", ncol = 1, strip.position = "right")+
  theme_bw()+
  theme(axis.title.y = element_blank(), legend.position = "bottom")+
  labs(fill = "", x = "Mesic")

ggplot(BP_DIST_d[variable == "SURS"])+
  geom_tile(aes(x=year, y = month, fill = DIFF))+
  facet_wrap(.~variable, ncol = 1, strip.position = "right")+
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0)
