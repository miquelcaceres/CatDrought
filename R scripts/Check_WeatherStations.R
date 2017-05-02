library(meteoland)
AEMET_path = "D:/Recerca/Datasets/Climate/AEMET/"

dates = seq(as.Date("2017-01-01"), Sys.Date()-1, by="day")
ndays = length(dates)
day_data = vector("list",ndays)
for(i in 1:ndays) {
  day_data[[i]] = readmeteorologypoint(paste0(AEMET_path,"Download/DailyCAT/",as.character(dates[i]),".txt"))
}
AEMET_stcodes = rownames(day_data[[1]])
AEMET_latlon = cbind(day_data[[1]]$coords.x1,day_data[[1]]$coords.x2)
AEMET_elevation = day_data[[1]]$elevation
AEMET_slope = rep(0, length(AEMET_elevation))
AEMET_aspect = rep(0, length(AEMET_elevation))
AEMET_latlon_sp = SpatialPoints(AEMET_latlon,CRS("+proj=longlat"))
AEMET_utm31_sp = spTransform(AEMET_latlon_sp, CRS("+proj=utm +zone=31"))

nstations = length(AEMET_stcodes)

cumulativePrecipitation = rep(0, nstations)
names(cumulativePrecipitation) = AEMET_stcodes
for(i in 1:ndays) {
  codes = rownames(day_data[[i]])
  p = day_data[[i]]$Precipitation
  p[is.na(p)] = 0
  cumulativePrecipitation[codes] = cumulativePrecipitation[codes] + p
}
cat(paste("Stations with zero precipitation: ", paste(AEMET_stcodes[which(cumulativePrecipitation==0)],collapse = ", "),"\n"))
