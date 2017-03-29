library(meteoland)

AEMET_path = "D:/Recerca/Datasets/Climate/AEMET/"

#Get current date
date = Sys.Date()

#CALIBRATED PARAMETERIZATION FOR 2001
params = defaultInterpolationParams()
params$N_MinTemperature = 60
params$alpha_MinTemperature = 9.5
params$N_MaxTemperature = 60
params$alpha_MaxTemperature = 9.5
params$N_DewTemperature = 10
params$alpha_DewTemperature = 2.5
params$N_PrecipitationEvent = 3
params$alpha_PrecipitationEvent = 13
params$N_PrecipitationAmount = 60
params$alpha_PrecipitationAmount = 4
params$N_Wind = 2
params$alpha_Wind = 3
params$St_Precipitation=5
params$St_TemperatureRange=15
params$pop_crit = 0.50
params$f_max = 0.95
params$wind_height = 10
params$initial_Rp = 140000

#Get dates to be read for smoothing
j = julian(date)
jvec = (j-max(params$St_Precipitation, params$St_TemperatureRange)):j
datevec = as.Date(jvec, origin = as.Date("1970-01-01"),format="%j")

ndays = length(datevec)

#Load daily weather station data (including preceeding days)
day_data = vector("list",ndays)
for(i in 1:ndays) day_data[[i]] = readmeteorologypoint(paste0(AEMET_path,"Download/DailyCAT/",as.character(datevec[[i]]),".txt"))

AEMET_stcodes = rownames(day_data[[1]])
AEMET_latlon = cbind(day_data[[1]]$coords.x1,day_data[[1]]$coords.x2)
AEMET_elevation = day_data[[1]]$elevation
AEMET_slope = rep(0, length(AEMET_elevation))
AEMET_aspect = rep(0, length(AEMET_elevation))
AEMET_latlon_sp = SpatialPoints(AEMET_latlon,CRS("+proj=longlat"))
AEMET_utm31_sp = spTransform(AEMET_latlon_sp, CRS("+proj=utm +zone=31"))

nstations = length(AEMET_elevation)

#Re-shape data
MinTemperature = matrix(NA, nrow = nstations, ncol=ndays)
rownames(MinTemperature)=AEMET_stcodes
colnames(MinTemperature) = as.character(datevec)
MaxTemperature = MinTemperature
Precipitation = MinTemperature
RelativeHumidity = MinTemperature
Radiation = MinTemperature
WindSpeed = MinTemperature
WindDirection = MinTemperature
for(i in 1:ndays) {
  MinTemperature[,i] = day_data[[i]][AEMET_stcodes, "MinTemperature"]
  MaxTemperature[,i] = day_data[[i]][AEMET_stcodes, "MaxTemperature"]
  Precipitation[,i] = day_data[[i]][AEMET_stcodes, "Precipitation"]
  RelativeHumidity[,i] = day_data[[i]][AEMET_stcodes, "MeanRelativeHumidity"]
  Radiation[,i] = day_data[[i]][AEMET_stcodes, "Radiation"]
  WindSpeed[,i] = day_data[[i]][AEMET_stcodes, "WindSpeed"]
  WindDirection[,i] = day_data[[i]][AEMET_stcodes, "WindDirection"]
}

#Build Interpolator object
interpolator = MeteorologyInterpolationData(AEMET_utm31_sp, 
                                            elevation = AEMET_elevation, 
                                            slope = AEMET_slope, 
                                            aspect = AEMET_aspect, 
                             MinTemperature, 
                             MaxTemperature, 
                             Precipitation, 
                             RelativeHumidity, 
                             Radiation, 
                             WindSpeed, 
                             WindDirection, params = params)


#Interpolate meteorology
load("Rdata/IFN3_SPT_cat.rdata")
spm = interpolationpoints(interpolator, IFN3_SPT, date, verbose=FALSE)
save(spm, file="Rdata/temp_spm.rdata")
