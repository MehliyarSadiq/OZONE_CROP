# AOT40, M7/M12 and W126 calculation and plotting maps
library(maps);library(fields); library(ncdf4)
source('~/Desktop/0828 debug/analysis/code/get_geo.R')

load('~/Desktop/0828 debug/analysis/data/crop/crop_planting_harvesting_Sack_2x2.5.RData')

setwd("~/Desktop/0828 debug/analysis/data/ozone/")
year_seq = c(2006:2010) # present-day
crop.types = c('Maize','Wheat','Soybean','Rice') # 1, 2, 4, 5,
subset_index = c(1,2,4,5) # index corresponding to the above 4 crop types
harvesting_date = prescribed_harvesting_date_Sack[,,subset_index]

m7_years = array(NaN, dim=c(dim(harvesting_date),length(year_seq)))
m12_years = array(NaN, dim=c(dim(harvesting_date),length(year_seq)))
aot40_years = array(NaN, dim=c(dim(harvesting_date),length(year_seq)))
w126_years = array(NaN, dim=c(dim(harvesting_date), length(year_seq)))

for(iyear in year_seq){
#iyear = 2008
  data = nc_open(paste0('hourly_O3_', iyear,'.nc'), write = F)
  houro3 = ncvar_get(nc = data, varid = 'hourly_O3')
  lat = ncvar_get(nc = data, varid = 'lat')
  lon = ncvar_get(nc = data, varid = 'lon')
  nc_close(data)
   
  # 1, time zone shifts
  tmp = array(NA, dim=c(dim(houro3)[1:2],dim(houro3)[3]+48)) # take two more days than needed for time zone shift
  tmp[,,25:(25+dim(houro3)[3]-1)] = houro3
  tmp[,,1:24] = houro3[,,1:24]
  tmp[,,(25+dim(houro3)[3]):(dim(houro3)[3]+48)] = houro3[,,(dim(houro3)[3]-23):dim(houro3)[3]]
  
  lon.tmp = lon + 180 # 0 to 357.5
  local.houro3 = array(NA, dim=dim(houro3)) # store local time data here
  
  for(nlon in lon.tmp){
    xlon = nlon / 2.5 +1
    if(nlon > 353 | nlon < 8) local.houro3[xlon,,] = tmp[xlon,,25:(24+dim(houro3)[3])]
    else{
      nnn = floor((nlon - 7.5)/15)+1 #from 1 to 23
      local.houro3[xlon,,] = tmp[xlon,,(25-nnn):((24+dim(houro3)[3])-nnn)]
    }
  }
  # time zone shift finished
  
  # 2, calculate AOT40, M7/M12 and W126 for 4 crops and each grid cell according to harvest dates
  aot40_crops = array(NaN, dim=dim(harvesting_date))
  m7_crops = array(NaN, dim=dim(harvesting_date))
  m12_crops = array(NaN, dim=dim(harvesting_date))
  w126_crops = array(NaN, dim=dim(harvesting_date))
  
  for(icrop in 1:length(crop.types))
    for(i in 1:length(lon))
      for(j in 1: length(lat)){
        
        harvest_day = harvesting_date[i,j,icrop]
        if(is.na(harvest_day)) next
        
        end_day = harvest_day - 14 # include this day
        start_day = harvest_day - 14 - 90 # exclude this day
        if(start_day <= 1) next
        
        days = end_day - start_day # 90 days
        hours = days*24
        
        # extract 7-hour and 12-hour ozone according to definition
        period_m7 = rep(NaN, 7*days)
        period_m12 = rep(NaN, 12*days)
        period_aot= rep(NaN, 12*days)
        
        for (day in 1:days){
          period_m7[(1 + (day-1)*7):(7 + (day-1)*7)] = local.houro3[i,j,((start_day-1)*24 + 9 + (day-1)*24):((start_day-1)*24 + 15 + (day-1)*24)]
          period_m12[(1 + (day-1)*12):(12 + (day-1)*12)] = local.houro3[i,j,((start_day-1)*24 + 7 + (day-1)*24):((start_day-1)*24 + 18 + (day-1)*24)]
        }
        # 9:00-16:00, 7-hour mean, M7
        m7_crops[i,j,icrop] = mean(period_m7, na.rm = T)
        # 8:00 - 20:00, daytime mean of ozone, M12
        m12_crops[i,j,icrop] = mean(period_m12, na.rm = T)
        # 8:00 - 20:00, daytime sum of ozone above 40 ppbv, AOT40
        period_aot40 = period_m12 - 40
        period_aot40[period_aot40 <= 0] = 0 # change zeros and negatives to NA
        aot40_crops[i,j,icrop] = sum(period_aot40, na.rm = T)* 1e-3 # unit: ppm h
        # 8:00 - 20:00, W126
        period_w126 = period_m12*1e-3 #ppm
        for(ihour in 1:length(period_w126)) period_w126[ihour] = period_w126[ihour]/(1 + 4403*exp(-126*period_w126[ihour]))
        w126_crops[i,j,icrop] = sum(period_w126)
      }
  m7_years[,,,iyear-2005] = m7_crops 
  m12_years[,,,iyear-2005] = m12_crops 
  aot40_years[,,,iyear-2005] = aot40_crops
  w126_years[,,,iyear-2005] = w126_crops
  
  #for(i in 1:4) plot.field(m7_crops[,,i], lon, lat, type='def', zlim=c(0,70))
  #for(i in 1:4) plot.field(m12_crops[,,i], lon, lat, type='def', zlim=c(0,70))
  #for(i in 1:4) plot.field(aot40_crops[,,i], lon, lat, type='def', zlim=c(0,30))
}

m7_mean = apply(m7_years, 1:3, mean, na.rm = T)
m12_mean = apply(m12_years, 1:3, mean, na.rm = T)
aot40_mean = apply(aot40_years, 1:3, mean, na.rm = T)
w126_mean = apply(w126_years, 1:3, mean, na.rm = T)

#for(i in 1:4) plot.field(m7_mean[,,i], lon, lat, type='def', zlim=c(0,70))
#for(i in 1:4) plot.field(m12_mean[,,i], lon, lat, type='def', zlim=c(0,70))
for(i in 1:4) plot.field(aot40_mean[,,i], lon, lat, type='def', zlim=c(0,30))
for(i in 1:4) plot.field(w126_mean[,,i], lon, lat, type='def', zlim=c(0,40))

for(i in 1:4){
  assign(x = paste0('M7_', crop.types[i]), value = m7_mean[,,i])
  assign(x = paste0('M12_', crop.types[i]), value = m12_mean[,,i])
  assign(x = paste0('AOT40_', crop.types[i]), value = aot40_mean[,,i])
  assign(x = paste0('W126_', crop.types[i]), value = w126_mean[,,i])
}
 
save(list = c('lon', 'lat', 'crop.types', 'M7_Maize', 'M7_Wheat', 'M7_Soybean', 'M7_Rice', 'harvesting_date'), file = 'M7.RData')
save(list = c('lon', 'lat', 'crop.types', 'M12_Maize', 'M12_Wheat', 'M12_Soybean', 'M12_Rice', 'harvesting_date'), file = 'M12.RData')
save(list = c('lon', 'lat', 'crop.types', 'AOT40_Maize', 'AOT40_Wheat', 'AOT40_Soybean', 'AOT40_Rice', 'harvesting_date'), file = 'AOT40.RData')
save(list = c('lon', 'lat', 'crop.types', 'W126_Maize', 'W126_Wheat', 'W126_Soybean', 'W126_Rice', 'harvesting_date'), file = 'W126.RData')


