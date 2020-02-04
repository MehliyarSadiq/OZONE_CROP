# draw global contour plots of POD3 in ggplot2
library(ncdf4);library(maps); library(fields);library(ggplot2); library(gridExtra);
library(reshape2);library(RColorBrewer);library(gridExtra);library(cowplot)
source('~/Desktop/0828 debug/analysis/code/get_geo.R')
load('~/Desktop/0828 debug/analysis/data/crop/crop_planting_harvesting_Sack_2x2.5.RData')

setwd("~/Desktop/0828 debug/analysis/data/dose/")

crop.types = c('Maize','Wheat','Soybean','Rice') # 1, 2, 4, 5,
lat = seq(-90,90,2)
lon = seq(-180,177.5,2.5)

subset_index = c(1,2,4,5) # index corresponding to the above 4 crop types
year_seq = c(2006:2010) # present-day

for(i in 1:length(crop.types)){
  tmp = array(NaN, dim=c(length(lon), length(lat), length(year_seq)))
  
  for(year in year_seq){
  load(paste0('dose_', year, '_extract.RData'))
  
  subset = POD_daily[,,subset_index,]
  
  pod = apply(X = subset[,,i,], MARGIN = c(1,2), FUN = max, na.rm = TRUE)
  pod[which(!is.finite(pod))] = NaN
  
  tmp[,,year-2005] = pod
  #assign(x = paste0('pod_', year, '_', crop.types[i]), value = pod) # looking at output maps year by year
  }
  tmp_mean = apply(X = tmp, MARGIN = 1:2, FUN = mean, na.rm = T) # annual mean
  assign(x = paste0('POD3_DOSE_', crop.types[i]), value = tmp_mean)
}

for(i in 1:length(crop.types)) plot.field(get(paste0('POD3_DOSE_', crop.types[i])), lon, lat, type='def', zlim=c(0,25))

harvesting_date = prescribed_harvesting_date_Sack[,,subset_index]

save(list = c('lon', 'lat', 'crop.types', 'POD3_DOSE_Maize', 'POD3_DOSE_Wheat', 'POD3_DOSE_Soybean', 'POD3_DOSE_Rice', 'harvesting_date'), file = 'POD3_DOSE.RData')
# for(i in 1:5) plot.field(get(paste0('pod_', i+2005, '_Maize')), lon, lat, type='def', zlim=c(0,25))
# for(i in 1:5) plot.field(get(paste0('pod_', i+2005, '_Wheat')), lon, lat, type='def', zlim=c(0,25))
# for(i in 1:5) plot.field(get(paste0('pod_', i+2005, '_Soybean')), lon, lat, type='def', zlim=c(0,25))
# for(i in 1:5) plot.field(get(paste0('pod_', i+2005, '_Rice')), lon, lat, type='def', zlim=c(0,25))


#save(list = c('lat', 'lon'), file = 'pod_mean.RData')