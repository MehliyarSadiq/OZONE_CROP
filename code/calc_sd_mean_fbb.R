# draw global contour plots of POD3 in ggplot2
library(ncdf4);library(maps); library(fields);library(ggplot2); library(gridExtra);
source('~/Desktop/0828 debug/analysis/code/get_geo.R')
load('~/Desktop/0828 debug/analysis/data/crop/crop_planting_harvesting_Sack_2x2.5.RData')
load('~/Desktop/0828 debug/analysis/data/crop/crop_mask.RData')

setwd("~/Desktop/0828 debug/analysis/data/fbb/")
crop.types = c('Maize','Wheat','Soybean','Rice') # 1, 2, 4, 5,
lat = seq(-90,90,2)
lon = seq(-180,177.5,2.5)

subset_index = c(1,2,4,5) # index corresponding to the above 4 crop types
year_seq = c(2010:2018) # present-day

for(i in 1:length(crop.types)){
  tmp = array(NaN, dim=c(length(lon), length(lat), length(year_seq)))
  
  for(year in year_seq){
    load(paste0('fbb_', year, '_extract.RData'))
    
    subset = POD_daily[,,subset_index,]
    
    pod = apply(X = subset[,,i,], MARGIN = c(1,2), FUN = max, na.rm = TRUE)
    pod[which(!is.finite(pod))] = NaN
    
    tmp[,,year-2009] = pod
    #assign(x = paste0('pod_', year, '_', crop.types[i]), value = pod) # looking at output maps year by year
  }
  tmp_mean = apply(X = tmp, MARGIN = 1:2, FUN = mean, na.rm = T) # annual mean
  tmp_sd = apply(X = tmp, MARGIN = 1:2, FUN = sd, na.rm = T)
  
  assign(x = paste0('mean_POD3_FBB_', crop.types[i]), value = tmp_mean)
  assign(x = paste0('sd_POD3_FBB_', crop.types[i]), value = tmp_sd)
}

for(i in 1:length(crop.types))
  plot.field(100*get(paste0('mask_', crop.types[i]))*get(paste0('sd_POD3_FBB_', crop.types[i]))/get(paste0('mean_POD3_FBB_', crop.types[i])), lon, lat, type='def', zlim=c(0,30))

harvesting_date = prescribed_harvesting_date_Sack[,,subset_index]

save(list = c('lon', 'lat', 'year_seq', 'crop.types', 'mean_POD3_FBB_Maize', 'mean_POD3_FBB_Wheat', 'mean_POD3_FBB_Soybean', 'mean_POD3_FBB_Rice', 'sd_POD3_FBB_Maize', 'sd_POD3_FBB_Wheat', 'sd_POD3_FBB_Soybean', 'sd_POD3_FBB_Rice', 'harvesting_date'), file = 'mean_sd_POD3_FBB.RData')
