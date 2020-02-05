# plots of harvest date
source('~/Desktop/analysis/code/get_geo.R')
#
load('~/Desktop/analysis/data/crop/crop_mask.RData')
setwd('~/Desktop/analysis/data/')
load('./dose/pod_dose.RData')

# harvest dates for 4 crops
setwd('~/Desktop/0828 debug/analysis/figures/harvest/')

pdf('harvest_date_legend.pdf', width = 14, height = 8.5)
plot.field(harvesting_date, lon, lat, type = 'def', zlim=c(0,365), legend.only = T)
dev.off()

for(i in 1:length(crop.types)){
  tmp = harvesting_date[,,i] * get(paste0('mask_', crop.types[i]))
  pdf(paste0('harvest_date_', crop.types[i], '.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type = 'def', zlim=c(0,365), image.only = T)
  dev.off()
}

end_of_month = c(1,31,59,90,120,151,181,212,243,273,304,334,365)
month_harvest = array(NaN, dim = dim(harvesting_date))
for(icrop in 1:length(crop.types))
  for(i in 1:length(lon))
    for(j in 1:length(lat))
      for(imonth in 1:12){
        if(is.na(harvesting_date[i,j,icrop])) next
        
        if(harvesting_date[i,j,icrop] > end_of_month[imonth] & harvesting_date[i,j,icrop] <= end_of_month[imonth+1]) {
          month_harvest[i,j,icrop] = imonth
          next
        }
        
        else if(harvesting_date[i,j,icrop] > end_of_month[12]) month_harvest[i,j,icrop] = 12
      }
        
for(i in 1:4) plot.field(month_harvest[,,i], lon, lat, type='def', zlim=c(1,12), col = tim.colors(n=12))

setwd('~/Desktop/0828 debug/analysis/figures/harvest/')

pdf('harvest_month_legend.pdf', width = 14, height = 8.5)
plot.field(month_harvest[,,1], lon, lat, type = 'def', zlim=c(1,12), col = tim.colors(n=12), legend.only = T)
dev.off()

for(i in 1:length(crop.types)){
  tmp = month_harvest[,,i] * get(paste0('mask_', crop.types[i]))
  pdf(paste0('harvest_month_', crop.types[i], '.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type = 'def', zlim=c(1,12), col = tim.colors(n=12), image.only = T)
  dev.off()
}
