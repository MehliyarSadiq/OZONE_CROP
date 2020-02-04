# exposure metrics regional averages
library(maps); library(fields); library(ggplot2)
source('~/Desktop/0828 debug/analysis/code/get_geo.R')
lat = seq(-90,90,2)
lon = seq(-180,177.5,2.5)
load('~/Desktop/0828 debug/analysis/data/crop/crop_mask.RData')
# define regions,
regions = c('US', 'SA', 'EU', 'EA')
for(region in regions) assign(x = paste0('mask_', region), value = array(NaN, dim=dim(mask_Maize)))
mask_US[23:47,57:70] = 1
mask_SA[41:61,21:46] = 1
mask_EU[69:87,63:73] = 1
mask_EA[101:129,48:73] = 1

for(region in regions) plot.field(get(paste0('mask_', region)), lon, lat, type = 'sign')

setwd('~/Desktop/0828 debug/analysis/data/ozone/')
for(method in c('POD3_FBB', 'POD3_DOSE', 'AOT40', 'W126')){
  load(paste0(method, '.RData'))
  metric.avg = array(NaN, dim=c(4,4))
  metric.sd = array(NaN, dim=c(4,4))
  
  for(icrop in 1:length(crop.types))
    for(iregion in 1:length(regions)){
      tmp.avg = mean(get(paste0(method, '_', crop.types[icrop]))*get(paste0('mask_', crop.types[icrop]))*get(paste0('mask_', regions[iregion])), na.rm = T)
      tmp.sd = sd(get(paste0(method, '_', crop.types[icrop]))*get(paste0('mask_', crop.types[icrop]))*get(paste0('mask_', regions[iregion])), na.rm = T)
      
      metric.avg[icrop, iregion] = tmp.avg
      metric.sd[icrop, iregion] = tmp.sd
    }
  crops = rep(crop.types, 4)
  region = c(rep(regions[1], 4), rep(regions[2], 4), rep(regions[3], 4), rep(regions[4], 4))
  ozone.metric.avg = as.vector(metric.avg)
  ozone.metric.sd = as.vector(metric.sd)
  assign(x = paste0('df_',method), value = data.frame(crops, region, method = method, ozone.metric.avg, ozone.metric.sd))
}

# M7/M12
load('M7.RData')
load('M12.RData')
group = c('M12_Maize', 'M7_Wheat', 'M12_Soybean', 'M7_Rice')
metric.avg = array(NaN, dim=c(4,4))
metric.sd = array(NaN, dim=c(4,4))

for(icrop in 1:length(crop.types))
  for(iregion in 1:length(regions)){
    tmp.avg = mean(get(group[icrop])*get(paste0('mask_', crop.types[icrop]))*get(paste0('mask_', regions[iregion])), na.rm = T)
    tmp.sd = sd(get(group[icrop])*get(paste0('mask_', crop.types[icrop]))*get(paste0('mask_', regions[iregion])), na.rm = T)
    #tmp = 100 * (1-tmp)
    metric.avg[icrop, iregion] = tmp.avg
    metric.sd[icrop, iregion] = tmp.sd
  }
ozone.metric.avg = as.vector(metric.avg)
ozone.metric.sd = as.vector(metric.sd)
assign(x = 'df_m', value = data.frame(crops, region, method = 'M7/M12', ozone.metric.avg, ozone.metric.sd))


# combine all dfs
df_all = rbind(df_AOT40, df_m, df_W126, df_POD3_DOSE, df_POD3_FBB)

p = ggplot(df_all, aes(fill = crops, y = ozone.metric.avg, x = region)) +
  geom_bar(position = 'dodge', stat='identity') +
  #geom_errorbar(aes(ymin=ozone.metric.avg-ozone.metric.sd, ymax=ozone.metric.avg+ozone.metric.sd), width=.2,
  #              position=position_dodge(.9)) +
  facet_wrap(~method, scales = 'free') +
  theme(legend.title = element_blank()) +
  labs(x = 'Regions', y = 'Ozone metric', title = 'Regional average')
print(p)

