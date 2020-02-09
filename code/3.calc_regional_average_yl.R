# RY regional averages
library(maps); library(fields); library(ggplot2)
source('~/Desktop/analysis/code/get_geo.R')
load('~/Desktop/analysis/data/crop/crop_prod.RData')
# define regions,
# Jacky: 
# USA: lon : -125 to -65, lat: 22 - 49 # USA: lon[23:47], lat[57:70]
# Arg + Bra: lon: -80 to -30, lat -50 to 0 # Arg + Bra: lon[41:61], lat[21:46]
# Europe: lon -10 to 35, lat: 35 to 55 # Europe: lon[69:87], lat[63:73]
# China + india: lon: 70 - 140, lat: 5 to 55 # China + India: lon[101:129], lat[48:73]
# 4 methods, 4 crops, 4 regions

regions = c('US', 'SA', 'EU', 'EA')
for(region in regions) assign(x = paste0('mask_', region), value = array(NaN, dim=dim(prod_Maize)))
mask_US[23:47,57:70] = 1
mask_SA[41:61,21:46] = 1
mask_EU[69:87,63:73] = 1
mask_EA[101:129,48:73] = 1
for(region in regions) plot.field(get(paste0('mask_', region)), lon, lat, type = 'sign')

setwd('~/Desktop/analysis/data/crop/ry/')

for(method in c('POD3_FBB', 'POD3_DOSE', 'AOT40', 'W126')){
  load(paste0('ry_', method, '.RData'))
  yl.avg = array(NaN, dim=c(4,4)) # store yield loss for a method
  
  for(icrop in 1:length(crop.types))
    for(iregion in 1:length(regions)){
      # regional calculation, same as global
      prod = get(paste0('prod_', crop.types[i]))*get(paste0('mask_', regions[iregion])) # map: ton per grid cell
      ry = get(paste0('ry_', crop.types[icrop], '_', method)) # relative yield map
      prod_untact = prod / ry # map: theoretical production, unaffected by ozone
      prod.theo = sum(prod_untact, na.rm = T) # number: theoretical total production
      prod_loss = prod - prod_untact       # map: production loss (tons)
      prod.loss = sum(prod_loss, na.rm = T) # number: total loss (tons)
      plot.field(prod_loss, lon , lat, col = rev(tim.colors(n=32)[17:32]), type = 'def', zlim = c(-40000,0))
      
      yl.avg[icrop, iregion] = -100*prod.loss/prod.theo # number: prod loss (%)
      print(paste(method, crop.types[icrop], regions[iregion], 'prod loss (%): ', yl.avg[icrop, iregion]))
    }
  crops = rep(crop.types, 4)
  region = c(rep(regions[1], 4), rep(regions[2], 4), rep(regions[3], 4), rep(regions[4], 4))
  yield.loss.avg = as.vector(yl.avg)
  assign(x = paste0('df_',method), value = data.frame(crops, region, method = method, yield.loss.avg))
}

# M7/M12
load('ry_m7-m12.RData')
group = c('ry_Maize_M12', 'ry_Wheat_M7', 'ry_Soybean_M12', 'ry_Rice_M7')
yl.avg = array(NaN, dim=c(4,4))

for(icrop in 1:length(crop.types))
  for(iregion in 1:length(regions)){
    # regional calculation, same as global
    prod = get(paste0('prod_', crop.types[i]))*get(paste0('mask_', regions[iregion])) # map: ton per grid cell
    ry = get(group[icrop]) # relative yield map
    prod_untact = prod / ry # map: theoretical production, unaffected by ozone
    prod.theo = sum(prod_untact, na.rm = T) # number: theoretical total production
    prod_loss = prod - prod_untact       # map: production loss (tons)
    prod.loss = sum(prod_loss, na.rm = T) # number: total loss (tons)
    plot.field(prod_loss, lon , lat, col = rev(tim.colors(n=32)[17:32]), type = 'def', zlim = c(-40000,0))
    
    yl.avg[icrop, iregion] = -100*prod.loss/prod.theo # number: prod loss (%)
    print(paste(method, crop.types[icrop], regions[iregion], 'prod loss (%): ', yl.avg[icrop, iregion]))
  }
yield.loss.avg = as.vector(yl.avg)
assign(x = 'df_m', value = data.frame(crops, region, method = 'M7/M12', yield.loss.avg))

# combine all dfs
df_all = rbind(df_AOT40, df_m, df_W126, df_POD3_DOSE, df_POD3_FBB)

p = ggplot(df_all, aes(fill = method, y = yield.loss.avg, x = region)) +
  geom_bar(position = 'dodge', stat='identity') +
  facet_wrap(~crops) +
  #coord_cartesian(ylim=c(0.8,1)) +
  theme(legend.title = element_blank()) +
  labs(x = 'Regions', y = 'Yield loss (%)', title = ' ')
print(p)

p = ggplot(df_all, aes(fill = crops, y = yield.loss.avg, x = region)) +
  geom_bar(position = 'dodge', stat='identity') +
  facet_wrap(~method) +
  #coord_cartesian(ylim=c(0.8,1)) +
  theme(legend.title = element_blank()) +
  labs(x = 'Regions', y = 'Yield loss (%)', title = ' ')
print(p)

save(list = c('df_all', 'crop.types', 'mask_EA', 'mask_EU', 'mask_SA', 'mask_US'), file = 'region.masks.RData')
