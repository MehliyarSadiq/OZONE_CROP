# crop production mask >500Mg/grid cell
library(fields)
source('~/Desktop/analysis/code/get_geo.R')

crop.types=c("Maize", "Wheat", "Soybean", "Rice")
load('~/Desktop/analysis/data/crop/crop_prod.RData')
lat = seq(-90,90,2)
lon = seq(-180,177.5,2.5)

for(crop.type in crop.types){
  assign(x = paste0('mask_', crop.type), value = ifelse(get(paste0('prod_', crop.type))>500, yes = 1, no = NaN))
  plot.field(get(paste0('mask_', crop.type)), lon, lat, type = 'sign')
}

setwd('~/Desktop/analysis/data/crop')
save(list = c('lat', 'lon', 'crop.types', 'mask_Maize', 'mask_Wheat', 'mask_Soybean', 'mask_Rice'), file = 'crop_mask.RData')
