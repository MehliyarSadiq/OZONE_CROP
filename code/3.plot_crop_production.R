library(fields); library(maps)
#crop production plot
source('~/Desktop/analysis/code/get_geo.R')
load('~/Desktop/analysis/data/crop/crop_prod.RData')
lat = seq(-90,90,2)
lon = seq(-180,177.5,2.5)
crop.types = c('Maize', 'Wheat', 'Soybean', 'Rice')

setwd('~/Desktop/analysis/figures/yield/')
#
# save following image manually
tmp = prod_Rice
tmp = ifelse(prod_Rice>500, yes = prod_Rice, no = NaN)
pdf('yield_legend.pdf', width = 8.5, height = 8.5)
plot.field(tmp[6:144,17:84]*1e-5, lon[6:144], lat[17:84], col = tim.colors(n=32)[17:32], type = 'def', zlim=c(0,20), legend.only = T)
dev.off()

for(crop.type in crop.types){
  tmp = get(paste0('prod_', crop.type))
  tmp = ifelse(tmp>500, yes = tmp, no = NaN)
  plot.field(tmp[6:144,17:84]*1e-5, lon[6:144], lat[17:84], col = tim.colors(n=32)[17:32], type = 'def', zlim=c(0,20), image.only = T)
  pdf(paste0('yield_', crop.type,'.pdf'), width = 14, height = 8.5)
    plot.field(tmp[6:144,17:84]*1e-5, lon[6:144], lat[17:84], col = tim.colors(n=32)[17:32], type = 'def', zlim=c(0,20), image.only = T)
  dev.off()
}
