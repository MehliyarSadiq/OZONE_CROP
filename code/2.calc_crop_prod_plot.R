# calculate crop production
library(fields)
source('~/Desktop/analysis/code/get_geo.R')
lat = seq(-90,90,2)
lon = seq(-180,177.5,2.5)

load('~/Desktop/analysis/data/crop/crop.FAO-USDA.hres.0.5x0.5.1961-2010.RData')

prod = array(NaN, dim=dim(crop.yield.5yma[,,50,]))
for(i in 1:4) prod[,,i] = crop.yield.5yma[,,50,i] * harvest.area[,,50,i]
for(i in 1:4) plot.field(prod[,,i], lon.0.5, lat.0.5)

prod_2degree = array(NaN, dim=c(144,91,4))

lonlon = replicate(length(lat.0.5), lon.0.5)
latlat = t(replicate(length(lon.0.5), lat.0.5))
lon.array = array(lonlon)
lat.array = array(latlat)
latlon = cbind(lat.array, lon.array)

for(i in 1:4) {
  plot.field(prod[,,i], lon.0.5, lat.0.5)
  print(paste(crop.name[i], 'before regriding:', sum(prod[,,i], na.rm = T)))
  
  for(ilat in 1:(length(lat)-1))
    for(ilon in 1:length(lon)){ # summing up grids belong to coarser grid
      subset.0.5 = prod[(1+5*(ilon-1)):(5*ilon),(1+4*(ilat-1)):(4*ilat),i]
      prod_2degree[ilon,ilat,i] = sum(subset.0.5, na.rm = T)
    }
  #prod.array = array(prod[,,i])
  #prod_2degree[,,i] = sp.regrid(spdata = prod[,,i], lon.in = lon.0.5, lat.in = lat.0.5, lon.out = lon, lat.out = lat)
  #print(paste(crop.name[i], 'after regriding:', sum(prod_2degree[,,i], na.rm = T)))
  #prod_2degree[,,i] = spavg(spdata = prod.array, latlon = latlon, lat.frame = lat, lon.frame = lon)
  print(paste(crop.name[i], 'after regriding:', sum(prod_2degree[,,i], na.rm = T)))
  plot.field(prod_2degree[,,i], lon, lat)
}
crop.list = c('Wheat', 'Rice', 'Maize', 'Soybean')
for(i in 1:4) assign(x = paste0('prod_', crop.list[i]), value = prod_2degree[,,i])

setwd('~/Desktop/analysis/data/crop')
unit.info = 'crop.prod: ton/grid cell'
crop.types = crop.list
save(list = c('prod_Maize', 'prod_Rice', 'prod_Soybean', 'prod_Wheat', 'crop.types', 'unit.info'), file = 'crop_prod.RData')

