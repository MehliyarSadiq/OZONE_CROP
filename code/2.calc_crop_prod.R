# calculate crop production
library(fields)
source('~/Desktop/analysis/code/get_geo.R')

# FAO crop data, 0.5 degree resolution
load('~/Desktop/analysis/data/crop/crop.FAO-USDA.hres.0.5x0.5.1961-2010.RData')

crop_prod = crop.yield*harvest.area # [lon, lat, years, crops]
dim(crop_prod)

# 2006-2010 mean crop production
crop_prod_5year_mean = apply(X = crop_prod[,,46:50,], FUN = mean, MARGIN = c(1,2,4), na.rm = T)
dim(crop_prod_5year_mean)

# lat and lon of TEMIR outputs
lat = seq(-90,90,2)
lon = seq(-180,177.5,2.5)
# output grid
prod_2degree = array(NaN, dim=c(length(lon),length(lat),length(crop.name)))

# split lon grids in half, for regridding
crop_prod_0.5x0.25 = array(NaN, dim=c(2*length(`lon.0.5`),length(`lat.0.5`),4))
dim(crop_prod_0.5x0.25)
sum(crop_prod_5year_mean[,,3], na.rm = T) # global total prod. in tonnes
for(ilon in 1:(2*length(`lon.0.5`))){
    lon.ind = as.integer((ilon+1)/2)
    crop_prod_0.5x0.25[ilon,,] = 0.5*crop_prod_5year_mean[lon.ind,,]
    }
sum(crop_prod_0.5x0.25[,,3], na.rm = T)

# before and after regridding
before = crop_prod_0.5x0.25
after = prod_2degree

for(icrop in 1:length(crop.name)){
    print(paste('global total production', crop.name[icrop], 'before regriding:', sum(before[,,icrop], na.rm = T)))

    for(ilat in 1:length(lat))
    for(ilon in 1:length(lon)){ # summing up grids belong to coarser grid
        
        # get the subset of data in finer grid that falls into coarser grid
        
        # 1, get lat indices
        if(ilat == 1) lat.ind = c(1,2) # first and last box contains only 2 finer grids
        else if(ilat == length(lat)) lat.ind = c(length(`lat.0.5`)-1,length(`lat.0.5`))
        # ilat = 2, lat.ind = 3,4,5,6, ilat = 3, lat.ind = 7,8,9,10
        else lat.ind = (3+(ilat-2)*4):(2+(ilat-1)*4)
        #print(lat.ind)
        
        # 2, get lon indices
        if(ilon == 1) lon.ind = c(1:5,(2*length(`lon.0.5`)-4):(2*length(`lon.0.5`))) 
        #ilon = 2, lon.ind = 6:15, ilon = 3, lon.ind = 16:25
        else lon.ind = (6+(ilon-2)*10):(5+(ilon-1)*10)
        
        subset = before[lon.ind,lat.ind,]
        after[ilon,ilat,icrop] = sum(subset[,,icrop], na.rm = T) # sum up the subset of data
    }
    print(paste('global total production', crop.name[icrop], 'after regriding:', sum(after[,,icrop], na.rm = T)))
    #plot.field(after[,,icrop], lon, lat)
}
prod_2degree = after

crop.types = c('Wheat', 'Rice', 'Maize', 'Soybean')
for(i in 1:4) assign(x = paste0('prod_', crop.types[i]), value = prod_2degree[,,i])
# save data
setwd('~/Desktop/analysis/data/crop')
unit.info = 'crop.prod: ton/grid cell'
save(list = c('prod_Maize', 'prod_Rice', 'prod_Soybean', 'prod_Wheat', 'crop.types', 'unit.info', 'lat', 'lon'), file = 'crop_prod.RData')

