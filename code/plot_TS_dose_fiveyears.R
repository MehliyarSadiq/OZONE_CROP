library(ncdf4);library(maps); library(fields); library(ggplot2); library(gridExtra)
setwd('~/Desktop/0828 debug/analysis/code/')

#source('../code/get_geo.R')

lon = seq(-180,177.5,2.5)
lat = seq(-90,90,2)
year_sq = c(2006:2018)

for(year in year_sq){
case=paste0('dose_', year)

setwd('~/Desktop/0828 debug/analysis/data/dose')
# processed output from simulations
load(paste0(case, '_extract.RData')) # processed output data

load("~/Desktop/0828 debug/analysis/data/crop/crop_planting_harvesting_Sack_2x2.5.RData")     # calendar to identify accumulation period
setwd('~/Desktop/0828 debug/analysis/figures/TSs/dose')

# look at time evolution of factors of four major crops
crop.type = c('Maize', 'Wheat', 'Soybean', 'Rice') # 1, 2, 4, 5,
grid = matrix(c(37,66,36,68,37,66,117,60), nrow = 4, ncol = 2, byrow = TRUE)
if(year == 2008) one_year = 1:366
else one_year = 1:365

subset.ind = c(1,2,4,5)
pod_subset = POD_daily[,,subset.ind,]
flux_subset = O3_flux_daily[,,subset.ind,]
cond_subset = g_s_daily[,,subset.ind,]

f_phen_subset = f_phen_daily[,,subset.ind,]
f_t_subset = f_t_daily[,,subset.ind,]
f_d_subset = f_d_daily[,,subset.ind,]
f_light_subset = f_light_daily[,,subset.ind,]

lai_sun_subset = LAI_sun_daily[,,subset.ind,]
lai_sha_subset = LAI_sha_daily[,,subset.ind,]

harvest_date_subset = prescribed_harvesting_date_Sack[,,c(1,2,4,5)]

for(i in 1:4){
  #i = 2 # Kansas, Maize
  
  #get accumulation period
  harvest_date = as.integer(harvest_date_subset[grid[i,1],grid[i,2],i])
  period = c((harvest_date-14-90):(harvest_date-14))
  
  pod = pod_subset[grid[i,1],grid[i,2],i,] # rainfed crops
  p1 = qplot(x = one_year, y = pod, geom = 'line', xlab = 'day', ylab = 'POD', main = paste('POD for', crop.type[i])) +
    annotate("rect", xmin = period[1], xmax = max(period), ymin = 0, ymax = Inf, alpha = .4)
  #print(p1)
  
  flux = flux_subset[grid[i,1],grid[i,2],i,]
  p2 = qplot(x = one_year, y = flux, geom = 'line', xlab = 'day', ylab = 'Flux', main = paste('Ozone flux for', crop.type[i]))+
    annotate("rect", xmin = period[1], xmax = max(period), ymin = 0, ymax = Inf, alpha = .4)
  #print(p2)
  
  gs = cond_subset[grid[i,1],grid[i,2],i,]
  p3 = qplot(x = one_year, y = gs, geom = 'line', xlab = 'day', ylab = 'Conductance', main = paste('Conductance for', crop.type[i]))+
    annotate("rect", xmin = period[1], xmax = max(period), ymin = 0, ymax = Inf, alpha = .4)
  #print(p3)
  
  # grouping
  grid.arrange(p1,p2,p3, nrow = 1)
  jpeg(paste0(case, '_', crop.type[i],'_POD_flux_gs.jpg'), width = 1200, height = 250)
  grid.arrange(p1,p2,p3, nrow = 1)
  dev.off()
  
  # factors
  phen = f_phen_subset[grid[i,1],grid[i,2],i,]
  p4 = qplot(x = one_year, y = phen, geom = 'line', xlab = 'day', ylab = ' ', main = 'Phenological factor') +
    annotate("rect", xmin = period[1], xmax = max(period), ymin = 0, ymax = Inf, alpha = .4)
  #print(p4)
  
  ft = f_t_subset[grid[i,1],grid[i,2],i,]
  p5 = qplot(x = one_year, y = ft, geom = 'line', xlab = 'day', ylab = ' ', main = 'Temperature factor') +
    annotate("rect", xmin = period[1], xmax = max(period), ymin = 0, ymax = Inf, alpha = .4)
  #print(p5)
  
  fd = f_d_subset[grid[i,1],grid[i,2],i,]
  p6 = qplot(x = one_year, y = fd, geom = 'line', xlab = 'day', ylab = ' ', main = 'VPD factor') +
    annotate("rect", xmin = period[1], xmax = max(period), ymin = 0, ymax = Inf, alpha = .4)
  #print(p6)
  
  fl = f_light_subset[grid[i,1],grid[i,2],i,]
  p7 = qplot(x = one_year, y = fl, geom = 'line', xlab = 'day', ylab = ' ', main = 'Light factor') +
    annotate("rect", xmin = period[1], xmax = max(period), ymin = 0, ymax = Inf, alpha = .4)
  #print(p7)
  
  # grouping
  grid.arrange(p4,p5,p6,p7, nrow = 2)
  jpeg(paste0(case, '_', crop.type[i], '_factors.jpg'), width = 1000, height = 600)
  grid.arrange(p4,p5,p6,p7, nrow = 2)
  dev.off()
  
  lai = lai_sha_subset[grid[i,1],grid[i,2],i,] + lai_sun_subset[grid[i,1],grid[i,2],i,]
  p8 = qplot(x = one_year, y = lai, geom = 'line', xlab = 'day', ylab = ' ', main = 'LAI') +
    annotate("rect", xmin = period[1], xmax = max(period), ymin = 0, ymax = Inf, alpha = .4)
  print(p8)
}
}

