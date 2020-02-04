# A quick look at the outputs
library(ncdf4);library(maps); library(fields); library(ggplot2); library(gridExtra)

lon = seq(-180,177.5,2.5)
lat = seq(-90,90,2)
year_sq = c(2006:2010)

for(year in year_sq){
case=paste0('fbb_', year)

setwd('~/Desktop/0828 debug/analysis/data/fbb')
# processed output from simulations
load(paste0(case, '_extract.RData'))
load('~/Desktop/0828 debug/analysis/data/crop/crop_planting_harvesting_Sack_2x2.5.RData') # to identify accumulation period

setwd('~/Desktop/0828 debug/analysis/figures/TSs/fbb/')

# look at time evolution of factors of three crops
crop.type = c('Maize', 'Wheat', 'Soybean', 'Rice') # 1, 2, 4, 5,
grid = matrix(c(37,66,36,68,37,66,117,60), nrow = 4, ncol = 2, byrow = TRUE)
if(year == 2008) one_year = 1:366
else one_year = 1:365

subset.ind = c(1,2,4,5)
pod_subset = POD_daily[,,subset.ind,]
flux_subset = O3_flux_daily[,,subset.ind,]
cond_subset = g_s_daily[,,subset.ind,]

phi_sun_subset = phi_sun_daily[,,subset.ind,]
phi_sha_subset = phi_sha_daily[,,subset.ind,]
lai_sun_subset = LAI_sun_daily[,,subset.ind,]
lai_sha_subset = LAI_sha_daily[,,subset.ind,]

harvest_date_subset = prescribed_harvesting_date_Sack[,,subset.ind]

for(i in 1:length(crop.type)){

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
  
  # light factor and intermediate variables
  phisun = phi_sun_subset[grid[i,1],grid[i,2],i,]
  ppsun = qplot(x = one_year, y = phisun, geom = 'line', xlab = 'day', ylab = ' ', main = 'Absorbed PAR sunlit') +
    annotate("rect", xmin = period[1], xmax = max(period), ymin = 0, ymax = Inf, alpha = .4)
  #print(ppsun)
  
  phisha = phi_sha_subset[grid[i,1],grid[i,2],i,]
  ppsha = qplot(x = one_year, y = phisha, geom = 'line', xlab = 'day', ylab = ' ', main = 'Absorbed PAR shaded', ylim = c(0,200)) + 
    annotate("rect", xmin = period[1], xmax = max(period), ymin = 0, ymax = Inf, alpha = .4)
  #print(ppsha)
  
  laisun = lai_sun_subset[grid[i,1],grid[i,2],i,]
  plsun = qplot(x = one_year, y = laisun, geom = 'line', xlab = 'day', ylab = ' ', main = 'LAI sunlit') +
    annotate("rect", xmin = period[1], xmax = max(period), ymin = 0, ymax = Inf, alpha = .4)
  #print(plsun)
  
  laisha = lai_sha_subset[grid[i,1],grid[i,2],i,]
  plsha = qplot(x = one_year, y = laisha, geom = 'line', xlab = 'day', ylab = ' ', main = 'LAI shaded') +
    annotate("rect", xmin = period[1], xmax = max(period), ymin = 0, ymax = Inf, alpha = .4)
  #print(plsha)
  
  grid.arrange(ppsun,ppsha,plsun, plsha, nrow = 2)
  jpeg(paste0(case, '_', crop.type[i], '_light_factor.jpg'), width = 800, height = 600)
  grid.arrange(ppsun,ppsha,plsun, plsha, nrow = 2)
  dev.off()
}
}

#plot.field(POD_daily[,,1,250], lon, lat)
