# plot of sd/mean for fbb and dose

setwd("~/Desktop/0828 debug/analysis/data/dose/")
load(paste0("mean_sd_POD3_DOSE.RData"))
setwd("~/Desktop/0828 debug/analysis/figures/dose/")
for(i in 1:length(crop.types)){
  tmp = 100*get(paste0('mask_', crop.types[i]))*get(paste0('sd_POD3_DOSE_', crop.types[i]))/get(paste0('mean_POD3_DOSE_', crop.types[i]))
  
  pdf(paste0('sd_mean_', crop.types[i], '.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type='def', zlim=c(0,30))
  dev.off()
}


setwd("~/Desktop/0828 debug/analysis/data/fbb//")
load(paste0("mean_sd_POD3_FBB.RData"))
setwd("~/Desktop/0828 debug/analysis/figures/fbb//")
for(i in 1:length(crop.types)){
  tmp = 100*get(paste0('mask_', crop.types[i]))*get(paste0('sd_POD3_FBB_', crop.types[i]))/get(paste0('mean_POD3_FBB_', crop.types[i]))
  
  pdf(paste0('sd_mean_', crop.types[i], '.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type='def', zlim=c(0,30))
  dev.off()
}
  