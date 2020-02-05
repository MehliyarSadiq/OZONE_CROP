# plots of aot40, m7, pod-dose, pod-fbb
source('~/Desktop/0828 debug/analysis/code/get_geo.R')
load('~/Desktop/0828 debug/analysis/data/crop/crop_mask.RData')

# pod dose plots for 4 crops
setwd('~/Desktop/0828 debug/analysis/data/')
load('./dose/POD3_DOSE.RData')

setwd('../figures/dose')

pdf('pod_dose_legend.pdf', width = 8, height = 8)
plot.field(POD3_DOSE_Maize, lon, lat, type = 'def', zlim=c(0,25), legend.only = T, horizontal = T)
dev.off()

for(i in 1:length(crop.types)){
  tmp = get(paste0('POD3_DOSE_', crop.types[i])) * get(paste0('mask_', crop.types[i]))
  
  pdf(paste0('pod_dose_', crop.types[i], '.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type='def', zlim=c(0,25), image.only = T)
  dev.off()
}

# pod fbb plots for 4 crops
setwd('~/Desktop/0828 debug/analysis/data/')
load('./fbb/POD3_FBB.RData')
setwd('../figures/fbb')

pdf('pod_fbb_legend.pdf', width = 8, height = 8)
plot.field(POD3_FBB_Maize, lon, lat, type = 'def', zlim=c(0,25), legend.only = T, horizontal = T)
dev.off()

for(i in 1:length(crop.types)){
  tmp = get(paste0('POD3_FBB_', crop.types[i])) * get(paste0('mask_', crop.types[i]))
  
  pdf(paste0('pod_fbb_', crop.types[i], '.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type='def', zlim=c(0,25), image.only = T)
  dev.off()
}

# aot40
setwd('~/Desktop/0828 debug/analysis/data/')
load('./ozone/AOT40.RData')
setwd('../figures/aot40/')

pdf('aot40_legend.pdf', width = 8, height = 8)
plot.field(AOT40_Maize, lon, lat, type = 'def', zlim=c(0,30), legend.only = T, horizontal = T)
dev.off()

for(i in 1:length(crop.types)){
  tmp = get(paste0('AOT40_', crop.types[i])) * get(paste0('mask_', crop.types[i]))
  
  pdf(paste0('aot40_', crop.types[i], '.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type='def', zlim=c(0,30), image.only = T)
  dev.off()
}


# m7
setwd('~/Desktop/0828 debug/analysis/data/')
load('./ozone/m7.RData')
setwd('../figures/m7/')

pdf('m7_legend.pdf', width = 8, height = 8)
plot.field(M7_Maize, lon, lat, type = 'def', zlim=c(0,70), legend.only = T, horizontal = T)
dev.off()

for(i in 1:length(crop.types)){
  tmp = get(paste0('M7_', crop.types[i])) * get(paste0('mask_', crop.types[i]))
  
  pdf(paste0('m7_', crop.types[i], '.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type='def', zlim=c(0,70), image.only = T)
  dev.off()
}

# m12
setwd('~/Desktop/0828 debug/analysis/data/')
load('./ozone/m12.RData')
setwd('../figures/m12/')

pdf('m12_legend.pdf', width = 8, height = 8)
plot.field(M12_Maize, lon, lat, type = 'def', zlim=c(0,70), legend.only = T, horizontal = T)
dev.off()

for(i in 1:length(crop.types)){
  tmp = get(paste0('M12_', crop.types[i])) * get(paste0('mask_', crop.types[i]))
  
  pdf(paste0('m12_', crop.types[i], '.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type='def', zlim=c(0,70), image.only = T)
  dev.off()
}

# w126
setwd('~/Desktop/0828 debug/analysis/data/')
load('./ozone/W126.RData')
setwd('../figures/w126/')

pdf('w126_legend.pdf', width = 8, height = 8)
plot.field(W126_Maize, lon, lat, type = 'def', zlim=c(0,40), legend.only = T, horizontal = T)
dev.off()

for(i in 1:length(crop.types)){
  tmp = get(paste0('W126_', crop.types[i])) * get(paste0('mask_', crop.types[i]))
  
  pdf(paste0('w126_', crop.types[i], '.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type='def', zlim=c(0,40), image.only = T)
  dev.off()
}
