# calculate and store, maybe plot RY estimations from AOT40, M7, POD-DOSE
library(maps); library(fields);
source('~/Desktop/analysis/code/get_geo.R')
#
load('~/Desktop/analysis/data/crop/crop_mask.RData')
setwd('~/Desktop/analysis/data/')
load('./ozone/aot40.RData')
# RY equations for aot40, Tai_2014_NCC_supp
# 1, Maize, RY = 1 - 0.00356AOT40
ry_Maize_AOT40 = 1 - 0.00356*AOT40_Maize
#plot.field(ry_Maize_AOT40[6:144,17:84], lon[6:144], lat[17:84], type = 'def', zlim = c(0.9,1))
# 2, Wheat, RY = 1 - 0.0163AOT40
ry_Wheat_AOT40 = 1 - 0.0163*AOT40_Wheat
# 3, Soybean, RY = 1 - 0.0113AOT40
ry_Soybean_AOT40 = 1 - 0.0113*AOT40_Soybean
# 4, Rice, RY = 1 - 0.00415AOT40
ry_Rice_AOT40 = 1 - 0.00415*AOT40_Rice

setwd('~/Desktop/analysis/figures/RY/aot40')
pdf(paste0('ry_legend_aot40.pdf'), width = 14, height = 8.5)
plot.field(ry_Maize_AOT40, lon, lat, type = 'def', zlim = c(0.6,1), legend.only = T, col = rev(tim.colors(n=32)[17:32]))
dev.off()

for(i in 1:4) {
  tmp = get(paste0('ry_', crop.types[i], '_AOT40')) * get(paste0('mask_', crop.types[i]))
  
  pdf(paste0('ry_', crop.types[i], '_aot40.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type = 'def', zlim = c(0.6,1), image.only = T, col = rev(tim.colors(n=32)[17:32]))
  dev.off()
}

setwd('~/Desktop/analysis/data/crop/RY')
save(list = c('lon', 'lat', 'crop.types', 'ry_Maize_AOT40', 'ry_Wheat_AOT40', 'ry_Soybean_AOT40', 'ry_Rice_AOT40'), file = 'ry_AOT40.RData')
print('finished ploting AOT40')

# RY equations for M7, Tai_2014_NCC_supp
setwd('~/Desktop/analysis/data/')
load('./ozone/m7.RData')
load('./ozone/m12.RData')
# 1, Maize, RY = exp(-(M12/124)^2.83) / exp(-(20/124)^2.83)
ry_Maize_M12 = exp(-(M12_Maize/124)^2.83) / exp(-(20/124)^2.83)
# 2, Wheat, RY = exp(-(M7/186)^3.2) / exp(-(25/186)^3.2)
ry_Wheat_M7 = exp(-(M7_Wheat/186)^3.2) / exp(-(25/186)^3.2)
# 3, Soybean, RY = exp(-(M12/107)^1.58) / exp(-(20/107)^1.58)
ry_Soybean_M12 = exp(-(M12_Soybean/107)^1.58) / exp(-(20/107)^1.58)
# 4, Rice, RY = exp(-(M7/202)^2.47) / exp(-(25/202)^2.47)
ry_Rice_M7 = exp(-(M7_Rice/202)^2.47) / exp(-(25/202)^2.47)
group = c('ry_Maize_M12', 'ry_Wheat_M7', 'ry_Soybean_M12', 'ry_Rice_M7')

setwd('~/Desktop/analysis/figures/RY/m7')

pdf('m7_m12_legend.pdf', width = 14, height = 8.5)
plot.field(ry_Maize_M12[6:144,17:84], lon[6:144], lat[17:84], type = 'def', zlim = c(0.6,1), legend.only = T, col = rev(tim.colors(n=32)[17:32]))
dev.off()

for(i in 1:4){
  tmp = get(group[i]) * get(paste0('mask_', crop.types[i]))
  
  pdf(paste0(group[i], '.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type = 'def', zlim = c(0.6,1), image.only = T, col = rev(tim.colors(n=32)[17:32]))
  dev.off()
}

setwd('~/Desktop/analysis/data/crop/RY')
save(list = c('lon', 'lat', 'crop.types', 'ry_Maize_M12', 'ry_Wheat_M7', 'ry_Soybean_M12', 'ry_Rice_M7'), file = 'ry_m7-m12.RData')

print('finished ploting m7/m12')

# w126
setwd('~/Desktop/analysis/data/')
load('./ozone/W126.RData')
# 1, Maize,
ry_Maize_W126 = exp(-(W126_Maize/97.9)^2.966)
# 2, Wheat, 
ry_Wheat_W126 = exp(-(W126_Wheat/53.4)^2.367)
# 3, Soybean, 
ry_Soybean_W126 = exp(-(W126_Soybean/101.505)^1.452)
# 4, Rice, no equation available
ry_Rice_W126 = W126_Rice*0 + 1

setwd('~/Desktop/analysis/figures/RY/w126/')

pdf(paste0('ry_legend_w126.pdf'), width = 14, height = 8.5)
plot.field(ry_Maize_W126, lon, lat, type = 'def', zlim = c(0.85,1), legend.only = T, col = rev(tim.colors(n=32)[17:32]))
dev.off()

for(i in 1:4) {
  tmp = get(paste0('ry_', crop.types[i], '_W126')) * get(paste0('mask_', crop.types[i]))
  
  pdf(paste0('ry_', crop.types[i], '_w126.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type = 'def', zlim = c(0.85,1), image.only = T, col = rev(tim.colors(n=32)[17:32]))
  dev.off()
}
setwd('~/Desktop/analysis/data/crop/RY')
save(list = c('lon', 'lat', 'crop.types', 'ry_Maize_W126', 'ry_Wheat_W126', 'ry_Soybean_W126', 'ry_Rice_W126'), file = 'ry_W126.RData')

print('finished ploting w126')


# RY equations for POD3IAM, 
# method 1: Mills 2018
# RY = 0.9756 - 0.0064POD3IAM, only works for wheat

# method 2: Mills 2018 GCB
# wheat: yield loss (%) = (POD3 - 0.1) * 0.64
# for other crops: above result multiply by relative sensitivty (rs)
# relative sensitivity (rs) = slope_M7(soybean/maize/rice) / slope_M7(wheat) 
# wheat, RY = 0.96 - 0.0048M7, slope = 0.0048, rs = 0.0048/0.0048
# soybean, RY = 1.001 - 0.005M7, slope = 0.005, rs = 0.005/0.0048
# maize, RY = 1.03 - 0.0031M7, slope = 0.0031, rs = 0.0031/0.0048
# rice, RY = 0.987 - 0.0021M7, slope = 0.0021, rs = 0.0021/0.0048
setwd('~/Desktop/analysis/data/')
load('./ozone/POD3_DOSE.RData')

# 'Maize', 'Wheat', 'Soybean', 'Rice'
rs = c(0.0031/0.0048, 0.0048/0.0048, 0.005/0.0048, 0.0021/0.0048)
#for(i in 1:4) assign(x = paste0('ry_', crop.types[i], '_POD3_DOSE'), value = 0.9756 - 0.0064*get(paste0('POD3_DOSE_', crop.types[i])))
for(i in 1:4){
  assign(x = paste0('yl_', crop.types[i], '_POD3_DOSE'), value = (get(paste0('POD3_DOSE_', crop.types[i])) - 0.1)*0.64*rs[i])
  assign(x = paste0('ry_', crop.types[i], '_POD3_DOSE'), value = (100-get(paste0('yl_', crop.types[i], '_POD3_DOSE')))/100)
}

setwd('~/Desktop/analysis/figures/RY/dose/')

pdf(paste0('ry_legend_pod_dose.pdf'), width = 14, height = 8.5)
plot.field(ry_Maize_POD3_DOSE, lon, lat, type = 'def', zlim = c(0.85,1), legend.only = T, col = rev(tim.colors(n=32)[17:32]))
dev.off()

for(i in 1:4) {
  tmp = get(paste0('ry_', crop.types[i], '_POD3_DOSE')) * get(paste0('mask_', crop.types[i]))
  pdf(paste0('ry_', crop.types[i], '_pod_dose.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type = 'def', zlim = c(0.85,1), image.only = T, col = rev(tim.colors(n=32)[17:32]))
  dev.off()
}
setwd('~/Desktop/analysis/data/crop/RY')
save(list = c('lon', 'lat', 'crop.types', 'ry_Maize_POD3_DOSE', 'ry_Wheat_POD3_DOSE', 'ry_Soybean_POD3_DOSE', 'ry_Rice_POD3_DOSE'), file = 'ry_POD3_DOSE.RData')

print('finished ploting pod-dose')

# fbb
setwd('~/Desktop/analysis/data/')
load('./ozone/POD3_FBB.RData')
# 1, Maize
for(i in 1:4){
  assign(x = paste0('yl_', crop.types[i], '_POD3_FBB'), value = (get(paste0('POD3_FBB_', crop.types[i])) - 0.1)*0.64*rs[i])
  assign(x = paste0('ry_', crop.types[i], '_POD3_FBB'), value = (100-get(paste0('yl_', crop.types[i], '_POD3_FBB')))/100)
}

setwd('~/Desktop/analysis/figures/RY/fbb/')

pdf(paste0('ry_legend_pod_fbb.pdf'), width = 14, height = 8.5)
plot.field(ry_Maize_POD3_FBB, lon, lat, type = 'def', zlim = c(0.85,1), legend.only = T, col = rev(tim.colors(n=32)[17:32]))
dev.off()

for(i in 1:4) {
  tmp = get(paste0('ry_', crop.types[i], '_POD3_FBB')) * get(paste0('mask_', crop.types[i]))
  
  pdf(paste0('ry_', crop.types[i], '_pod_fbb.pdf'), width = 14, height = 8.5)
  plot.field(tmp[6:144,17:84], lon[6:144], lat[17:84], type = 'def', zlim = c(0.85,1), image.only = T, col = rev(tim.colors(n=32)[17:32]))
  dev.off()
}
setwd('~/Desktop/analysis/data/crop/RY')
save(list = c('lon', 'lat', 'crop.types', 'ry_Maize_POD3_FBB', 'ry_Wheat_POD3_FBB', 'ry_Soybean_POD3_FBB', 'ry_Rice_POD3_FBB'), file = 'ry_POD3_FBB.RData')

print('finished ploting pod-fbb')
