# RY global averages
library(maps); library(fields); library(ggplot2)
source('~/Desktop/analysis/code/get_geo.R')
lat = seq(-90,90,2)
lon = seq(-180,177.5,2.5)
load('~/Desktop/analysis/data/crop/crop_mask.RData')

setwd('~/Desktop/analysis/data/crop/RY/')
for(method in c('POD3_FBB', 'POD3_DOSE', 'AOT40', 'W126')){
  load(paste0('ry_', method, '.RData'))
  yl.avg = rep(NaN, 4)
  yl.sd = rep(NaN, 4)

  for(icrop in 1:length(crop.types)){
    yl.avg[icrop] = mean(100 *(1 - get(paste0('ry_', crop.types[icrop], '_', method))*get(paste0('mask_', crop.types[icrop]))), na.rm = T)
    yl.sd[icrop] = sd(100 *(1 - get(paste0('ry_', crop.types[icrop], '_', method))*get(paste0('mask_', crop.types[icrop]))), na.rm = T)
    }
  assign(x = paste0('df_',method), value = data.frame(crop.types, method = method, yield.loss.avg = yl.avg, yield.loss.sd = yl.sd))
}

# M7/M12
load('ry_m7-m12.RData')
group = c('ry_Maize_M12', 'ry_Wheat_M7', 'ry_Soybean_M12', 'ry_Rice_M7')
yl.avg = rep(NaN, 4)
yl.sd = rep(NaN, 4)

for(icrop in 1:length(crop.types)){
    yl.avg[icrop] = mean(100*(1-get(group[icrop])*get(paste0('mask_', crop.types[icrop]))), na.rm = T)
    yl.sd[icrop] = sd(100*(1-get(group[icrop])*get(paste0('mask_', crop.types[icrop]))), na.rm = T)
  }
assign(x = 'df_m', value = data.frame(crop.types, method = 'M7/M12', yield.loss.avg = yl.avg, yield.loss.sd = yl.sd))

# combine all dfs
df_all = rbind(df_AOT40, df_m, df_W126, df_POD3_DOSE, df_POD3_FBB)

p = ggplot(df_all, aes(y = yield.loss.avg, x = crop.types)) +
  geom_bar(stat='identity', fill = 'steelblue') +
  geom_text(aes(label=format(yield.loss.avg, digits = 1)), vjust=1.6, color="white", size=3.5) +
  facet_wrap(~method) +
  labs(x = ' ', y = 'Yield loss (%)', title = "Global average")
print(p)

p = ggplot(df_all, aes(y = yield.loss.avg, x = method)) +
  geom_bar(stat='identity', fill = 'steelblue') +
  geom_text(aes(label=format(yield.loss.avg, digits = 1)), vjust=1.6, color="white", size=3.5) +
  facet_wrap(~crop.types) +
  labs(x = ' ', y = 'Yield loss (%)', title = "Global average")
print(p)

mills = data.frame(crop.types = crop.types, method = 'Mills et al. 2018', yield.loss.avg = c(6.1, 7.1, 12.4, 4.4), yield.loss.sd = rep(0,4))
df_all = rbind(df_all, mills)

p = ggplot(df_all, aes(x = crop.types, y = yield.loss.avg, group = method)) + 
  geom_point(aes(shape = method), size = 3.5, position = position_dodge(0.5)) +
  scale_shape_manual(values = c(4,7,10,16,17,22)) +
  theme(legend.title = element_blank()) +
  labs(x = ' ', y = 'Yield loss (%)', title = "Global average") +
  geom_segment(aes(x = 'Maize', y = 2.2, xend = 'Maize', yend = 5.5, linetype = 'Ainsworth et al. 2017'), color = "blue") +
  geom_segment(aes(x = 'Rice', y = 3, xend = 'Rice', yend = 4), color = "blue") +
  geom_segment(aes(x = 'Soybean', y = 5.4, xend = 'Soybean', yend = 15.6), color = "blue") +
  geom_segment(aes(x = 'Wheat', y = 3.9, xend = 'Wheat', yend = 15.4), color = "blue")
print(p)


#save(list = c('df_all', 'crop.types', 'mask_EA', 'mask_EU', 'mask_SA', 'mask_US'), file = 'data.frame.yield.loss.RData')
