# energy loss
library(fields); library(ggplot2)
source('~/Desktop/analysis/code/get_geo.R')

load('~/Desktop/analysis/data/crop/crop_prod.RData')
lat = seq(-90,90,2)
lon = seq(-180,177.5,2.5)

expo.metrics = c('AOT40', 'POD3_DOSE', 'POD3_FBB', 'W126')
unit_conver = c(3.65, 3.34, 4.46, 3.63) # 10^6 kcal ton-1

# total food production energy
for(i in 1:length(crop.types)){
  prod = get(paste0('prod_', crop.types[i])) # map: ton per grid cell
  prod.total = sum(prod, na.rm = T)         # number: total yield (tons)
  print(paste(crop.types[i], 'total food prodcution energy (1e12 kcal)',prod.total*unit_conver[i]*1e-6))
}

for(j in 1:length(expo.metrics))
{
load(paste0('~/Desktop/analysis/data/crop/ry/ry_', expo.metrics[j], '.RData'))
# prod_crop_name, ton per grid cell
# ry_crop_name
#unit_conver = c(3.65, 3.34, 4.46, 3.63) # 10^6 kcal ton-1
prod.loss.per = rep(0, 4)
ener.loss = rep(0, 4)

for(i in 1:length(crop.types)){
  prod = get(paste0('prod_', crop.types[i])) # map: ton per grid cell
  prod.total = sum(prod, na.rm = T)         # number: total yield (tons)

  prod_untact = prod / get(paste0('ry_', crop.types[i], '_', expo.metrics[j])) # map: theoretical production, unaffected by ozone
  prod.theo = sum(prod_untact, na.rm = T) # number: theoretical total production
  prod_loss = prod - prod_untact       # map: production loss (tons)
  prod.loss = sum(prod_loss, na.rm = T) # number: total loss (tons)
  plot.field(prod_loss, lon , lat, col = rev(tim.colors(n=32)[17:32]), type = 'def', zlim = c(-40000,0))
  
  print(paste(expo.metrics[j], crop.types[i], 'prod loss (%): ', 100*prod.loss/prod.theo))
  
  prod.loss.per[i] = -100*prod.loss/prod.theo # number: prod loss (%)
  ener.loss[i] = prod.loss * unit_conver[i]/(1e6) # energy loss(10e12kcal)
}
assign(x = paste0('df_',expo.metrics[j]), value = data.frame(crop.types, method = expo.metrics[j], pl = prod.loss.per, el = ener.loss))
}

# m7 and m12
load('~/Desktop/analysis/data/crop/ry/ry_m7-m12.RData')
pl_maize = prod_Maize / ry_Maize_M12 - prod_Maize # production loss map: ton per grid cell
pl.maize = sum(pl_maize, na.rm = T)/sum(prod_Maize / ry_Maize_M12, na.rm = T) # number: ton
print(paste('M12 Maize yield loss (%) ', -100*pl.maize))
el.maize = pl.maize * unit_conver[1] * 1e-6 # energy loss(10e12kcal)

pl_wheat = prod_Wheat / ry_Wheat_M7 - prod_Wheat
pl.wheat = sum(pl_wheat, na.rm = T)/sum(prod_Wheat / ry_Wheat_M7, na.rm = T)
print(paste('M7 Wheat yield loss (%) ', -100*pl.wheat))
el.wheat = pl.wheat * unit_conver[2] * 1e-6 # energy loss(10e12kcal)

pl_soybean = prod_Soybean / ry_Soybean_M12 - prod_Soybean
pl.soybean = sum(pl_soybean, na.rm = T)/sum(prod_Soybean / ry_Soybean_M12, na.rm = T)
print(paste('M12 Soybean yield loss (%) ', -100*pl.soybean))
el.soybean = pl.soybean * unit_conver[3] * 1e-6 # energy loss(10e12kcal)

pl_rice = prod_Rice / ry_Rice_M7 - prod_Rice
pl.rice = sum(pl_rice, na.rm = T)/sum(prod_Rice / ry_Rice_M7, na.rm = T)
print(paste('M7 Rice yield loss (%) ', -100*pl.rice))
el.rice = pl.rice * unit_conver[4] * 1e-6 # energy loss(10e12kcal)

ener.loss = c(el.maize, el.wheat, el.soybean, el.rice)
print(ener.loss)
ener_sum = sum(el.maize, el.wheat, el.soybean, el.rice)

prod.loss.per = 100*c(pl.maize, pl.wheat, pl.soybean, pl.rice)
assign(x = 'df_m', value = data.frame(crop.types, method = 'M7/M12', pl = prod.loss.per, el = ener.loss))

# combine all dfs
df_all = rbind(df_AOT40, df_m, df_W126, df_POD3_DOSE, df_POD3_FBB)

p = ggplot(df_all, aes(y = pl, x = crop.types)) +
  geom_bar(stat='identity', fill = 'steelblue') +
  geom_text(aes(label=format(pl, digits = 1)), vjust=1.6, color="white", size=3.5) +
  facet_wrap(~method) +
  labs(x = ' ', y = 'Production loss (%)') #, title = "Global average"
print(p)

p = ggplot(df_all, aes(x = crop.types, y = -el)) + 
  geom_bar(stat='identity', fill = 'steelblue') +
  geom_text(aes(label=format(pl, digits = 1)), vjust=1.6, color="white", size=3.5) +
  facet_wrap(~method) +
  labs(x = ' ', y = 'Energy loss (^12 kcal)') #, title = "Global average"
  print(p)

for(i in 1:length(crop.types)){
  L = df_all$crop.types == crop.types[i]
  #print(df_all[L,]$pl)
  mean_5_formatted = format(mean(df_all[L,]$pl), digits = 3)
  print(paste(crop.types[i], 'mean (5 metrics)', mean_5_formatted))
  sd_5_formatted = format(sd(df_all[L,]$pl), digits = 3)
  print(paste(crop.types[i], 'sd (5 metrics)', sd_5_formatted))
  
  mean_4_formatted = format(mean(df_all[L,]$pl[c(1,2,4,5)]), digits = 3)
  print(paste(crop.types[i], 'mean (4 metrics)', mean_4_formatted))
  sd_4_formatted = format(sd(df_all[L,]$pl[c(1,2,4,5)]), digits = 3)
  print(paste(crop.types[i], 'sd (4 metrics)', sd_4_formatted))
}


# mills = data.frame(crop.types = crop.types, method = 'Mills et al. 2018', pl = c(6.1, 7.1, 12.4, 4.4), el = rep(0,4))
# df_all = rbind(df_all, mills)
# 
# p = ggplot(df_all, aes(x = crop.types, y = pl, group = method)) + 
#   geom_point(aes(shape = method), size = 3.5, position = position_dodge(0.4)) +
#   scale_shape_manual(values = c(4,7,10,16,17,22)) +
#   theme(legend.title = element_blank()) +
#   labs(x = ' ', y = 'Production loss (%)') + #, title = "Global average"
#   geom_segment(aes(x = 'Maize', y = 2.2, xend = 'Maize', yend = 5.5, linetype = 'Ainsworth et al. 2017'), color = "blue") +
#   geom_segment(aes(x = 'Rice', y = 3, xend = 'Rice', yend = 4), color = "blue") +
#   geom_segment(aes(x = 'Soybean', y = 5.4, xend = 'Soybean', yend = 15.6), color = "blue") +
#   geom_segment(aes(x = 'Wheat', y = 3.9, xend = 'Wheat', yend = 15.4), color = "blue")
# print(p)
