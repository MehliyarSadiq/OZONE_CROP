# energy loss
library(fields)
source('~/Desktop/analysis/code/get_geo.R')

load('~/Desktop/analysis/data/crop/crop_prod.RData')
lat = seq(-90,90,2)
lon = seq(-180,177.5,2.5)

expo.metrics = c('AOT40', 'POD3_DOSE', 'POD3_FBB', 'W126')

for(j in 1:length(expo.metrics))
{
load(paste0('~/Desktop/analysis/data/crop/ry/ry_', expo.metrics[j], '.RData'))
# prod_crop_name, ton per grid cell
# ry_crop_name
unit_conver = c(3.65, 3.34, 4.46, 3.63) # 10^6 kcal ton-1
yield_loss = rep(0, 4)
ener_loss = rep(0, 4)

for(i in 1:length(crop.types)){
  yield = get(paste0('prod_', crop.types[i])) # ton per grid cell
  yield_total = sum(yield, na.rm = T)         # total yield (tons)
  yield.untact = yield / get(paste0('ry_', crop.types[i], '_', expo.metrics[j])) # theoretical yield, unaffected by ozone
  yield.loss = yield - yield.untact       # yield loss (tons)
  loss_total = sum(yield.loss, na.rm = T) # total loss (tons)
  plot.field(yield.loss, lon , lat, col = rev(tim.colors(n=32)[17:32]), type = 'def', zlim = c(-40000,0))
  
  print(paste(expo.metrics[j], crop.types[i], 'yield loss (%): ', 100*loss_total/yield_total))
  yield_loss[i] = 100*loss_total/yield_total # yield loss (%)
  
  energy = yield.loss * unit_conver[i]
  ener_loss_sum = sum(energy, na.rm = T)
  ener_loss[i] = ener_loss_sum/(1e6) # energy loss(10e12kcal)
}
#print(paste(expo.metrics[j], ' theoretical total (x10e12 kcal):', ener_total/1e6))
#print(paste(expo.metrics[j], ' loss (x10e12 kcal):', ener_loss_sum/1e6))
#print(paste(expo.metrics[j], ' loss (%):', 100*ener_loss_sum/ener_total))
assign(x = paste0('df_',expo.metrics[j]), value = data.frame(crop.types, method = expo.metrics[j], yield.loss.avg = -1*yield_loss, yield.loss.sd = -1*yield_loss))
}

# m7 and m12
load('~/Desktop/analysis/data/crop/ry/ry_m7-m12.RData')
yl.maize = prod_Maize / ry_Maize_M12 - prod_Maize
yl.total.maize = sum(yl.maize, na.rm = T)/sum(prod_Maize / ry_Maize_M12, na.rm = T)
print(paste('M12 Maize yield loss (%) ', -100*yl.total.maize))
el.maize = -1 * (prod_Maize / ry_Maize_M12 - prod_Maize) * unit_conver[1]

yl.wheat = prod_Wheat / ry_Wheat_M7 - prod_Wheat
yl.total.wheat = sum(yl.wheat, na.rm = T)/sum(prod_Wheat / ry_Wheat_M7, na.rm = T)
print(paste('M7 Wheat yield loss (%) ', -100*yl.total.wheat))
el.wheat = -1 * (prod_Wheat / ry_Wheat_M7 - prod_Wheat)* unit_conver[2]

yl.soybean = prod_Soybean / ry_Soybean_M12 - prod_Soybean
yl.total.soybean = sum(yl.soybean, na.rm = T)/sum(prod_Soybean / ry_Soybean_M12, na.rm = T)
print(paste('M12 Soybean yield loss (%) ', -100*yl.total.soybean))
el.soybean = -1 * (prod_Soybean / ry_Soybean_M12 - prod_Soybean)* unit_conver[3]

yl.rice = prod_Rice / ry_Rice_M7 - prod_Rice
yl.total.rice = sum(yl.rice, na.rm = T)/sum(prod_Rice / ry_Rice_M7, na.rm = T)
print(paste('M7 Rice yield loss (%) ', -100*yl.total.rice))
el.rice = -1* (prod_Rice / ry_Rice_M7 - prod_Rice)* unit_conver[4]

ener_sum = sum(sum(el.maize, na.rm = T), sum(el.wheat, na.rm = T), sum(el.soybean, na.rm = T), sum(el.rice, na.rm = T))

yield_loss = 100*c(yl.total.maize, yl.total.wheat, yl.total.soybean, yl.total.rice)
assign(x = 'df_m', value = data.frame(crop.types, method = 'M7/M12', yield.loss.avg = yield_loss, yield.loss.sd = yield_loss))
#print(paste('M7/M12 : ', ener_sum/(1e6)))


# combine all dfs
df_all = rbind(df_AOT40, df_m, df_W126, df_POD3_DOSE, df_POD3_FBB)

p = ggplot(df_all, aes(y = yield.loss.avg, x = crop.types)) +
  geom_bar(stat='identity', fill = 'steelblue') +
  geom_text(aes(label=format(yield.loss.avg, digits = 1)), vjust=1.6, color="white", size=3.5) +
  facet_wrap(~method) +
  labs(x = ' ', y = 'Yield loss (%)') #, title = "Global average"
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
  labs(x = ' ', y = 'Yield loss (%)') + #, title = "Global average"
  geom_segment(aes(x = 'Maize', y = 2.2, xend = 'Maize', yend = 5.5, linetype = 'Ainsworth et al. 2017'), color = "blue") +
  geom_segment(aes(x = 'Rice', y = 3, xend = 'Rice', yend = 4), color = "blue") +
  geom_segment(aes(x = 'Soybean', y = 5.4, xend = 'Soybean', yend = 15.6), color = "blue") +
  geom_segment(aes(x = 'Wheat', y = 3.9, xend = 'Wheat', yend = 15.4), color = "blue")
print(p)


#save(list = c('df_all', 'crop.types', 'mask_EA', 'mask_EU', 'mask_SA', 'mask_US'), file = 'data.frame.yield.loss.RData')

