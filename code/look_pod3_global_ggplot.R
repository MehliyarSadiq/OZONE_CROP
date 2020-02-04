# draw global contour plots of POD3 in ggplot2
library(ncdf4);library(maps); library(fields);library(ggplot2); library(gridExtra);
library(reshape2);library(RColorBrewer);library(gridExtra);library(cowplot)

lat = seq(-90,90,2)
lon = seq(-180,177.5,2.5)
setwd("~/Desktop/0828 debug/analysis/data/")

load('fbb_2008_processed.RData')

#crop.types = c('Maize','Wheat', 'Winter wheat', 'Soybean', 'Rice', 'Rice.2', 'Maize.2')
crop.types = c('Maize','Wheat','Soybean','Rice') # 1, 2, 4, 5,

count = 1
myplots = list()

for (i in 1:length(crop.types)){
    print(count)
    subset_index = c(1,2,4,5)
    subset = POD_daily[,,subset_index,]
    pod = apply(subset[,,i,], c(1,2), FUN = max, na.rm = TRUE)
    pod[which(!is.finite(pod))] = NaN
    pod[which(pod > 20)] = 20
    #lat.subset = lat[61:71]
    #pod = pod[,61:71]
    # 1, melt data into dataframe
    rownames(pod) = as.character(lon)
    #colnames(pod) = as.character(lat.subset)
    colnames(pod) = as.character(lat)
    output = melt(data = pod, value.name = "POD", varnames = c('lon', 'lat'))
    # 2, geom_raster
    myplots[[count]] = ggplot() +
      geom_raster(data = subset(output, !is.na(POD)&POD>=1), aes(x = lon, y = lat, fill = POD)) +
      scale_fill_gradientn(colors = rev(brewer.pal(n=10, name = "Spectral")), limits=c(0,20)) +
      theme(aspect.ratio = 0.6, 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            line = element_blank(),
            panel.background = element_blank()) +
      labs(title = paste(crop.types[i]), x = " ", y = " ", fill = "mmol/m2") +
      borders('world', colour = "black") #ylim = c(20,50))
    #print(myplots[[count]])
    count = count + 1
}
print(myplots)
#tt = plot_grid(plotlist = myplots, nrow = 4, ncol = 2) # gridding them shrinks the global maps, doesn't look good
#title = ggdraw() + draw_label("POD", fontface = 'bold')
#plot_grid(title, tt, rel_heights = c(0.1,1))
#print(tt)
